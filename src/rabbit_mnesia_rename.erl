%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2016 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_mnesia_rename).
-include("rabbit.hrl").
-compile(export_all).
-export([rename/2]).

-define(CONVERT_TABLES, [schema, rabbit_durable_queue]).

%% Supports renaming the nodes in the Mnesia database. In order to do
%% this, we take a backup of the database, traverse the backup
%% changing node names and pids as we go, then restore it.
%%
%% That's enough for a standalone node, for clusters the story is more
%% complex. We can take pairs of nodes From and To, but backing up and
%% restoring the database changes schema cookies, so if we just do
%% this on all nodes the cluster will refuse to re-form with
%% "Incompatible schema cookies.". Therefore we do something similar
%% to what we do for upgrades - the first node in the cluster to
%% restart becomes the authority, and other nodes wipe their own
%% Mnesia state and rejoin. They also need to tell Mnesia the old node
%% is not coming back.
%%
%% If we are renaming nodes one at a time then the running cluster
%% might not be aware that a rename has taken place, so after we wipe
%% and rejoin we then update any tables (in practice just
%% rabbit_durable_queue) which should be aware that we have changed.

%%----------------------------------------------------------------------------

-spec rename(node(), [{node(), node()}]) -> 'ok'.

%%----------------------------------------------------------------------------

rename(Node, NodeMapList) ->
    try
        io:format("Mnesia is running ~p~n", [mnesia:system_info(is_running)]),
        %% Check everything is correct and figure out what we are
        %% changing from and to.
        {FromNode, ToNode, NodeMap} = prepare(Node, NodeMapList),

        %% We backup and restore Mnesia even if other nodes are
        %% running at the time, and defer the final decision about
        %% whether to use our mutated copy or rejoin the cluster until
        %% we restart. That means we might be mutating our copy of the
        %% database while the cluster is running. *Do not* contact the
        %% cluster while this is happening, we are likely to get
        %% confused.
        application:set_env(kernel, dist_auto_connect, never),

        %% Take a copy we can restore from if we abandon the
        %% rename. We don't restore from the "backup" since restoring
        %% that changes schema cookies and might stop us rejoining the
        %% cluster.
        ok = rabbit_mnesia:copy_db(mnesia_copy_dir()),

        %% And make the actual changes
        rabbit_control_main:become(FromNode),
        take_backup(before_backup_name()),
        convert_backup(NodeMap, before_backup_name(), after_backup_name()),
        ok = rabbit_file:write_term_file(rename_config_name(),
                                         [{FromNode, ToNode}]),
        convert_config_files(NodeMap),
        rabbit_control_main:become(ToNode),
        restore_backup(after_backup_name()),
        ok
    after
        stop_mnesia()
    end.

prepare(Node, NodeMapList) ->
    %% If we have a previous rename and haven't started since, give up.
    case rabbit_file:is_dir(dir()) of
        true  -> exit({rename_in_progress,
                       "Restart node under old name to roll back"});
        false -> ok = rabbit_file:ensure_dir(mnesia_copy_dir())
    end,

    %% Check we don't have two nodes mapped to the same node
    {FromNodes, ToNodes} = lists:unzip(NodeMapList),
    case length(FromNodes) - length(lists:usort(ToNodes)) of
        0 -> ok;
        _ -> exit({duplicate_node, ToNodes})
    end,

    %% Figure out which node we are before and after the change
    FromNode = case [From || {From, To} <- NodeMapList,
                             To =:= Node] of
                   [N] -> N;
                   []  -> Node
               end,
    NodeMap = dict:from_list(NodeMapList),
    ToNode = case dict:find(FromNode, NodeMap) of
                 {ok, N2} -> N2;
                 error    -> FromNode
             end,

    %% Check that we are in the cluster, all old nodes are in the
    %% cluster, and no new nodes are.
    Nodes = rabbit_mnesia:cluster_nodes(all),
    case {FromNodes -- Nodes, ToNodes -- (ToNodes -- Nodes),
          lists:member(Node, Nodes ++ ToNodes)} of
        {[], [], true}  -> ok;
        {[], [], false} -> exit({i_am_not_involved,        Node});
        {F,  [], _}     -> exit({nodes_not_in_cluster,     F});
        {_,  T,  _}     -> exit({nodes_already_in_cluster, T})
    end,
    {FromNode, ToNode, NodeMap}.

take_backup(Backup) ->
    start_mnesia(),
    ok = mnesia:backup(Backup),
    stop_mnesia().

restore_backup(Backup) ->
    ok = mnesia:install_fallback(Backup, [{scope, local}]),
    start_mnesia(),
    stop_mnesia(),
    rabbit_mnesia:force_load_next_boot().

dir()                -> rabbit_mnesia:dir() ++ "-rename".
before_backup_name() -> dir() ++ "/backup-before".
after_backup_name()  -> dir() ++ "/backup-after".
rename_config_name() -> dir() ++ "/pending.config".
mnesia_copy_dir()    -> dir() ++ "/mnesia-copy".

start_mnesia() -> rabbit_misc:ensure_ok(mnesia:start(), cannot_start_mnesia),
                  rabbit_table:force_load(),
                  rabbit_table:wait_for_replicated().
stop_mnesia()  -> stopped = mnesia:stop().

convert_backup(NodeMap, FromBackup, ToBackup) ->
    mnesia:traverse_backup(
      FromBackup, ToBackup,
      fun
          (Row, Acc) ->
              case lists:member(element(1, Row), ?CONVERT_TABLES) of
                  true  -> {[update_term(NodeMap, Row)], Acc};
                  false -> {[Row], Acc}
              end
      end, switched).

config_files() ->
    [rabbit_node_monitor:running_nodes_filename(),
     rabbit_node_monitor:cluster_status_filename()].

backup_of_conf(Path) ->
    filename:join([dir(), filename:basename(Path)]).

convert_config_files(NodeMap) ->
    [convert_config_file(NodeMap, Path) || Path <- config_files()].

convert_config_file(NodeMap, Path) ->
    {ok, Term} = rabbit_file:read_term_file(Path),
    {ok, _} = file:copy(Path, backup_of_conf(Path)),
    ok = rabbit_file:write_term_file(Path, update_term(NodeMap, Term)).

lookup_node(OldNode, NodeMap) ->
    case dict:find(OldNode, NodeMap) of
        {ok, NewNode} -> NewNode;
        error         -> OldNode
    end.

update_term(NodeMap, L) when is_list(L) ->
    [update_term(NodeMap, I) || I <- L];
update_term(NodeMap, T) when is_tuple(T) ->
    list_to_tuple(update_term(NodeMap, tuple_to_list(T)));
update_term(NodeMap, Node) when is_atom(Node) ->
    lookup_node(Node, NodeMap);
update_term(NodeMap, Pid) when is_pid(Pid) ->
    rabbit_misc:pid_change_node(Pid, lookup_node(node(Pid), NodeMap));
update_term(_NodeMap, Term) ->
    Term.
