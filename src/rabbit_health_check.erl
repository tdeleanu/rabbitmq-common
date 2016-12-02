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
-module(rabbit_health_check).

%% External API
-export([node/1, node/2, health_checks/0]).

%% Internal API
-export([node_health_check/3, health_check_local/1]).

-spec node(node(), timeout()) -> ok | {badrpc, term()} | {error_string, string()}.
-spec node_health_check(node(), integer(), atom()) -> ok | {badrpc, term()} | {error_string, string()}.
-spec health_check_local(atom()) -> ok | {error_string, string()}.

%%----------------------------------------------------------------------------
%% External functions
%%----------------------------------------------------------------------------

node(Node) ->
    %% Timeout for each check operation
    node(Node, 20000).
node(Node, Timeout) ->
    run_checks(Node, Timeout, health_checks()).

health_checks() ->
    [list_channels, list_queues, alarms, rabbit_node_monitor].

node_health_check(Node, Timeout, Check) ->
    rabbit_misc:rpc_call(Node,
                         rabbit_health_check, health_check_local, [Check],
                         Timeout).

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------
run_checks(Node, Timeout, []) ->
    ok;
run_checks(Node, Timeout, [C|Cs]) ->
    case node_health_check(Node, Timeout, C) of
        ok    -> run_checks(Node, Timeout, Cs);
        Error -> {C, Error}
    end.

health_check_local(list_channels) ->
    case rabbit_channel:info_local([pid]) of
        L when is_list(L) ->
            ok;
        Other ->
            ErrorMsg = io_lib:format("list_channels unexpected output: ~p",
                                     [Other]),
            {error_string, ErrorMsg}
    end;

health_check_local(list_queues) ->
    health_check_queues(rabbit_vhost:list());

health_check_local(rabbit_node_monitor) ->
    case rabbit_node_monitor:partitions() of
        L when is_list(L) ->
            ok;
        Other ->
            ErrorMsg = io_lib:format("rabbit_node_monitor reports unexpected partitions value: ~p",
                                     [Other]),
            {error_string, ErrorMsg}
    end;

health_check_local(alarms) ->
    case proplists:get_value(alarms, rabbit:status()) of
        [] ->
            ok;
        Alarms ->
            ErrorMsg = io_lib:format("resource alarm(s) in effect:~p", [Alarms]),
            {error_string, ErrorMsg}
    end.

health_check_queues([]) ->
    ok;
health_check_queues([VHost|RestVHosts]) ->
    case rabbit_amqqueue:info_local(VHost) of
        L when is_list(L) ->
            health_check_queues(RestVHosts);
        Other ->
            ErrorMsg = io_lib:format("list_queues unexpected output for vhost ~s: ~p",
                                     [VHost, Other]),
            {error_string, ErrorMsg}
    end.
