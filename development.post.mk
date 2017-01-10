# --------------------------------------------------------------------
# Framing sources generation.
# --------------------------------------------------------------------

PYTHON       ?= python
CODEGEN_0_9_1 = $(CURDIR)/codegen-amqp-0.9.1.py
CODEGEN_1_0   = $(CURDIR)/codegen-amqp-1.0.py
CODEGEN_DIR  ?= $(DEPS_DIR)/rabbitmq_codegen
CODEGEN_AMQP  = $(CODEGEN_DIR)/amqp_codegen.py

AMQP_SPEC_0_8   = $(CODEGEN_DIR)/amqp-rabbitmq-0.8.json
AMQP_SPEC_0_9_1 = $(CODEGEN_DIR)/amqp-rabbitmq-0.9.1.json		\
		  $(CODEGEN_DIR)/credit_extension.json
AMQP_SPEC_1_0   = $(CODEGEN_DIR)/amqp-1.0/messaging.xml			\
		  $(CODEGEN_DIR)/amqp-1.0/security.xml			\
		  $(CODEGEN_DIR)/amqp-1.0/transport.xml			\
		  $(CODEGEN_DIR)/amqp-1.0/transactions.xml

# AMQP 0-9-1.

include/rabbit_framing.hrl:: $(CODEGEN_0_9_1) $(CODEGEN_AMQP) \
    $(AMQP_SPEC_0_9_1) $(AMQP_SPEC_0_8)
	$(gen_verbose) env PYTHONPATH=$(CODEGEN_DIR) \
	 $(PYTHON) $(CODEGEN_0_9_1) --ignore-conflicts header \
	 $(AMQP_SPEC_0_9_1) $(AMQP_SPEC_0_8) $@

src/rabbit_framing_amqp_0_9_1.erl:: $(CODEGEN_0_9_1) $(CODEGEN_AMQP) \
    $(AMQP_SPEC_0_9_1)
	$(gen_verbose) env PYTHONPATH=$(CODEGEN_DIR) \
	 $(PYTHON) $(CODEGEN_0_9_1) body $(AMQP_SPEC_0_9_1) $@

src/rabbit_framing_amqp_0_8.erl:: $(CODEGEN_0_9_1) $(CODEGEN_AMQP) \
    $(AMQP_SPEC_0_8)
	$(gen_verbose) env PYTHONPATH=$(CODEGEN_DIR) \
	 $(PYTHON) $(CODEGEN_0_9_1) body $(AMQP_SPEC_0_8) $@

# AMQP 1.0.

include/rabbit_framing_amqp_1_0.hrl:: $(CODEGEN_1_0) $(CODEGEN_AMQP) \
    $(AMQP_SPEC_1_0)
	$(gen_verbose) env PYTHONPATH=$(CODEGEN_DIR) \
	  $(PYTHON) $(CODEGEN_1_0) hrl $(AMQP_SPEC_1_0) > $@

src/rabbit_framing_amqp_1_0.erl:: $(CODEGEN_1_0) $(CODEGEN_AMQP) \
    $(AMQP_SPEC_1_0)
	$(gen_verbose) env PYTHONPATH=$(CODEGEN_DIR) \
	  $(PYTHON) $(CODEGEN_1_0) erl $(AMQP_SPEC_1_0) > $@

clean:: clean-extra-sources

clean-extra-sources:
	$(gen_verbose) rm -f $(EXTRA_SOURCES)
