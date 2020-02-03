include ../../../../include.mk
include ../../vsn.mk

ERL_FILES  = $(wildcard *.erl)
HRL_FILES  = $(wildcard *.hrl) $(wildcard ../../include/*.hrl)
MODULES    = $(ERL_FILES:%.erl=%)
EBIN_FILES = $(MODULES:%=../../ebin/%.beam)

ERLC       = erlc
ERLC_FLAGS := \
	+debug_info \
	-I ../../include \
	+warnings_as_errors \
	-pa $(SYSTEM_DIR)/erlang/econfd/ebin

all: $(SYSTEM_DIR)/erlang/econfd/ebin $(EBIN_FILES)

clean:
	rm -f $(EBIN_FILES) erl_crash.dump core* *~

../../ebin/%.beam: %.erl
	@mkdir -p ../../ebin
	$(ERLC) $(ERLC_FLAGS) -o ../../ebin $<

$(EBIN_FILES): $(HRL_FILES)
