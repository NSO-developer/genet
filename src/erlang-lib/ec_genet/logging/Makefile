include ../vsn.mk

ERL_FILES  = $(wildcard *.erl)
HRL_FILES  = $(wildcard *.hrl) $(wildcard ../include/*.hrl)
MODULES    = $(ERL_FILES:%.erl=%)
EBIN_FILES = $(MODULES:%=../ebin/%.beam)

ERLC       = erlc
ERLC_FLAGS := \
	+debug_info \
	-I ../include \
	-pa $(NCS_DIR)/erlang/econfd/ebin
#	+warnings_as_errors

all: $(NCS_DIR)/erlang/econfd/ebin $(EBIN_FILES)

$(NCS_DIR)/erlang/econfd/ebin:
	@echo 'Where is NCS installed? Set $$NCS_DIR to point it out!'
	exit 1

clean:
	rm -f $(EBIN_FILES) erl_crash.dump core* *~

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<

$(EBIN_FILES): $(HRL_FILES)
