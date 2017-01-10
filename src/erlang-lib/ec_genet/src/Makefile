include ../vsn.mk

APP_DIR    = $(shell dirname $(CURDIR))
APP_NAME   = $(shell basename $(APP_DIR))
ERL_FILES  = $(wildcard *.erl)
HRL_FILES  = $(wildcard *.hrl) $(wildcard ../include/*.hrl)
MODULES    = $(ERL_FILES:%.erl=%)
EBIN_FILES = $(MODULES:%=../ebin/%.beam) ../ebin/$(APP_NAME).app

ERLC       = erlc
ERLC_FLAGS := \
	+debug_info \
	-I $(APP_DIR)/include \
	-pa $(NCS_DIR)/erlang/econfd/ebin \
	-pa ../../ec_genet/ebin

# jlindbla: Turned logging off, which gave some warnings, so turned off warnings as errors
#	+warnings_as_errors \

APPSCRIPT = '$$vsn=shift; $$mods=""; while(@ARGV){ $$_=shift; s/^([A-Z].*)$$/\'\''$$1\'\''/; $$mods.=", " if $$mods; $$mods .= $$_; } while(<>) { s/%VSN%/$$vsn/; s/%MODULES%/$$mods/; print; }'

all: $(NCS_DIR)/erlang/econfd/ebin $(EBIN_FILES)

$(NCS_DIR)/erlang/econfd/ebin:
	@echo 'Where is NCS installed? Set $$NCS_DIR to point it out!'
	exit 1

clean:
	rm -f $(EBIN_FILES) erl_crash.dump core* *~

../ebin/%.app: %.app.src ../vsn.mk Makefile
	perl -e $(APPSCRIPT) "$(VSN)" $(MODULES) < $< > $@

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<

$(EBIN_FILES): $(HRL_FILES)
