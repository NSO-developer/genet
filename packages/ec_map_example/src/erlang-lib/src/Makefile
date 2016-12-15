include $(NCS_DIR)/src/ncs/build/include.ncs.mk 
include ../vsn.mk

APP_NAME   = ec_map_example
APP_DIR    = $(shell dirname $(PWD))
GENET	   = ../../../../ec_genet
YANG_DIR   = ../../yang
YANGS	   = $(filter-out %-ann.yang, $(wildcard $(YANG_DIR)/*.yang))
NS_DIR	   = ../ns
NS_FILES   = $(YANGS:$(YANG_DIR)/%.yang=$(NS_DIR)/%.hrl)
ERL_FILES  = $(wildcard *.erl)
HRL_FILES  = $(wildcard *.hrl) $(NS_FILES)
MODULES    = $(ERL_FILES:%.erl=%)
EBIN_FILES = $(MODULES:%=../ebin/%.beam) ../ebin/$(APP_NAME).app

$(NS_DIR)/%.hrl: $(YANG_DIR)/%.yang
	$(NCSC) --emit-hrl $@ `ls $*-ann.yang  > /dev/null 2>&1 && echo "-a $*-ann.yang"` \
		--yangpath $(NCS_DIR)/src/ncs/yang \
		--strict-yang --fail-on-warnings $<

ERLC       = erlc
ERLC_FLAGS := \
	+warnings_as_errors \
	+debug_info \
	-I $(GENET)/src/erlang-lib/ec_genet/include -I $(NS_DIR) \
	-pa $(NCS_DIR)/erlang/econfd/ebin \
	-pa ../../ec_map_example/ebin

APPSCRIPT = '$$vsn=shift; $$mods=""; while(@ARGV){ $$_=shift; s/^([A-Z].*)$$/\'\''$$1\'\''/; $$mods.=", " if $$mods; $$mods .= $$_; } while(<>) { s/%VSN%/$$vsn/; s/%MODULES%/$$mods/; print; }'

all: $(NCS_DIR)/erlang/econfd/ebin $(NS_FILES) $(EBIN_FILES)

$(NCS_DIR)/erlang/econfd/ebin:
	@echo 'Where is NCS installed? Set $$NCS_DIR to point it out!'
	exit 1

clean:
	rm -f $(EBIN_FILES) $(NS_FILES) erl_crash.dump core* *~

../ebin/%.app: %.app.src ../vsn.mk Makefile
	perl -e $(APPSCRIPT) "$(VSN)" $(MODULES) < $< > $@

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<

$(EBIN_FILES): $(ERL_FILES) $(HRL_FILES)
