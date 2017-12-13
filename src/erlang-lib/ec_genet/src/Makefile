include $(NCS_DIR)/src/ncs/build/include.ncs.mk 
include ../vsn.mk

APP_DIR    = $(shell dirname $(CURDIR))
APP_NAME   = $(shell basename $(APP_DIR))
ERL_FILES  = $(wildcard *.erl)
MODULES    = $(ERL_FILES:%.erl=%) ec_genet_logger
EBIN_FILES = $(MODULES:%=../ebin/%.beam) ../ebin/$(APP_NAME).app
YANG_DIR   = ../../../yang
YANGS	   = $(filter-out %-ann.yang, $(wildcard $(YANG_DIR)/*.yang))
NS_DIR	   = ../ns
NS_FILES   = $(YANGS:$(YANG_DIR)/%.yang=$(NS_DIR)/%.hrl)
HRL_FILES  = $(wildcard *.hrl) $(wildcard ../include/*.hrl) $(NS_FILES)

NCS_VERSION ?= 4.5
NCS_VERSION_CHECK = $(shell head -n1 $(NCS_DIR)/VERSION \
	| cut -d . -f-2 \
	| awk '{if ($$2 < $(NCS_VERSION)) {print "old"} else {print "new"}}')

ERLC       = erlc
ERLC_FLAGS := \
	+debug_info \
	-I $(APP_DIR)/include \
	-I $(APP_DIR)/ns \
	+warnings_as_errors \
	-pa $(NCS_DIR)/erlang/econfd/ebin \
	-pa ../../ec_genet/ebin

ifeq ($(NCS_VERSION_CHECK), old)
ERLC_FLAGS += -DCONFD_CS_IS_LEAF_LIST=0
endif

ifndef NO_AOP_LOGGING
ERLC_FLAGS += '+{parse_transform,aop}' '+{aop_config_dir, "logging"}' '+{aop_groups_module, genet_logging}'
endif


APPSCRIPT = '$$vsn=shift; $$mods=""; while(@ARGV){ $$_=shift; s/^([A-Z].*)$$/\'\''$$1\'\''/; $$mods.=", " if $$mods; $$mods .= $$_; } while(<>) { s/%VSN%/$$vsn/; s/%MODULES%/$$mods/; print; }'

all: $(NCS_DIR)/erlang/econfd/ebin logging_all $(EBIN_FILES)

EDOC_FLAG = ../edoc/.done
docs: $(EDOC_FLAG)

$(EDOC_FLAG): $(ERL_FILES) $(wildcard ../include/*.hrl) overview.edoc
	@mkdir -p $(dir $(EDOC_FLAG))
	erl -noshell -s edoc_run application genet '"."' \
		'[{dir,"$(dir $(EDOC_FLAG))"},{todo,true},no_subpackages,{overview, "overview.edoc"},{includes,["../include"]}]'
	@touch $(EDOC_FLAG)

.PHONY: logging_all
logging_all:
	$(MAKE) -C logging all


$(NS_DIR)/%.hrl: $(YANG_DIR)/%.yang
	$(NCSC) --emit-hrl $@ $(addprefix -a , $(wildcard $(YANG_DIR)/$*-ann.yang)) \
		--yangpath $(NCS_DIR)/src/ncs/yang \
		--strict-yang --fail-on-warnings $<


$(NCS_DIR)/erlang/econfd/ebin:
	@echo 'Where is NCS installed? Set $$NCS_DIR to point it out!'
	exit 1

clean:
	rm -f $(EBIN_FILES) $(NS_FILES) erl_crash.dump core* *~
	$(MAKE) -C logging clean

../ebin/%.app: %.app.src ../vsn.mk Makefile
	perl -e $(APPSCRIPT) "$(VSN)" $(MODULES) < $< > $@

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ../ebin $<

../ebin/aop.beam: logging_all

$(EBIN_FILES): $(HRL_FILES) ../ebin/aop.beam
