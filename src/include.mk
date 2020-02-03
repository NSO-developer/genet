ifndef SYSTEM_DIR
ifdef NCS_DIR
SYSTEM_DIR = $(NCS_DIR)
else
ifdef CONFD_DIR
SYSTEM_DIR = $(CONFD_DIR)
else
$(error SYSTEM_DIR not set, NCS_DIR/CONFD_DIR unknown - cannot continue)
endif
endif
endif

COMPILER = $(wildcard $(SYSTEM_DIR)/bin/confdc $(SYSTEM_DIR)/bin/ncsc)

ifeq (,$(findstring confd,$(COMPILER)))
SYSTEM = ncs
include $(SYSTEM_DIR)/src/$(SYSTEM)/build/include.ncs.mk
else
SYSTEM = confd
include $(SYSTEM_DIR)/src/$(SYSTEM)/build/include.mk
endif
