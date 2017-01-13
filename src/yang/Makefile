# Include standard NCS examples build definitions and rules
include $(NCS_DIR)/src/ncs/build/include.ncs.mk

SRCS = $(filter-out %-ann.yang, $(wildcard *.yang))
FXSS = $(SRCS:%.yang=../../load-dir/%.fxs)

all: $(FXSS)

../../load-dir/%.fxs: %.yang
	$(NCSC) $(addprefix -a , $(wildcard $*-ann.yang)) \
             $(YANGPATH) -c -o $@ $<

clean:
	rm -rf $(FXSS)
