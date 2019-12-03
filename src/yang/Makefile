# Include standard NCS examples build definitions and rules
include ../include.mk

SRCS = $(filter-out %-ann.yang, $(wildcard *.yang))
FXSS = $(SRCS:%.yang=../../load-dir/%.fxs)

all: $(FXSS)

../../load-dir/%.fxs: %.yang
	$(COMPILER) $(addprefix -a , $(wildcard $*-ann.yang)) \
             $(YANGPATH) -c -o $@ $<

clean:
	rm -rf $(FXSS)
