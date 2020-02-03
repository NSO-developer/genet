SUBDIRS=$(wildcard */Makefile erlang-lib/*/Makefile)

all clean:
	for f in $(SUBDIRS); do \
	  (cd `dirname $$f` && $(MAKE) $@ ) || exit 1; \
	done

docs:
	$(MAKE) -C erlang-lib/ec_genet $@
