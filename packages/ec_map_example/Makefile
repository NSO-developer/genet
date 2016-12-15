all clean:
	for f in erlang-lib/*/Makefile; do \
	  (cd `dirname $$f` && $(MAKE) $@ ) || exit 1; \
	done; \
