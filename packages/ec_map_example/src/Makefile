SUBDIRS=yang erlang-lib

all clean:
	for f in $(SUBDIRS); do \
	  (cd $$f && $(MAKE) $@ ) || exit 1; \
	done
