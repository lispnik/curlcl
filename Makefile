# Makefile for libcurl.
#
# Targets:
#   make test    load the test system and run the FiveAM suite (default)
#   make deps    restore ocicl-vendored dependencies
#   make tables  regenerate src/options.lisp and src/infos.lisp from the
#                installed libcurl headers (see generator/generate-tables.lisp)
#   make clean   remove compiled fasls, including the ASDF cache for this tree
#
# LISP overrides the Lisp used (default sbcl); the flags below assume an
# SBCL-compatible command line.  Dependencies are resolved by ASDF via the
# usual ocicl setup (run `make deps` first if they are not present).
#
# CURL_LIVE_TESTS=1 additionally runs the opt-in suite that talks to the real
# network; it is skipped by default so the suite stays hermetic.

LISP ?= sbcl

.PHONY: all test deps tables clean

all: test

# Run the suite, exiting non-zero if any check fails.  RUN-TESTS explains the
# failures and returns the status; fiveam:run! alone would exit 0 on failure.
test:
	$(LISP) --non-interactive \
	  --eval '(asdf:load-system :libcurl/test)' \
	  --eval '(uiop:quit (if (libcurl/test:run-tests) 0 1))'

deps:
	ocicl install

# Regenerate the committed option and info tables from the curl headers of the
# libcurl this machine would load.  Only run this deliberately: the emitted
# files are committed so a build never depends on headers being installed.
tables:
	$(LISP) --non-interactive \
	  --eval '(asdf:load-system :libcurl/generator)' \
	  --eval '(libcurl/generator:generate-tables)'

clean:
	rm -rf *.fasl
	rm -rf $(HOME)/.cache/common-lisp/*/$(CURDIR)
