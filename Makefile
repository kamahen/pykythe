# Simple scripts for testing etc.

# See examples at the end of this file.
# You should be able to run with --jobs parameter and get the same
# results each time (the outputs should be idempotent), but the order
# of running will vary.  If you use --jobs, you should specify
# --output-sync=target (this seems to be the default).

# To see how to run this for a single source, see target
# test_pykythe_pykythe which also outputs *.kythe.entries files for
# all the imported files.
# Also scripts/test3a.sh (and test3.sh)
# ALso target test_pykythe_pykythe_all

# ("clean" shouldn't be necessary, but the dependencies are fairly
# complicated, so it's possible that "make test" doesn't run
# everything.  In particular, pykythe doesn't check its "version" when
# reusing an import.

# Assumes that ../kythe has been cloned from
# https://github.com/google/kythe and has been built with `bazel build
# //... //kythe/web/ui` and that the latest Kythe tarball has been
# downloaded and installed in /opt/kythe.

# To build the indexes for the pyythe source and display
# it with the Kythe browser:
# - download the latest Kythe build from https://github.com/kythe/kythe/releases
# - unpack it and link it into ~/kythe (e.g., so that ~/kythe/tools/http_server exists)
# - make -C ~/src/pykythe add-index-pykythe run-kythe-server
#   (if you use Emacs, you can't do "make run-kythe-server" in the *compilation*
#    window because Emacs will kill the process; instead, run it in a shell)
# Because of some pre-processing, $HOME/src/pykythe/* files show in
# /tmp/pykythe_test/SUBST/$HOME/src/pykythe/pykythe/ast_raw.py
# e.g.:
# http://localhost:8080/#/tmp/pykythe_test/SUBST/home/peter/src/pykythe/pykythe/ast_raw.py?root=ROOT&corpus=CORPUS&signature

# A word about abspath and realpath ...
#   Bash realpath resolves symlinks unless --no-symlinks is specified.
#   Make realpath fails if the file doesn't exist.
#   Therefore, we use Make abspath and Bash realpath --no-symlinks, so that
#   all paths are absolute and contain no extra "." or "/", but we stick
#   with symlinks (if any). This decision might need to be revisited.

TESTOUTDIR:=$(abspath /tmp/pykythe_test)
SHELL:=/bin/bash
# Assume that type -p returns an abspath ...
PYTHON3_EXE:=$(shell type -p python3.7)  # TODO: 3.9 doesn't have lib2to3
FIND_EXE:=$(shell type -p find)          # /usr/bin/find The following
# /usr/bin/swipl:
SWIPL_EXE_GLBL:=$(shell type -p swipl)
SWIPL_SRC:=swipl-devel/src
SWIPL_EXE_DEVEL:=$(abspath swipl-devel/build/src/swipl)
SWIPL_EXE_GITHUB:=$(abspath ../swipl-devel/build/src/swipl)
# SWIPL_EXE_GITHUB:=$(abspath ../swipl-devel/build.nlr/src/swipl)
# SWIPL_EXE:=$(abspath $(SWIPL_EXE_DEVEL))
SWIPL_EXE:=$(abspath $(SWIPL_EXE_GITHUB))
# SWIPL_EXE:=$(abspath $(SWIPL_EXE_GLBL))
# From github:
# SWIPL_EXE requires having run build-swpl or build-swpl-full, or else
# you'll get a weird error message about "--version: command not found"
# SWIPL_EXE:=$(realpath $(SWIPL_EXE_DEVEL))
COVERAGE=$(shell type -p coverage)      # /usr/local/bin/coverage
# For running parallel(1) - by experiment this works (2x the number of CPUs)
# (larger numbers smooth things out for processing large/small source files):
# TODO: parallel(1) can specify -j +3, for example
NPROC:=$(shell expr $$(nproc) \* 2)
NPROC_BAZEL:=$(shell expr $$(nproc))

# stuff for running tests (see https://kythe.io/docs/kythe-verifier.html)
KYTHE:=../kythe
KYTHE_BIN:=$(KYTHE)/bazel-bin
KYTHE_GENFILES:=$(KYTHE)/bazel-genfiles

BROWSE_PORT_PYKYTHE:=8080  # underhood assumes port 8080: underhood/treetide/underhood/ui/webpack.config.js
BROWSE_PORT_PYTYPE:=8089
SRC_BROWSER_PORT:=9999
# (VERIFIER_EXE is defined below, using the version built from source)
# VERIFIER_EXE:=/opt/kythe/tools/verifier
# TODO: Something happened with v0.0.31 or later that is incompatible
#       with older servers (which support the UI).
#       See also https://github.com/TreeTide/underhood/issues/12
# KYTHE_V:=kythe-v0.0.30
# ENTRYSTREAM_EXE:=$(HOME)/Downloads/$(KYTHE_V)/tools/entrystream
# WRITE_ENTRIES_EXE:=$(HOME)/Downloads/$(KYTHE_V)/tools/write_entries
# WRITE_TABLES_EXE:=$(HOME)/Downloads/$(KYTHE_V)/tools/write_tables
# HTTP_SERVER_EXE:=$(HOME)/Downloads/$(KYTHE_V)/tools/http_server
# # TODO: remove HTTP_SERVER_RESOURCES
# HTTP_SERVER_RESOURCES:=$(HOME)/Downloads/$(KYTHE_V)/web/ui
# Note: The following should contain v0.0.30 (34 doesn't work properly):
ENTRYSTREAM_EXE:=/opt/kythe/tools/entrystream
WRITE_ENTRIES_EXE:=/opt/kythe/tools/write_entries
WRITE_TABLES_EXE:=/opt/kythe/tools/write_tables
HTTP_SERVER_EXE:=/opt/kythe/tools/http_server
# TODO: remove HTTP_SERVER_RESOURCES
HTTP_SERVER_RESOURCES:=/opt/kythe/web/ui

# If Kythe built from source:
VERIFIER_EXE:=$(KYTHE_BIN)/kythe/cxx/verifier/verifier
# ENTRYSTREAM_EXE:=$(KYTHE_BIN)/kythe/go/platform/tools/entrystream/entrystream
# WRITE_ENTRIES_EXE:=$(KYTHE_BIN)/kythe/go/storage/tools/write_entries/write_entries
# WRITE_TABLES_EXE:=$(KYTHE_BIN)/kythe/go/serving/tools/write_tables/write_tables
# HTTP_SERVER_EXE:=$(KYTHE_BIN)/kythe/go/serving/tools/http_server/http_server

# TRIPLES_EXE:=$(KYTHE_BIN)/kythe/go/storage/tools/triples/triples
KYTHE_EXE:=$(KYTHE_BIN)/kythe/go/serving/tools/kythe/kythe

# WARNING: web_ui modifies $(HTTP_SERVER_RESOURCES)
# TODO: fix this for where the build_kythe target puts the resources
#       (e.g. ../kythe/bazel-genfiles/kythe/web/ui/resources/public)
# HTTP_SERVER_RESOURCES:=/tmp/kythe_resources

PYKYTHE_SRCS:=$(shell ls pykythe/*.{py,pl} | sort)
# TODO: see also https://docs.python.org/3/library/uuid.html
#	cat swipl-devel/VERSION -- if SWIPL_EXE (SWIPL_EXE_DEVEL) hasn't been built
VERSION:=$(shell ($(SWIPL_EXE_GLBL) --version; \
	$(PYTHON3_EXE) --version; head -999999 $(PYKYTHE_SRCS)) | sha1sum | cut -f1 -d' ')
# To add a random piece: $$RANDOM
# or with more randomness:
# -$(shell $(PYTHON3_EXE) -c 'import os, base64; print(base64.urlsafe_b64encode(os.urandom(9)).decode("ascii"))')
BATCH_ID:=$(shell date --utc --iso-8601=ns | sed -e 's/\+00:00//' -e 's/,/./')

TRACEDIR=/tmp
TRACEFILE=strace
# PYKYTHE_STRACE=strace -o $(TRACEDIR)/$(TRACEFILE)-$$(date '+%Y%m%d-%H%M%S.%N')
PYKYTHE_STRACE=
PYKYTHE_EXE=$(TESTOUTDIR)/pykythe.qlf
# PYKYTHE_EXE_ is used for, e.g. "swipl pykythe/pykythe.pl --"
PYKYTHE_EXE_=$(PYKYTHE_EXE)
# PYKYTHE_EXE_=$(SWIPL_EXE) pykythe/pykythe.pl --
TEST_DATA_DIR:=test_data
#  TEST_GRAMMAR_FILE:=py3_test_grammar.py  # TODO: check all places this is used
TESTGITHUB:=$(HOME)/tmp/test-github
PARSECMD_OPT:=--parsecmd="$(PYTHON3_EXE) -m pykythe"
# ENTRIESCMD_OPT:=--entriescmd=$(realpath ../kythe/bazel-bin/kythe/go/platform/tools/entrystream/entrystream)
ENTRIESCMD_OPT:=--entriescmd=$(ENTRYSTREAM_EXE)
# PYTHONPATH starts at .., so "absolute" paths in test_data should be
#            of the form "pykythe.test_data.___"
#            (see also fix_for_verifier.py and ${ROOT_DIR} etc. substitutions
PWD_REAL:=$(abspath .)
TYPESHED_REAL:=$(abspath ./typeshed)
SUBSTDIR:=$(TESTOUTDIR)/SUBST
KYTHEOUTDIR:=$(TESTOUTDIR)/KYTHE
BUILTINS_SYMTAB_FILE:=$(TESTOUTDIR)/KYTHE/builtins_symtab.pl
TESTOUT_PYKYTHEDIR:=$(KYTHEOUTDIR)/pykythe
# $(PWD_REAL) starts with "/", so we're just appending:
SUBSTDIR_PWD_REAL:=$(SUBSTDIR)$(PWD_REAL)
KYTHEOUTDIR_PWD_REAL:=$(KYTHEOUTDIR)$(PWD_REAL)
PYTHONPATH_DOT:=$(shell realpath --no-symlinks .. | sed 's!^/!$(SUBSTDIR)/!')
PYTHONPATH_BUILTINS:=$(SUBSTDIR)/BUILTINS
BUILTINS_PATH:=$(SUBSTDIR)/BUILTINS/builtins.pyi
TESTOUT_SRCS:=$(shell $(FIND_EXE) $(TEST_DATA_DIR) -name '*.py'  | sort | \
    sed -e 's!^!$(SUBSTDIR_PWD_REAL)/!')
TESTOUT_TARGETS:=$(shell $(FIND_EXE) $(TEST_DATA_DIR) -name '*.py' | sort | \
    sed -e 's!^!$(KYTHEOUTDIR)$(SUBSTDIR_PWD_REAL)/!' -e 's!\.py$$!.kythe.verifier!')
TESTOUT_TYPESHED:=$(KYTHEOUTDIR)$(TYPESHED_REAL)
# Note: kythe_corpus would normally be '' or '/'; it is set to
#       'CORPUS' to verify that it gets passed through properly
#       ('' could be happend if something fails pass the value).
KYTHE_CORPUS_ROOT_OPT:=--kythe_corpus='CORPUS' --kythe_root='ROOT'
VERSION_OPT:=--version='$(VERSION)'
PYKYTHEOUT_OPT:=--kytheout='$(KYTHEOUTDIR)'
ifeq ($(BATCH_ID),)
    BATCH_OPT:=--pykythebatch_suffix=
else
    BATCH_OPT:=--pykythebatch_suffix='.pykythe.batch-$(BATCH_ID)'
endif
# TODO: parameterize following for python3.7, etc.:
PYTHONPATH_OPT:=--pythonpath='$(PYTHONPATH_DOT):$(PYTHONPATH_BUILTINS):$(TYPESHED_REAL)/stdlib/3.7:$(TYPESHED_REAL)/stdlib/3:$(TYPESHED_REAL)/stdlib/2and3:/usr/lib/python3.7'
PYTHONPATH_OPT_NO_SUBST:=--pythonpath='$(PYTHONPATH_DOT):$(TYPESHED_REAL)/stdlib/3.7:$(TYPESHED_REAL)/stdlib/3:$(TYPESHED_REAL)/stdlib/2and3:/usr/lib/python3.7'
PYKYTHE_OPTS0=$(VERSION_OPT) $(BATCH_OPT) \
	--builtins_symtab=$(BUILTINS_SYMTAB_FILE) \
	--builtins_path=$(BUILTINS_PATH) \
	$(PYKYTHEOUT_OPT) $(PARSECMD_OPT) $(ENTRIESCMD_OPT) $(KYTHE_CORPUS_ROOT_OPT)
PYKYTHE_OPTS=$(PYKYTHE_OPTS0) $(PYTHONPATH_OPT)
TIME:=time

# .PRECIOUS: %.kythe.entries %.json-decoded %.json
.SECONDARY:  # Do not delete any intermediate files

.PHONY: FORCE

.PHONY: show-vars
show-vars:
	@echo "BATCH_ID                 $(BATCH_ID)"
	@echo "VERSION                  $(VERSION)"
	@echo "TESTOUTDIR               $(TESTOUTDIR)"
	@echo "PWD_REAL                 $(PWD_REAL)"
	@echo "TYPESHED_REAL            $(TYPESHED_REAL)"
	@echo "SUBSTDIR                 $(SUBSTDIR)"
	@echo "KYTHEOUTDIR              $(KYTHEOUTDIR)"
	@echo "BUILTINS_SYMTAB_FILE     $(BUILTINS_SYMTAB_FILE)"
	@echo "TESTOUT_PYKYTHEDIR       $(TESTOUT_PYKYTHEDIR)"
	@echo "SUBSTDIR_PWD_REAL        $(SUBSTDIR_PWD_REAL)"
	@echo "KYTHEOUTDIR_PWD_REAL     $(KYTHEOUTDIR_PWD_REAL)"
	@echo "PYTHONPATH_DOT           $(PYTHONPATH_DOT)"
	@echo "PYTHONPATH_BUILTINS      $(PYTHONPATH_BUILTINS)"
	@echo "SWIPL_EXE                $(SWIPL_EXE)"
	@# echo "SWIPL_EXE_GLBL ..."
	@echo "SWIPL_EXE_DEVEL          $(SWIPL_EXE_DEVEL)"
	@# echo "TESTOUT_SRCS           $(TESTOUT_SRCS)"
	@echo

# $(PYKYTHE_SRCS) is a dependency because it's used to compute $(VERSION)
$(SUBSTDIR_PWD_REAL)/pykythe/bootstrap_builtins_symtab.pl: $(PYKYTHE_SRCS)

$(SUBSTDIR_PWD_REAL)/%: % scripts/fix_for_verifier.py
	@# FROM_DIR TO_DIR TYPESHED_DIR FROM_FILE TO_FILE
	@echo "fix >$@"
	@mkdir -p "$(dir $@)"
	@# TODO: the following shouldn't be needed if all the
	@#       *.py files all processed, but for some reason
	@#       some __init__.py files aren't (or maybe only pykythe/__init__.py)
	@# for i in $$(find $(SUBSTDIR_PWD_REAL) -type d); do touch "$$i/__init__.py"; done
	@$(PYTHON3_EXE) scripts/fix_for_verifier.py "${VERSION}" "$(TEST_DATA_DIR)" "$(SUBSTDIR)$(abspath $(TEST_DATA_DIR))" "$(TYPESHED_REAL)" "$(PYTHONPATH_BUILTINS)" \
		"$(abspath $<)" "$@"

########
# TODO: the following have an extra stutter in the files
########

# $(SUBSTDIR_PWD_REAL)/test_data/imports_dir1/i1.py: \
# 	$(SUBSTDIR_PWD_REAL)/test_data/imports_dir1/i3.py \
# 	$(SUBSTDIR_PWD_REAL)/test_data/imports_dir1/i1_sub/i4.py \
# 	$(SUBSTDIR_PWD_REAL)/test_data/imports_dir1/i1_sub/i4a.py

# $(SUBSTDIR_PWD_REAL)/test_data/imports_dir1/i1_sub/i4.py: \
# 	$(SUBSTDIR_PWD_REAL)/test_data/imports_dir1/i1_sub/i4a.py \
# 	$(SUBSTDIR_PWD_REAL)/test_data/imports_dir1/i5.py \
# 	$(SUBSTDIR_PWD_REAL)/test_data/imports_dir1/i8/i9.py

# $(SUBSTDIR_PWD_REAL)/test_data/imports_dir1/imports1.py: \
# 	$(SUBSTDIR_PWD_REAL)/test_data/imports_dir1/i1_sub/i4.py \
# 	$(SUBSTDIR_PWD_REAL)/test_data/imports_file1 \
# 	$(SUBSTDIR_PWD_REAL)/test_data/imports_dir1/i6.py \
# 	$(SUBSTDIR_PWD_REAL)/test_data/imports_dir1/i7.py \
# 	$(SUBSTDIR_PWD_REAL)/test_data/imports_dir1/i8/__init__.py

.PHONY: testout_srcs
testout_srcs: $(TESTOUT_SRCS)

.PHONY: importlab
importlab:
	../importlab/bin/importlab --tree --trim -P.. .

.PHONY: pykythe_test
pykythe_test: $(SWIPL_EXE) FORCE # $(TESTOUTDIR)/KYTHE/builtins_symtab.pl tests/c3_tests.pl
	mkdir -p $(PYTHONPATH_DOT) "$(PYTHONPATH_BUILTINS)"
	@# "test_data/imports1.py" is used in the test suite and must be a real file
	@# because absolute_file resolution uses the existence of the file.
	$(SWIPL_EXE) --version
	$(SWIPL_EXE) </dev/null
	@# DO NOT SUBMIT --threads=yes
	$(SWIPL_EXE) -g run_tests -t halt tests/c3_tests.pl
	$(SWIPL_EXE) --threads=no -g 'plunit:load_test_files([])' -g plunit:pykythe_run_tests -t halt -l pykythe/pykythe.pl \
		-- $(PYTHONPATH_OPT) test_data/dummy_dir/dummy_file.py
	$(SWIPL_EXE) --threads=yes -g 'plunit:load_test_files([])' -g plunit:pykythe_run_tests -t halt -l pykythe/pykythe.pl -- $(PYTHONPATH_OPT) test_data/dummy_dir/dummy_file.py

$(PYKYTHE_EXE): pykythe/*.pl pykythe_test $(SWIPL_EXE)
	mkdir -p $(dir $@)
	$(SWIPL_EXE) --version
	$(SWIPL_EXE) --stand_alone=true --undefined=error --verbose=false \
	    --foreign=save \
	    -o $@ -c pykythe/pykythe.pl
	@# To find the .so files and packages:
	@# ldd $@ | grep '=>' | cut -f3 -d' ' | sort | xargs -L1 dpkg-query -S | grep -v libc6:amd64

# TODO: this rule requires all the SUBST files ($(TESTOUT_SRCS))
#       because it isn't easy to write more specific Make fules (and
#       what about circular imports?):
#  $(SUBSTDIR_PWD_REAL)/%.py
# NOTE: the 1st argument is important (used by "$<")
$(KYTHEOUTDIR)%.kythe.json \
$(KYTHEOUTDIR)%.kythe.entries: \
		%.py \
		$(TESTOUT_SRCS) \
		$(PYKYTHE_SRCS) \
		$(BUILTINS_SYMTAB_FILE) \
	 	$(PYKYTHE_EXE) \
		pykythe/pykythe.pl pykythe/*.pl \
		pykythe/__main__.py pykythe/*.py
	@# TODO: make this into a script (with a saved state - qsave_program/2 stand_alone).
	@#       maybe?: set_prolog_flag(generate_debug_info, false)
	@# Note that -O changes the order of some directives (see the comment in
	@# pykythe/pykythe.pl with the last `set_prolog_flag(autoload, false)`.
	PYKYTHE_STRACE_EXE="$(PYKYTHE_STRACE)" && \
	  echo $$PYKYTHE_STRACE_EXE && \
	  $(TIME) $$PYKYTHE_STRACE_EXE $(PYKYTHE_EXE_) \
	    $(PYKYTHE_OPTS) \
	    "$<"

# Bootstrap the builtins:

# WARNING -- this doesn't play nicely with --jobs because $(BUILTINS_SYMTAB_FILE)
#            is output twice
#            TODO: split into two rules and have a builtins_symtab_bootstrap.pl

# TODO: This uses /tmp/pykythe_test/SUBST/home/peter/src/pykythe/pykythe/bootstrap_builtins.py
#       This is the same as make $(mumble)/src/typeshed/stdlib/2and3/builtins.kythe.json
#       followed by gen_builtins_symtab.
# $(PYKYTHE_SRCS) is a dependency because it's used to compute $(VERSION)

$(BUILTINS_SYMTAB_FILE) \
$(KYTHEOUTDIR)$(TYPESHED_REAL)/stdlib/2and3/builtins.pykythe.symtab \
$(KYTHEOUTDIR)$(TYPESHED_REAL)/stdlib/2and3/builtins.kythe.json \
$(KYTHEOUTDIR)$(TYPESHED_REAL)/stdlib/2and3/builtins.kythe.entries: \
		$(SUBSTDIR_PWD_REAL)/pykythe/bootstrap_builtins.py \
		$(SUBSTDIR_PWD_REAL)/pykythe/bootstrap_builtins_symtab.pl \
		$(PYKYTHE_SRCS) \
	 	$(PYKYTHE_EXE) \
		$(TYPESHED_REAL)/stdlib/2and3/builtins.pyi pykythe/builtins_extra.pyi \
		pykythe/gen_builtins_symtab.pl \
		pykythe/pykythe.pl pykythe/*.pl \
		pykythe/__main__.py pykythe/*.py
	mkdir -p "$(dir $(BUILTINS_SYMTAB_FILE))"
	@# Override the builtins:
	mkdir -p "$(PYTHONPATH_BUILTINS)"
	cat $(TYPESHED_REAL)/stdlib/2and3/builtins.pyi \
	        pykythe/builtins_extra.pyi \
		>$(BUILTINS_PATH)
	@# This bootstrap symtab file will be replaced by running gen_builtins_symtab.pl below
	cat $(SUBSTDIR_PWD_REAL)/pykythe/bootstrap_builtins_symtab.pl \
		>"$(BUILTINS_SYMTAB_FILE)"
	PYKYTHE_STRACE_EXE="$(PYKYTHE_STRACE)" && \
	  echo $$PYKYTHE_STRACE_EXE && \
	  $(TIME) $$PYKYTHE_STRACE_EXE $(PYKYTHE_EXE_) \
	    $(PYKYTHE_OPTS) \
	    "$(SUBSTDIR_PWD_REAL)/pykythe/bootstrap_builtins.py"
	@# instead of input from "$(KYTHEOUTDIR)$(TYPESHED_REAL)/stdlib/2and3/builtins.kythe.json"
	@# use "$(KYTHEOUTDIR)$(PYTHONPATH_BUILTINS)/builtins.kythe.json":
	$(SWIPL_EXE) pykythe/gen_builtins_symtab.pl \
	    -- $(VERSION_OPT) $(PYTHONPATH_OPT) \
	    $(KYTHEOUTDIR)$(PYTHONPATH_BUILTINS)/builtins.pykythe.symtab \
	    $(KYTHEOUTDIR)$(PYTHONPATH_BUILTINS)/builtins.kythe.json \
	    "$(BUILTINS_SYMTAB_FILE)"
	@# For debugging:
	$(MAKE) $(KYTHEOUTDIR)$(PYTHONPATH_BUILTINS)/builtins.kythe.json-decoded

%.json-decoded: %.json scripts/decode_json.py
	$(PYTHON3_EXE) scripts/decode_json.py <"$<" >"$@"

.PHONY: json-decoded-all
json-decoded-all:
	set -o pipefail; \
	find $(KYTHEOUTDIR) -type f -name '*.kythe.json' | sort | \
	  time parallel --will-cite -L1 -j$(NPROC) \
	  '$(PYTHON3_EXE) scripts/decode_json.py <{} >{}-decoded'

$(KYTHEOUTDIR)/%.kythe.verifier: $(KYTHEOUTDIR)/%.kythe.entries
	@# TODO: --ignore_dups
	@# TODO: concatenate all *.kythe.entries files so that
	@#       more ./pykythe/type goals can be executed
	@#       (see rules add-index-pykythe)
	@#       (may need to pipe through sort -u)
	set -o pipefail; $(VERIFIER_EXE) -check_for_singletons -goal_prefix='#-' \
		"/$*.py" \
		<"$<" | tee "$@" || (rm "$@" ; exit 1)

.PHONY: verify-%
# TODO: make the following work:
verify-%: $(KYTHEOUTDIR_PWD_REAL)/$(TEST_DATA_DIR)/%.kythe.verifier

.PHONY: etags
etags: pykythe/TAGS

pykythe/TAGS: pykythe/TAGS-py pykythe/TAGS-pl
	cat pykythe/TAGS-pl pykythe/TAGS-py >$@

pykythe/TAGS-pl: pykythe/*.pl browser/*.pl scripts/*.pl /usr/lib/swi-prolog/library/*.pl /usr/lib/swi-prolog/library/http/*.pl
	cd pykythe ; etags -l prolog -o ../$@ *.pl ../browser/*.pl ../scripts/*.pl ../tests/test_pykythe.py /usr/lib/swi-prolog/library/*.pl /usr/lib/swi-prolog/library/http/*.pl

pykythe/TAGS-py: pykythe/*.py tests/test_pykythe.py
	cd pykythe ; etags -l python -o ../$@ *.py ../tests/test_pykythe.py

.PHONY: test
test: all_tests

.PHONY: all_tests
all_tests: etags unit_tests pykythe_test test_imports1 test_data_tests # json-decoded-all

.PHONY: unit_tests
unit_tests: tests/test_pykythe.py \
		pykythe/ast.py \
		pykythe/ast_color.py \
		pykythe/ast_cooked.py \
		pykythe/ast_raw.py \
		pykythe/fakesys.py \
		pykythe/pod.py
	$(PYTHON3_EXE) tests/test_pykythe.py

.PHONY: test_imports1
test_imports1:  # run imports code, to ensure that it behaves as expected
	cd .. && PYTHONPATH=. $(PYTHON3_EXE) -B pykythe/test_data/imports1.py
	cd test_data && PYTHONPATH=../.. $(PYTHON3_EXE) -B imports1.py

test_c3_a:  # run c3_a, to ensure it behaves as expected
	$(PYTHON3_EXE) -B test_data/c3_a.py

.PHONY: test_data_tests
test_data_tests:
	@# running in parallel gains ~30%, it seems
	@# (large files such as py3_test_grammar.py dominate)
	$(MAKE) -j$(NPROC) -Oline $(TESTOUT_TARGETS) \
		$(KYTHEOUTDIR)$(PWD_REAL)/pykythe/__main__.kythe.entries

.PHONY: test_python_lib
test_python_lib: # Also does some other source files I have lying around
	$(MAKE) $(PYKYTHE_EXE) $(BUILTINS_SYMTAB_FILE)
	@# TODO: too many args causes "out of file resources":
	@#     $(TIME) $(PYKYTHE_EXE_) $(PYKYTHE_OPTS0) $(PYTHONPATH_OPT_NO_SUBST) $$(find /usr/lib/python3.7 -name '*.py' | sort)
	@# "sort" in the following is to make results more reproducible
	@# "--group" probably slows things down a bit - there's a noticable dip
	@#           in the CPU history every so often, which presumably is when
	@#           the outputs are gathered and printed

	@# There are roughly 1300 files in /usr/lib/python3.7 (6900 in the whole test),
	@#     so batches of 50-150 are reasonable.
	@# parallel(1) seems to spawn too many jobs, which causes paging,
	@#     so limit it with the "-j" option
	@#     (could also use the --semaphore --fg option)
	@# TODO: /usr has more *.py files than /usr/lib/python3.7 but takes 3x longer
	@# TODO: use annotate-output (from package devscripts) to add the timestamps
	@#       and remove the timestamps from pykytype's logging.
	@# Note: ./typeshed not here because it's in mypy and pytype
	@# TODO: paralall -L80 means it processes in clumps of 80 --
	@#       parameterize this on NPROC?
	set -o pipefail; \
	find /usr/lib/python3.7 ../mypy ../pytype ../yapf ../importlab ../kythe . \
	  -name '*.py' -o -name '*.pyi' | sort | \
	  parallel -v --will-cite --keep-order --group -L80 -j$(NPROC) \
	  --joblog=$(TESTOUTDIR)/joblog-$$(date +%Y-%m-%d-%H-%M) \
	  '/usr/bin/time -f "\t%E real\t%U user\t%S sys\t%I-%O file" \
	    $(PYKYTHE_EXE_) $(PYKYTHE_OPTS0) $(PYTHONPATH_OPT_NO_SUBST) {}'

.PHONY: test_single_src
# This is an example of running on a single source
SINGLE_SRC=/usr/lib/python3.7/multiprocessing/connection.py
SINGLE_SRC=/tmp/pykythe_test/SUBST/home/peter/src/pykythe/test_data/a10.py
test_single_src:
	$(MAKE) $(SINGLE_SRC)
	$(MAKE) $(PYKYTHE_EXE_) $(BUILTINS_SYMTAB_FILE)
	$(TIME) $(PYKYTHE_EXE_) $(PYKYTHE_OPTS0) $(PYTHONPATH_OPT_NO_SUBST) $(SINGLE_SRC)

# Reformat all the source code (uses .style.yapf)
.PHONY: pyformat
pyformat:
	find . -type f -name '*.py' | \
		grep -v $(TEST_DATA_DIR) | grep -v /typeshed/ | \
		xargs yapf -i

.PHONY: yapf
yapf: pyformat

# Don't use "black" to reformat -- it changes the source code
# (adds "," to lists).
.PHONY: black
black:
	find . -type f -name '*.py' | grep -v $(TEST_DATA_DIR) | xargs black -l 99 -S

.PHONY: pylint
pylint:
	find . -type f -name '*.py' | grep -v $(TEST_DATA_DIR) | grep -v /typeshed/ | \
		grep -v snippets.py | xargs -L1 pylint --disable=missing-docstring,fixme,no-else-return,bad-continuation

.PHONY: pyflakes
pyflakes:
	find . -type f -name '*.py' | grep -v $(TEST_DATA_DIR) | \
		grep -v snippets.py | xargs -L1 pyflakes

PYTYPE_DIR=/tmp/pykythe_pytype
# TODO: PYTYPE_V=3.7 when it's supported:
PYTYPE_V=3.6
# Can't do the following because need to compile
# things like parser_ext:
#   PYTYPE=PYTHONPATH=$$(pwd)/../pytype ../pytype/scripts/pytype
# and PYTHONPATH seems to confuse other things in:N
#   PYTYPE=PYTHONPATH=$$(pwd)/../pytype/build/lib.linux-x86_64-2.7 ../pytype/scripts/pytype
# The following requires having done an install from ../pytype:
# PYTYPE=/usr/local/bin/pytype
PYTYPE=$(shell type -p pytype)
# TODO: in the following, it would be nice if we could remove the
#       "cd pykythe" and instead using the file name "pykythe/...py"
#       but that seems to upset pytype's imnport mechanism.

# TODO: --python-version=3.6  # conflict if python3.6 is not default python3
#       maybe --no-site-packages ?
# Anyway, mypy doesn't yet have a plugin for dataclasses. :(
MYPY:=$(shell type -p mypy) --python-version=3.7 --strict-optional --check-untyped-defs --warn-incomplete-stub --warn-no-return --no-incremental --disallow-any-unimported --show-error-context --implicit-optional --strict --disallow-incomplete-defs
# TODO: --disallow-incomplete-defs  https://github.com/python/mypy/issues/4603
# TODO: --disallow-any-generics

.PHONY: mypy
mypy:
	$(MYPY) pykythe/__main__.py
	$(MYPY) tests/test_pykythe.py

.PHONY: lint
lint: pylint

.PHONY: clean
clean:
	@# preserve joblog
	$(RM) -r $(TESTOUTDIR)/KYTHE $(TESTOUTDIR)/SUBST $(PYKYTHE_EXE) pykythe/TAGS*

.PHONY: clean-lite clean_lite
clean-lite clean_lite:
	$(RM) -r $(KYTHEOUTDIR)

.PHONY: touch-fixed-src touch_fixed_src
touch-fixed-src touch_fixed_src:  # For doing a rerun with cache preserved
	-find $(SUBSTDIR) -type f -print0 | xargs -0 touch

.PHONY: clean-cache clean_cache
clean-batch clean_batch:
	@# leaves the BATCH cache intact
	find $(KYTHEOUTDIR) -name '*.kythe.json' -o -name '*.kythe.json-decoded' -o -name '*.kythe.entries' -o -name '*.kythe.verifier' -delete

PHONY: clean-batch clean_batch
	find $(KYTHEOUTDIR) -name '*.kythe.json-batch-*' -delete

.PHONY: clean-testout-srcs clean_testout_srcs
clean-testout-srcs clean_testout_srcs:
	find $(SUBSTDIR) -name '*.py' -delete

clean-batch clean_batch:

.PHONY: tkdiff
tkdiff:
	git difftool --no-prompt --tool=tkdiff
	@# $$(git diff --name-only | fgrep -v browser/examples/kythe_facts.pl)	

# .nq.gz files are for Cayley

#@#@# prep_server: $(TESTOUTDIR)/$(TEST_GRAMMAR_FILE).nq.gz

%.nq.gz: %.entries
	$(RM) -r $(TESTOUTDIR)/graphstore $(TESTOUTDIR)/tables
	@mkdir -p $(TESTOUTDIR)/graphstore $(TESTOUTDIR)/tables
	$(WRITE_ENTRIES_EXE) -graphstore $(TESTOUTDIR)/graphstore <"$<"
	$(WRITE_TABLES_EXE) -graphstore=$(TESTOUTDIR)/graphstore -out=$(TESTOUTDIR)/tables
	$(TRIPLES_EXE) "$<" | gzip >"$@"
	$(TRIPLES_EXE) -graphstore $(TESTOUTDIR)/graphstore

.PHONY: add-index-pykythe
# $(wildcard test_data/**.py) doesn't work, so do it this way:
TEST_FILES:=$(shell find pykythe test_data -name '*.py')
add-index-pykythe: \
		$(foreach file,$(TEST_FILES),$(basename $(KYTHEOUTDIR)$(SUBSTDIR)$(abspath $(file))).kythe.entries)
	$(MAKE) make-tables

.PHONY: make-tables
make-tables: # add-index-pykythe
	$(RM) -r $(TESTOUTDIR)/graphstore $(TESTOUTDIR)/tables
	mkdir -p $(TESTOUTDIR)/graphstore $(TESTOUTDIR)/tables
	@# cat $(basename $(KYTHEOUTDIR)$(SUBSTDIR)$(abspath pykythe))/*.kythe.json
	set -o pipefail; \
	    cat $$(find $(KYTHEOUTDIR) -name '*.kythe.json') /dev/null | \
	    time $(ENTRYSTREAM_EXE) --read_format=json | \
	    time $(WRITE_ENTRIES_EXE) -graphstore $(TESTOUTDIR)/graphstore
	time $(WRITE_TABLES_EXE) -graphstore=$(TESTOUTDIR)/graphstore -out=$(TESTOUTDIR)/tables
	@# To view items without server running:
	@ # $(KYTHE_EXE) -api /tmp/pykythe_test/tables nodes -max_fact_size=200 'kythe://CORPUS?lang=python?root=ROOT#.tmp.pykythe_test.SUBST.home.peter.src.pykythe.test_data.imports_dir1.i1_sub.i4.III'
	@ # TODO: requires server running: https://github.com/kythe/kythe/issues/4248
	@ # $(KYTHE_EXE) -api http://localhost:$(BROWSE_PORT_PYKYTHE) xrefs -definitions all -node_definitions -page_size 999999 -references all 'kythe://CORPUS?lang=python?root=ROOT#.tmp.pykythe_test.SUBST.home.peter.src.pykythe.pykythe.pod.PlainOldData'
	@ # https://kythe.io/docs/kythes-command-line-tool.html

.PHONY: make-json
make-json:
	$(RM) -r $(TESTOUTDIR)/browser
	mkdir -p $(TESTOUTDIR)/browser/files
	@# in following: - 99 files in typeshed, 43 in test_data, 10 in pykythe
	set -o pipefail; \
	    find $(KYTHEOUTDIR) -name '*.kythe.json' | \
	    time $(SWIPL_EXE) -g main -t halt \
		browser/kythe_json_to_prolog.pl -- \
		--filesdir=$(TESTOUTDIR)/browser/files
	@# see run-src-browser, which forces a compile on first load
	@# time $(SWIPL_EXE) -g "qcompile('$(TESTOUTDIR)/browser/files/kythe_facts.pl')" -t halt
	@# kythe_facts.pl is 114MB, so don't copy it.
	@# cp --preserve=timestamps $(TESTOUTDIR)/browser/files/kythe_facts.pl browser/examples/

# This is obsolete (from when the JSON files were pre-generated)
# but shows how to pretty-print a JSON file.
.PHONY: make-json-pretty
make-json-pretty:
	find $(TESTOUTDIR)/browser/files -name '*.json' | \
		parallel --will-cite -L1 -j$(NPROC) \
		'$(PYTHON3_EXE) -m json.tool <{} >{}-pretty'

.PHONY: run_src_browser run-src-browser
# To prepare this: make-json
# http://localhost:$(SRC_BROWSER_PORT)/static/src_browser.html
run_src_browser run-src-browser:
	@mkdir -p $(TESTOUTDIR)/browser/files
	@# if .qlf doesn't exist, make an empty one, which
	@# will cause recompilation when it's loaded
	[ -e $(TESTOUTDIR)/browser/files/kythe_facts.qlf ] || \
	    touch $(TESTOUTDIR)/browser/files/kythe_facts.qlf
	@# TODO: remove "-g src_browser:main -l" (which are for debugging)  DO NOT SUBMIT
	@# TODO: compile src_browser.pl and use src_browser.qlf
	@# -- for debugging, change "browser/src_browser.pl" to
	@#                       to "-g src_browser:src_browser_main2 -l browser/src_browser.pl"
	$(SWIPL_EXE) --no-tty browser/src_browser.pl -- \
		--port=$(SRC_BROWSER_PORT) \
		--filesdir=$(TESTOUTDIR)/browser/files \
		--staticdir=$(realpath ./browser/static)

# TODO: pre-req:  prep_server
.PHONY: run_kythe_server run-kythe-server
# -public_resources=$(HTTP_SERVER_RESOURCES)
run_kythe_server run-kythe-server: # web_ui  # TODO: uncomment web_ui
	@# This is wrong: -listen=localhost:$(BROWSE_PORT_PYKYTHE)
	$(HTTP_SERVER_EXE) -serving_table=$(TESTOUTDIR)/tables \
	  -listen=:$(BROWSE_PORT_PYKYTHE)
	@# To view items with server running:
	@ $(KYTHE_EXE) -api http://localhost:$(BROWSE_PORT_PYKYTHE) nodes -max_fact_size=200 'kythe://CORPUS?lang=python?root=ROOT#tmp.pykythe_test.SUBST.home.peter.src.pykythe.test_data.t8.III'

run-kythe-server-pytype:
	$(HTTP_SERVER_EXE) -serving_table=/tmp/pytype-tables -listen=:$(BROWSE_PORT_PYKYTHE)

.PHONY: kythe-kythe
kythe-kythe:
	$(KYTHE_EXE) -api http://localhost:$(BROWSE_PORT_PYKYTHE)  # nodes -max_fact_size 999

.PHONY: build_kythe build-kythe
build_kythe build-kythe:
	cd ../kythe && git remote show origin && git pull --recurse-submodules
	cd ../kythe && nice bazel build --jobs=$(NPROC_BAZEL) @local_config_cc//:toolchain
	cd ../kythe && nice bazel build --jobs=$(NPROC_BAZEL) //...
	cd ../kythe && nice bazel test -k --jobs=$(NPROC_BAZEL) //...
	@# TODO: don't need LEIN_JAVA_CMD any more?
	cd ../kythe && LEIN_JAVA_CMD=/usr/lib/jvm/java-8-openjdk-amd64/bin/java nice bazel build //kythe/web/ui
	cd ../kythe && bazel shutdown

.PHONY: web_ui
web_ui:
	@# assumes that build_kythe has been done
	$(RM) -rf $(HTTP_SERVER_RESOURCES)
	@mkdir -p $(HTTP_SERVER_RESOURCES)
	cp -pR $(KYTHE)/kythe/web/ui/resources/public/{css,js,index.html} $(HTTP_SERVER_RESOURCES)
	cp -p  $(KYTHE_GENFILES)/kythe/web/ui/resources/public/js/main.js $(HTTP_SERVER_RESOURCES)/js/
# .PHONY: triples
# triples: $(TESTOUTDIR)/$(TEST_GRAMMAR_FILE).nq.gz

.PHONY: gc
gc:
	du -hsc .
	git gc
	git repack -a -d -f --depth=250 --window=250
	git gc
	du -hsc .

.PHONY: snapshot
snapshot:
	@# find . -name __pycache__ -type d | xargs rm -rf
	git gc
	cd .. && tar --create \
		--exclude=.cayley_history \
		--exclude=.mypy_cache \
		--exclude=__pycache__ \
		--exclude=.pytype \
		--exclude=.lgt_tmp \
		--exclude=typescript --exclude=typescript.gz \
		--gzip --file \
		$(HOME)/Downloads/pykythe_$$(date +%Y-%m-%d-%H-%M).tgz pykythe
	@# ls -lh $(HOME)/Downloads/pykythe_*.tgz

#@#@# PHONY: ls_uris
#@#@# ls_uris:
#@#@# 	$(KYTHE_EXE) -api $(TESTOUTDIR)/tables ls -uris

#@#@# PHONY: ls_decor
#@#@# ls_decor:
#@#@# 	$(KYTHE_EXE) -api $(TESTOUTDIR)/tables decor kythe://CORPUS?path=$(TEST_DATA_DIR)/$(TEST_GRAMMAR_FILE).py

FORCE:
.PHONY: FORCE


#@#@# PHONY: coverage
#@#@# coverage:
#@#@# 	-# file:///home/peter/src/pykythe/htmlcov/index.html
#@#@# 	-# TODO: use the variables in the rule for $(TESTOUTDIR)/%.kythe.json
#@#@# 	-# TODO: run test_pykythe.py and add to coverage results
#@#@# 	$(PYTHON3_EXE) $(COVERAGE) run --branch -m pykythe \
#@#@# 		$(KYTHE_CORPUS_ROOT_OPT) \
#@#@# 		--src="$(TEST_DATA_DIR)/py3_test_grammar.py" \
#@#@# 		--out_kythe=/dev/null --out_fqn_expr=/dev/null
#@#@# 	-# $(PYTHON3_EXE) $(COVERAGE) run --branch tests/test_pykythe.py
#@#@# 	$(COVERAGE) html
#@#@# 	rm -r pykythe/__pycache__


pytype:
	@# TODO: ast_cooked.py needs "from __future__" removed to make pytype happy
	-$(RM) -r .pytype pykythe/.pytype
	-cd pykythe && time pytype --protocols --exclude=bootstrap_builtins.py .
	cd pykythe && \
	for i in $$(ls *.py | fgrep -v bootstrap_builtins.py); do \
	    set -x; \
	    time pyxref --protocols --python_version=3.7 --imports_info=$$(pwd)/.pytype/imports/pykythe.pykythe.$$(basename $$i .py).imports $$i >/tmp/$$(basename $$i .py).pyxref.json;  \
	done
	@# if there are problems with above, add --debug option >/tmp/$$(basename $$i .py).pyxref.debug
	@# TODO: modify $(MAKE) make-tables to use /tmp/*.pyxref.json

	@ # TODO: use parameterized $(MAKE) make-tables
	$(RM) -r /tmp/pytype-graphstore /tmp/pytype-tables
	mkdir -p /tmp/pytype-graphstore /tmp/pytype-tables
	set -o pipefail; \
	    cat /tmp/*.pyxref.json | \
	    $(ENTRYSTREAM_EXE) --read_format=json | \
	    $(WRITE_ENTRIES_EXE) -graphstore /tmp/pytype-graphstore
	$(WRITE_TABLES_EXE) -graphstore=/tmp/pytype-graphstore -out=/tmp/pytype-tables
	# $(MAKE) run-kythe-server-pytype


run-pytype-server:
	@# This is wrong: -listen=localhost:$(BROWSE_PORT_PYKYTHE)
	$(HTTP_SERVER_EXE) -serving_table=/tmp/pytype-tables \
	  -public_resources=$(HTTP_SERVER_RESOURCES) \
	  -listen=:$(BROWSE_PORT_PYTYPE)

# $(PYTYPE_DIR)/%.pyi: %.py
# 	@mkdir -p $(dir $@)
# 	-$(PYTYPE) -V$(PYTYPE_V) -P $(PYTYPE_DIR) $< -o $@

# $(PYTYPE_DIR)/pykythe/ast.pyi: $(addprefix $(PYTYPE_DIR)/pykythe/,\
# 	pod.pyi)
# $(PYTYPE_DIR)/pykythe/ast_cooked.pyi: $(addprefix $(PYTYPE_DIR)/pykythe/,\
# 	pod.pyi typing_debug.pyi ast.pyi)
# $(PYTYPE_DIR)/pykythe/ast_raw.pyi: $(addprefix $(PYTYPE_DIR)/pykythe/,\
# 	pod.pyi typing_debug.pyi ast_cooked.pyi)
# $(PYTYPE_DIR)/pykythe/__main__.pyi: $(addprefix $(PYTYPE_DIR)/pykythe/,\
# 	pod.pyi typing_debug.pyi ast_raw.pyi ast_cooked.pyi ast.pyi)

######### DO NOT SUBMIT -- underhood

# Run target add-index-pykythe before doing this, to
# set up the serving tables.

# See
#   ../underhood/production/underhood/underhood_image.nix
#   ../underhood/treetide/underhood/ui/webpack.config.js
# https://github.com/TreeTide/underhood

run-underhood-frontend:
	@# http://localhost:9000 (see ../underhood/treetide/underhood/ui/webpack.config.js)
	@# TODO: --port 8081 --kythe_api_host=localhost --kythe_api_port=$(BROWSE_PORT_PYKYTHE)
	cd ../underhood && \
	nix-shell --run 'bazel run -c opt //treetide/underhood/frontend_server'
	nix-shell --run 'bazel shutdown'

run-underhood-ui:
	@# If this fails, try:
	@#   cd ../underhood/treetide/underhood/ui && nix-shell --run 'npm ci'
	@# and if 'npm ci' fails, try 'npm i'
	@# See also treetide/underhood/ui/webpack.config.js for things such
	@# as the port.
	@# See also treetide/underhood/ui/src/App.vue for highlight-mode
	@# (search for regexp '\bgo\b'):
	@#   highlight-mode="go"
	@#   codemirror/mode/go/go/js
	@#   mode: 'go'
	cd ../underhood/treetide/underhood/ui && \
	nix-shell --run 'npm run-script start:dev'
	@# View UI at http://localhost:9000

run-underhood-all:
	@# http://localhost:9000 (see run-underhood-frontend)
	@echo Suggest running the following commands in separate terminals, for easier cleanup.
	@echo $(MAKE) add-index-pykythe
	@echo $(MAKE) run-kythe-server
	@echo $(MAKE) run-underhood-frontend
	@echo $(MAKE) run-underhood-ui
	@exit 1
	@# Or you can do the following, which also finds the PIDs so
	@# that you can kill them.
	$(MAKE) add-index-pykythe
	$(MAKE) run-kythe-server &
	$(MAKE) run-underhood-frontend &
	sleep 5
	$(MAKE) run-underhood-ui &
	ps auwwwxxx|grep webpack; ps auwwwxxx|grep frontend; ps auwwwxxx|grep nix-shell


lint-logtalk:
	cat scripts/lint_logtalk.pl | swipl

# See https://github.com/SWI-Prolog/swipl/blob/master/CMAKE.md
#     https://www.swi-prolog.org/build/guidelines.html
.PHONY: build-swipl build-swipl-full
build-swipl: $(SWIPL_EXE_DEVEL)
$(SWIPL_EXE_DEVEL): $(SWIPL_SRC)/*.[ch]
	@# TODO: need a more complete set of source files
	cd swipl-devel && \
		mkdir -p build && \
		cd build && \
		cmake -G Ninja .. && \
		../script/pgo-compile.sh \
		ninja

build-swipl-full:
	cd swipl-devel && \
		rm -rf build && \
		mkdir -p build && \
		cd build && \
		cmake -G Ninja .. && \
		../script/pgo-compile.sh \
		ninja && \
		ctest -j 8

unused-predicates:
	cd pykythe && echo 'xref_source(pykythe, [silent(true)]). xref_defined(_, Goal, _), \+ xref_called(_, Goal, _), writeln(Goal), fail ; true.' | $(SWIPL_EXE)

upgrade-mypy:
	python3 -m pip install -U git+git://github.com/python/mypy.git

upgrade-pkgs:
	sudo apt update
	sudo apt autoremove
	sudo apt --with-new-pkgs --assume-yes upgrade
	@# And an old incantation from decades ago
	sudo sync
	sudo sync
	sudo sync
	sudo time fstrim --all -v

pull-swipl:
	cd ../swipl-devel && git pull --recurse

upgrade-swipl:
	@# See ../swipl-devel/CMAKE.md
	cd ../swipl-devel && \
		git pull --recurse && \
		git submodule update && \
		mkdir -p build && \
		cd build && \
		cmake -G Ninja .. && \
		ninja && \
		ctest -j 8
	@# ninja install

upgrade-swipl-full:
	cd ../swipl-devel && \
		git pull --recurse && \
		git submodule update && \
		rm -rf build && \
		mkdir  build && \
		cd     build && \
		cmake -G Ninja .. && \
		ninja && ctest -j 8
	@# ninja install

swipl-sanitize:
	@# in cmake/BUildType.cmake:
	@# add -DALLOC_DEBUG=1 to set(CMAKE_C_FLAGS_SANITIZE ... )
	cd ../swipl-devel && \
		rm -rf build.sanitize && \
		mkdir  build.sanitize && \
		cd     build.sanitize && \
		cmake -DCMAKE_BUILD_TYPE=Sanitize -G Ninja .. && \
		ninja && ctest -j 8

upgrade-emacs:
	@# https://www.emacswiki.org/emacs/EmacsSnapshotAndDebian
	@# The "git clean -dxf" probably isn't needed, but it's safe
	cd ../emacs && \
		git clean -dxf && \
		./autogen.sh && \
		./configure --prefix=$$HOME/.local && \
		make bootstrap install

rsync-backup:
	rsync -va --delete -e ssh 192.168.1.79:src/pykythe /home/peter/src_backup/

# ============== examples =================

# time make  --warn-undefined-variables -C ~/src/pykythe clean etags show-vars test make-tables json-decoded-all # test_python_lib # test_single_src

# find /tmp/pykythe_test -name '[ait]*' -o -name 'simple*' -delete; time make -k --warn-undefined-variables -C ~/src/pykythe BATCH_ID=bbb etags show-vars test_single_src # test  make-json make-json-pretty make-tables json-decoded-all #  # test_single_src

# make -C ~/src/pykythe clean etags test test_python_lib
# make -C ~/src/pykythe clean_lite etags test

##### - to find TODOs and related:
# find $(ls | grep -v typeshed)  -type f  ! -name '*~' -print|sort | xargs egrep -nH 'DO|NOT|SUBMIT|#- [ {]*{//|TODO|#-.*\?'
