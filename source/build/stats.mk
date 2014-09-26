################################################################################
# Git statistics
################################################################################

GITSTATS = $(ZOMP_TOOL_PATH)/../gitstats/gitstats
.PHONY: git_repo_stats
git_repo_stats:
	@$(ECHO) "Creating git repository statistincs ..."
	$(GITSTATS) . build/git-statistics
	@$(ECHO) "Open build/git-statistics/index.html to see git repository statistics"

################################################################################
# Count lines of code
################################################################################

CLOC_LANG_DEF_FILE = $(OUT_DIR)/cloc-lang-defs.txt

$(CLOC_LANG_DEF_FILE): source/build/cloc-zomp-def.txt source/build/cloc-glsl-def.txt makefile
	@$(ECHO) Creating cloc language definition file ...
	cloc --write-lang-def=$@
	cat source/build/cloc-zomp-def.txt >> $@
	cat source/build/cloc-glsl-def.txt >> $@

build/cloc.txt: $(CLOC_LANG_DEF_FILE)
	@$(ECHO) Creating lines of code statistics in $@ ...
	cloc --read-lang-def=$(CLOC_LANG_DEF_FILE) --exclude-dir=tools,build,data --report-file=$@ .

################################################################################
# Out of date, TODO: fix and add to test suite
################################################################################

# PROF_COMP_TARGET=metaballs
# 
# .PHONY: profile_comp
# profile_comp: $(ZOMPC_FILE) source/runtime.bc libs/opengl20.zomp libs/glfw.zomp
# 	cd examples && $(RM) -f $(PROF_COMP_TARGET).ll $(PROF_COMP_TARGET).bc
# 	cd examples && time make $(PROF_COMP_TARGET).ll $(PROF_COMP_TARGET).bc ZOMPCFLAGS=--print-timings
# 
# .PHONY: runtests
# runtests: $(LANG_CMOS)
# 	@$(ECHO) Running tests ...
# 	cd tests && time make clean_tests check
# 
# # FUNCTION_COUNTS=10 1000
# PERFTEST_GEN=
# # PERFTEST_GEN=_iexpr
# FUNCTION_COUNTS=100 1000 2000 3000 4000 5000 6000 7000 8000
# 
# .PHONY: perftest
# perftest: $(ZOMPC_FILE)
# 	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="$(FUNCTION_COUNTS)" PERFTEST_GEN=genperftest$(PERFTEST_GEN)
# 	gnuplot makeperfgraph.gnuplot || $(ECHO) "Could not execute gnuplot"
# 	mv temp.png perf_results$(PERFTEST_GEN).png
# 
# .PHONY: perftest2
# perftest2: $(ZOMPC_FILE)
# 	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="$(FUNCTION_COUNTS)" PERFTEST_GEN=genperftest
# 	$(CP) tests/timing.txt tests/timing_sexpr.txt
# 	cd tests && make clean_tests compileperftest FUNCTION_COUNTS="$(FUNCTION_COUNTS)" PERFTEST_GEN=genperftest_iexpr
# 	$(CP) tests/timing.txt tests/timing_iexpr.txt
# 	gnuplot makeperfgraph2.gnuplot || $(ECHO) "Could not execute gnuplot"

################################################################################
# Outdated line of code statistics. Does not work anymore, kept for reference
################################################################################

ML_SRC_FILES = $(wildcard source/*.ml) $(wildcard source/*.mli) $(wildcard source/*.mll) $(wildcard source/*.mly)

.PHONY: loc_stats_no_summary
loc_stats_no_summary:
	$(LS) $(wildcard source/*.ml source/*.mli source/*.mly source/*.mll) | grep -v source/newparser.ml | xargs $(LINE_COUNT) | $(SORT) -n
	$(LINE_COUNT) $(wildcard *.mk) makefile | $(SORT) -n
	$(LINE_COUNT) libs/libutils.cpp source/prelude.zomp source/runtime.c zomp.el source/zomputils.h source/zompvm_impl.cpp source/zompvm_impl.h source/zompvm_dummy.cpp | $(SORT) -n
	$(LINE_COUNT) $(wildcard libs/*.skel) | $(SORT) -n
	$(LS) $(wildcard libs/*.zomp) | grep -v libs/opengl20.\*\.zomp | grep -v libs/glfw\.zomp | grep -v libs/quicktext\.zomp | grep -v libs/glut.zomp | xargs $(LINE_COUNT) | $(SORT) -n
	$(LINE_COUNT) $(wildcard examples/*.zomp) | $(SORT) -n
	$(LINE_COUNT) $(wildcard testsuite/*.zomp) | $(SORT) -n
	$(LS) $(wildcard tests/*.zomp) | grep -v sharkperf.zomp | xargs $(LINE_COUNT) | $(SORT) -n

.PHONY: loc_stats
loc_stats: loc_stats_no_summary
	make -ks loc_stats_no_summary | grep total | awk '{ sum = sum + $$1; } END { print sum " total lines of code "; }'

