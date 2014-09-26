################################################################################
# Reporting
################################################################################

.PHONY: report
report: $(BUILD_DIR)/report.html $(BUILD_DIR)/testsuite/summary.txt
CLEAN_SUB_TARGETS += clean_report
clean_report:
	@$(ECHO) "Cleaning build report $(BUILD_DIR)/report.html ..."
	rm -f $(BUILD_DIR)/report.html

EXPECTED_BUILD_PRODUCTS = $(DEPLOY_DIR)/zompc $(DEPLOY_DIR)/zompsh $(DEPLOY_DIR)/vm_http_server \
    $(MLTEST) $(MAKE_REPORT) $(MAKE_HISTORY_REPORT) $(CHECK_TEST)

MAKE_REPORT = $(OUT_DIR)/testsuite/make_report
ALL_TARGETS += $(MAKE_REPORT)
FILES_TO_DELETE_ON_CLEAN += testsuite/make_report{.cmx,.cmi,.cmo,.o,}
$(MAKE_REPORT): CAML_LIBS += str

MAKE_VERSION_STRING="git-`git rev-parse --short HEAD``if ! git diff-index --quiet HEAD; then echo \\-modified; fi`"

.PHONY: $(BUILD_DIR)/report.html
$(BUILD_DIR)/report.html: $(MAKE_REPORT)
	@$(ECHO) "Creating test report ..."
	cat testsuite/report_head.html > $@
	echo "Report generated at `date \"+%Y-%m-%d %H:%M:%S\"`<br />" >> $@
	-echo "Version $(MAKE_VERSION_STRING) <br />" >> $@
	echo "Build variant = $(BUILD_VARIANT) <br />" >> $@
	echo "<br />" >> $@
	echo "Documentation: <a href=\"intermediate/caml-modules.svg\">OCaml modules deps</a>, <a href=\"intermediate/caml-types.svg\">types</a>, <a href=\"doc/index.html\">documentation</a><br />" >> $@
	$(MAKE_REPORT) "Unit tests" $(filter-out testsuite/check_test_verify/%, $(sort $(TESTSUITE_CASES:.testreport=))) >> $@
	./libs/make_libs_result_files.sh $(ZOMP_LIBS_SRC)
	$(MAKE_REPORT) "Libraries" $(sort $(ZOMP_LIBS_SRC:.zomp=)) >> $@
	./examples/make_examples_result_files.sh $(EXAMPLES_SOURCES)
	$(MAKE_REPORT) "Examples" $(sort $(EXAMPLES_SOURCES:.zomp=)) >> $@
	echo "<h2>OCaml unit tests</h2>" >> $@
	echo "<a href=\"../../$(MLTEST_OUTPUT_FILE)\">Output</a>\n" >> $@
	echo "<p><span style=\"font-family:monospace\">\n" >> $@
	(cat $(MLTEST_SUMMARY_FILE) 2>/dev/null || echo "File <span class=\"failed\">$(MLTEST_SUMMARY_FILE)</span> does not exist, mltests have not been run<br />") >> $@
	BEGIN_ERROR="<span class=\"failed\">" END_ERROR="</span>" NEWLINE="<br />" ./source/build/check_build_products.sh $(ARCH) $(EXPECTED_BUILD_PRODUCTS) >> $@
	echo "</span></p>\n" >> $@
	echo "</body>\n</html>" >> $@

MAKE_HISTORY_REPORT = $(OUT_DIR)/testsuite/make_history_report
ALL_TARGETS += $(MAKE_HISTORY_REPORT)
FILES_TO_DELETE_ON_CLEAN += testsuite/make_history_report{.cmx,.cmi,.cmo,.o,}
$(MAKE_HISTORY_REPORT): CAML_OBJS += source/common
$(MAKE_HISTORY_REPORT): CAML_LIBS += str bigarray

print_ci_stats: $(MAKE_HISTORY_REPORT)
	 ./testsuite/for_each_ci_run.sh $(MAKE_HISTORY_REPORT)

