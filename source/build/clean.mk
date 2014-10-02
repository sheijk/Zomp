#
# Cleaning
#

CLEAN_SUB_TARGETS += source/clean
.PHONY: source/clean
source/clean:
	$(DELETE_FILE) source/zomp_shell.o $(ZOMPSH_FILE)
	$(DELETE_FILE) source/runtime.bc source/runtime.ll source/runtime.o
	$(DELETE_FILE) source/machine.{cmi,cmo,cmx,ml,mli} source/machine_stubs.{c,o}
	$(DELETE_FILE) source/stats.{cmi,cmo,cmx,ml,mli} source/stats_stubs.{c,o} source/stats_impl.o
	$(DELETE_FILE) $(ZOMP_DLL_FILE) source/libzompvm.a
	$(DELETE_FILE) source/zompvm_impl.o source/zompvm_dummy.o
	$(DELETE_FILE) source/zompvm_caml.o source/zompvm_caml_dummy.o
	$(DELETE_FILE) source/*_flymake.*
	$(DELETE_FILE) source/indentlexer.{cmi,cmo,cma,cmx,o}
	$(DELETE_FILE) source/newparser.{cmi,cmo,o,ml,mli,conflicts}
	$(DELETE_FILE) source/newparser_tests.{cmi,cmo,o} source/newparser_tests
	$(DELETE_FILE) source/expandertests.cm?
	$(DELETE_FILE) source/vm_http_server.o source/mongoose.o source/vm_server.o source/vm_protocol.o
	$(DELETE_FILE) source/dllzompvm.so

.PHONY: clean
clean: $(CLEAN_SUB_TARGETS)
	@$(ECHO) "Cleaning ..."
	cd tests && make clean_tests
	@$(DELETE_FILE) $(FILES_TO_DELETE_ON_CLEAN)
	@$(DELETE_FILE) $(foreach f,$(LANG_CMOS),${f:.cmo=.cm?})
	@$(DELETE_FILE) $(foreach f,$(LANG_CMOS),${f:.cmo=.o})
	$(DELETE_FILE) expander_tests.cm?
	$(DELETE_FILE) *_flymake.*
	$(DELETE_FILE) $(AUTO_DEPENDENCY_FILE)
	$(DELETE_FILE) libs/glQuickText.o libs/libquicktext.dylib libs/libglut.dylib
	$(DELETE_FILE) perflog.txt
	$(DELETE_FILE) libs/libutils.dylib
	$(DELETE_FILE) gmon.out
	$(DELETE_FILE) $(DEPLOY_DIR)/vm_http_server
	$(DELETE_FILE) $(MLTEST_SUMMARY_FILE) $(MLTEST_OUTPUT_FILE)
	$(DELETE_FILE) $(BUILD_DIR)/.exists
	$(DELETE_DIR) -rdf $(OUT_DIR) $(DEPLOY_DIR) $(TESTSUITE_OUT_DIR)

CLEAN_SUB_TARGETS += clean_doc
.PHONY: clean_doc
clean_doc:
	rm -rf $(CAML_DOC_DIR)

.PHONY: clean_tags
clean_tags:
	$(DELETE_FILE) source/*.annot source/*.cmt source/*.cmti
	$(DELETE_FILE) source/*.conflicts
	$(DELETE_FILE) testsuite/*.annot testsuite/*.cmt testsuite/*.cmti
	$(DELETE_FILE) $(FLYMAKE_LOG)

clean_all: clean clean_tags

