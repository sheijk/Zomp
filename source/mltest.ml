
let () =
  Indentlexer.runInternalTests();
  Indentlexer_tests.runTerminatorTests();
  Indentlexer_tests.runTests();
  Newparser_tests.runTests();
  ()

