
let () =
  Indentlexer.runInternalTests();

  let tests = [
    Indentlexer_tests.runTerminatorTests;
    Indentlexer_tests.runTests;
    Newparser_tests.runTests;
  ] in

  let call f = f() in
  let summaries = List.map call tests in
  List.iter (fun sum -> sum.Testing.Summary.print stdout) summaries;

  let testFailed sum = sum.Testing.Summary.succeeded = false in
  let hadError = List.exists testFailed summaries in
  exit (if hadError then 1 else 0)


