
let withFileForWriting fileName f =
  let channel = open_out fileName in
  let result =
    try
      f channel
    with exn ->
      close_out channel;
      raise exn
  in
  close_out channel;
  result

let newlineRE = Str.regexp_string "\n"
let failedRE = Str.regexp_string "failed"
let succeededRE = Str.regexp_string "succeeded"

let runTests summaryFile =
  Indentlexer.runInternalTests();

  let tests = [
    Indentlexer_tests.runTerminatorTests;
    Indentlexer_tests.runTests;
    Newparser_tests.runTests;
  ] in

  let call f = f() in
  let summaries = List.map call tests in
  print_newline();

  let module S = Testing.Summary in
  withFileForWriting summaryFile (fun file ->
    let print sum =
      let summary = sum.S.summary in
      Printf.printf "%s" summary;
      let (>>=) x f = f x in
      let htmlSummary =
        summary
        >>= Str.global_replace newlineRE "<br />\n"
        >>= Str.global_replace failedRE "<span class=\"failed\">failed</span>"
        >>= Str.global_replace succeededRE "<span class=\"ok\">succeeded</span>"
      in
      Printf.fprintf file "%s" htmlSummary
    in
    List.iter print summaries);

  let testFailed sum = sum.Testing.Summary.succeeded = false in
  let hadError = List.exists testFailed summaries in
  exit (if hadError then 1 else 0)

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.fprintf stderr
      "Expected one argument specifying the file name of a summary file";
    exit 2
  end else
    let summaryFile = Sys.argv.(1) in
    runTests summaryFile

