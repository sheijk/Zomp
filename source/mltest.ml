(** Unit tests for OCaml modules.*)

let newlineRE = Str.regexp_string "\n"
let failedRE = Str.regexp_string "failed"
let succeededRE = Str.regexp_string "succeeded"

module BasicsParseDiagnosticsTest : Testing.CASE_STRUCT =
struct
  open Basics

  type input = string
  type output = (Basics.location * DiagnosticKind.t * string) option
  type result = [ `Return of output | `Exception of string ]

  let outputEqual l r = l = r

  let outputToString = function
    | Some (loc, kind, msg) ->
      formatDiagnostics kind loc msg
    | None ->
      "None"

  let inputToString str = str

  let testedFunc = parseDiagnostics

  let testCases =
    let module Diag = DiagnosticKind in
    [
      "file.zomp:10:5: error: omg an error with column",
      `Return (Some ({ fileName = "file.zomp"; line = 10; column = Some 5 },
                     Diag.Error,
                     "omg an error with column"));
      "file.zomp:1: warning: omg a warning without column",
      `Return (Some ({ fileName = "file.zomp"; line = 1; column = None },
                     Diag.Warning,
                     "omg a warning without column"));
      "file.zomp:1: foobar: omg a foobar without column",
      `Return None;
      "something completely different",
      `Return None;
      "testsuite/fundamental/test_type_errors.zomp:29: error: type error: lala",
      `Return (Some ({ fileName = "testsuite/fundamental/test_type_errors.zomp";
                       line = 29;
                       column = None },
                     Diag.Error,
                     "type error: lala"))
    ]

end

let runTests summaryFile =
  Indentlexer.runInternalTests();

  let tests = [
    Indentlexer_tests.runTerminatorTests;
    Indentlexer_tests.runTests;
    Newparser_tests.runTests;

    fun () ->
      let module M = Testing.Tester(BasicsParseDiagnosticsTest) in
      M.runTestsAndReport "Basics.parseDiagnostics";
  ] in

  let call f = f() in
  let summaries = List.map call tests in
  print_newline();

  Common.withFileForWriting summaryFile (fun file ->
    let print sum =
      let module S = Testing.Summary in
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

module Ast2Test = struct
  open Ast2
  open Printf

  (* Sorry. Need to go shopping for nicer string literal syntax extension. *)
  let referenceOutput = ""
    ^ "foo @testfile.zomp:1\n"
    ^ "----------\n"
    ^ "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx @testfile.zomp:5\n"
    ^ "----------\n"
    ^ "(short @testfile.zomp:10\n"
    ^ "  a\n"
    ^ "  b)\n"
    ^ "----------\n"
    ^ "(simple @testfile.zomp:1\n"
    ^ "  arg0\n"
    ^ "  arg1\n"
    ^ "  arg2\n"
    ^ "  arg3)\n"
    ^ "----------\n"
    ^ "(opseq @testfile.zomp:10\n"
    ^ "  (foo\n"
    ^ "    bar\n"
    ^ "    baz\n"
    ^ "    buzz\n"
    ^ "    long_argument_soup)\n"
    ^ "  (abcd @otherfile.zomp:11\n"
    ^ "    0123)\n"
    ^ "  lalal @:12)\n"
    ^ "----------\n"
    ^ "(x0123456789 @testfile.zomp:1\n"
    ^ "  a\n"
    ^ "  b\n"
    ^ "  c)\n"
    ^ "----------\n"
    ^ "(foobar @testfile.zomp:3\n"
    ^ "  (opcall @:4\n"
    ^ "    f!\n"
    ^ "    x!)\n"
    ^ "  (opseq\n"
    ^ "    (f2 @:5\n"
    ^ "      a\n"
    ^ "      b\n"
    ^ "      c)))\n"
    ^ "----------\n"
    ^ "(if @testfile.zomp:1\n"
    ^ "  (op>\n"
    ^ "    x\n"
    ^ "    (op*\n"
    ^ "      count\n"
    ^ "      10))\n"
    ^ "  then\n"
    ^ "  (opseq\n"
    ^ "    (println @:2\n"
    ^ "      'hello')\n"
    ^ "    (op= @:3\n"
    ^ "      x\n"
    ^ "      0))\n"
    ^ "  else @:4\n"
    ^ "  (opseq\n"
    ^ "    (opcall @:5\n"
    ^ "      abort)))\n"
    ^ "---\n"
    ^ "foo @testfile.zomp:1\n"
    ^ "--------------------\n"
    ^ "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx @testfile.zomp:5\n"
    ^ "--------------------\n"
    ^ "(short @testfile.zomp:10\n"
    ^ "  a\n"
    ^ "  b)\n"
    ^ "--------------------\n"
    ^ "(simple @testfile.zomp:1\n"
    ^ "  arg0\n"
    ^ "  arg1\n"
    ^ "  arg2\n"
    ^ "  arg3)\n"
    ^ "--------------------\n"
    ^ "(opseq @testfile.zomp:10\n"
    ^ "  (foo\n"
    ^ "    bar\n"
    ^ "    baz\n"
    ^ "    buzz\n"
    ^ "    long_argument_soup)\n"
    ^ "  (abcd @otherfile.zomp:11\n"
    ^ "    0123)\n"
    ^ "  lalal @:12)\n"
    ^ "--------------------\n"
    ^ "(x0123456789 @testfile.zomp:1\n"
    ^ "  a\n"
    ^ "  b\n"
    ^ "  c)\n"
    ^ "--------------------\n"
    ^ "(foobar @testfile.zomp:3\n"
    ^ "  (opcall @:4 f! x!)\n"
    ^ "  (opseq\n"
    ^ "    (f2 @:5 a b c)))\n"
    ^ "--------------------\n"
    ^ "(if @testfile.zomp:1\n"
    ^ "  (op>\n"
    ^ "    x\n"
    ^ "    (op* count 10))\n"
    ^ "  then\n"
    ^ "  (opseq\n"
    ^ "    (println @:2\n"
    ^ "      'hello')\n"
    ^ "    (op= @:3 x 0))\n"
    ^ "  else @:4\n"
    ^ "  (opseq\n"
    ^ "    (opcall @:5 abort)))"

  let loc = Basics.location "mltest" 0 None

  let idExpr id = Ast2.idExprLoc loc id

  let run() =
    let testWithMaxLength maxLength =
      let seperatorString = "\n" ^ String.make maxLength '-' ^ "\n" in
      let column = None in
      let l line = { Basics.fileName = "testfile.zomp"; line; column } in
      let l2 line = { Basics.fileName = "otherfile.zomp"; line; column } in
      let exprs =
        [idExprLoc (l 1) "foo";
         idExprLoc (l 5) "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
         simpleExprLoc (l 10) "short" ["a"; "b"];
         simpleExprLoc (l 1) "simple" ["arg0"; "arg1"; "arg2"; "arg3"];
         opseqExprLoc (l 10)
           [simpleExprLoc (l 10) "foo" ["bar"; "baz"; "buzz"; "long_argument_soup"];
            simpleExprLoc (l2 11) "abcd" ["0123"];
            idExprLoc (l2 12) "lalal"];
         simpleExprLoc (l 1) "x0123456789" ["a"; "b"; "c"];
         exprLoc (l 3) "foobar" [
           callExprLoc (l 4) [idExpr "f"; idExpr "x"];
           opseqExprLoc (l 4) [simpleExprLoc (l 5) "f2" ["a"; "b"; "c"]]];
         exprLoc (l 1) "if" [
           exprLoc (l 1) "op>" [idExprLoc (l 1) "x"; simpleExprLoc (l 1) "op*" ["count"; "10"]];
           idExprLoc (l 1) "then";
           opseqExprLoc (l 1) [
             simpleExprLoc (l 2) "println" ["'hello'"];
             simpleExprLoc (l 3) "op=" ["x"; "0"]];
           idExprLoc (l 4) "else";
           opseqExprLoc (l 4) [
             simpleExprLoc (l 5) "opcall" ["abort"]]];
        ]
      in
      let strings = List.map (fun e -> stToString maxLength (toStringTree e)) exprs in
      Common.combine seperatorString strings
    in
    let runOutputs = List.map testWithMaxLength [10; 20] in
    let output = Common.combine "\n---\n" runOutputs in
    printf "\nTesting Ast2 pretty printer\n%s\n" output;
    if output = referenceOutput then begin
      printf "Result ok\n";
    end else begin
      printf "Result is different from reference\n";
      let fileReference = Filename.temp_file "ast2_test-" "-reference"
      and fileOutput = Filename.temp_file "ast2_test-" "-output"
      and fileDiffOutput = Filename.temp_file "ast2_test-" "-diff_output"
      in
      let writeToFile ~file string =
        Common.withFileForWriting file (fun out -> output_string out string)
      in
      writeToFile ~file:fileReference referenceOutput;
      writeToFile ~file:fileOutput output;
      let runCommand cmd =
        match Sys.command cmd with
          | 0 -> ()
          | errorCode ->
            printf "returned %d\n" errorCode;
      in
      runCommand (sprintf "diff %s %s > %s" fileReference fileOutput fileDiffOutput);
      printf "Diff output:\n%s\n" (Common.readFile fileDiffOutput)
    end

end

module CommonTests = struct
  open Common

  let testCanonicalFileName() =
    let cases =
      let unchanged path = path, path in
      [
        unchanged "";
        unchanged "/foo/bar/buzz.jpg";
        "/foo/bar/../buzz.jpg", "/foo/buzz.jpg";
        unchanged "/etc/zomp";
        "/etc/zomp/stuff/..", "/etc/zomp";
        "/etc/zomp/../stuff", "/etc/stuff";
        "C:\\apps\\stuff", "C:/apps/stuff";
        unchanged "C:/apps/stuff";
        "/foo/bar/.", "/foo/bar";
        "/foo/./bar", "/foo/bar";
        "/foo//etc", "/foo/etc";
        "/foo/./..", "/";
        "/a/b/.././../c", "/c";
      ]
    in
    let handle (input, expected) =
      let result = canonicalFileName input in
      if result <> expected then begin
        Printf.printf "error: for %s got %s but expected %s\n" input result expected;
      end
    in
    Printf.printf "Testing canonicalFileName\n";
    List.iter handle cases

  let run() =
    testCanonicalFileName()
end

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.fprintf stderr
      "Expected one argument specifying the file name of a summary file";
    exit 2
  end else
    let summaryFile = Common.absolutePath Sys.argv.(1) in
    Printf.printf "Writing mltest report into %s\n" summaryFile;
    flush stdout;
    CommonTests.run();
    Ast2Test.run();
    runTests summaryFile

