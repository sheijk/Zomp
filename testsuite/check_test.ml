(**
Tool that is used to run the testsuite integration tests. Call it using
'check_test foo.testresult make-command' to run the tests in foo.zomp.
make-command is a command used to invoke make, it should be "$(MAKE)" if you
call this from a makefile. The tool will call '$make-command foo.exe' to
build the executable and '$make-command foo.test_output' to run the test.
*)

open Printf
open Basics

let scriptName = "check_test"

let testOutputExt = "test_output"
let compilationOutputExt = "compile_output"
let reportOutputExt = "testreport"
let statsExt = "compile_stats"

type usageError =
  | InvalidArguments
  | CompilerFailed of int

let failWith error =
  let errorCode, msg =
    match error with
      | InvalidArguments -> 1,
          sprintf "Expecting %s zomp-file" scriptName
      | CompilerFailed n -> 2, sprintf "Compilation failed with %d" n
  in
  fprintf stderr "%s\n" msg;
  flush stderr;
  exit errorCode

module Utils =
struct
  let withOpenFileIn fileName f =
    let stream = open_in fileName in
    try
      let result = f stream in
      close_in stream;
      result
    with exn ->
      close_in stream;
      raise exn

  let withOpenFileOut fileName f =
    let stream = open_out fileName in
    try
      f stream;
      close_out stream
    with exn ->
      close_out stream;
      raise exn

  let printFile filename =
    let rec printStream s =
      let line = input_line s in
      print_string line;
      print_newline();
      printStream s
    in
    try
      withOpenFileIn filename printStream
    with End_of_file ->
      ()

  let readFile fileName =
    withOpenFileIn fileName Common.readChannel

  let forEachLineInFile fileName f =
    let rec read lineNum stream =
      try
        let line = input_line stream in
        let () = f lineNum line in
        read (lineNum+1) stream
      with End_of_file -> ()
    in
    withOpenFileIn fileName (read 1)

  let readOneLineFileIfPresent fileName =
    try
      withOpenFileIn fileName (fun stream ->
        let line = input_line stream in
        Some line)
    with End_of_file | Sys_error _ ->
      None

  let replaceExtension fileName ext =
    let noExt =
      try
        Filename.chop_extension fileName
      with Invalid_argument _ ->
        fileName
    in
    sprintf "%s.%s" noExt ext

  let (=~) str re = Str.string_match (Str.regexp re) str 0

  let stringToList str =
    let strLength = String.length str in
    let rec loop lst i =
      if i >= strLength then lst
      else (String.unsafe_get str i) :: loop lst (i+1)
    in
    loop [] 0

  let formatQuantity count singular =
    sprintf "%d %s" count (if count = 1 then singular else singular ^ "s")

  let addToList item refToList = refToList := item :: !refToList

  (** Returns a string enumerating all strings in last separated by strings. The
      last element is separated by verbForLast. Like "a, b, c and d". *)
  let verbalConcat verbForLast list =
    match List.rev list with
      | [] -> ""
      | [single] -> single
      | last :: remaining ->
        sprintf "%s %s %s"
          (String.concat ", " (List.rev remaining))
          verbForLast
          last

  let timeStr time =
    sprintf "%d-%02d-%02d %02d:%02d:%02d"
      (time.Unix.tm_year + 1900) (time.Unix.tm_mon + 1) time.Unix.tm_mday
      time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec

  let withoutSuffix line suffix =
    let lineLength = String.length line in
    if (lineLength > 0) && (line.[lineLength-1] = suffix ) then
      Str.string_before line (lineLength-1)
    else
      line

  let listIteri f start list =
    let rec iterWithIndex index = function
      | [] -> ()
      | hd :: tl ->
        let () = f index hd in
        iterWithIndex (index+1) tl
    in
    iterWithIndex start list

  let escapeHtmlText = Common.escapeHtmlText
end

open Utils

module ExpectationKind =
struct

  type t =
  (** Compiler output that looks like "name.zomp:10: error: message" or
      "name.zomp:10:3: error: message". The numbers are line and column.
      Using this once or more will result in compilation being expected to
      fail. Line and file name in the error diagnostics must be the same as
      the the file and line number where the expectation is put. *)
  | CompilerError
  (** Like compiler error but file and line can be anything. *)
  | CompilerErrorNoLoc
  (** Any compiler output. *)
  | CompilerOutput
  (** This particular compiler output won't appear. *)
  | NotCompilerOutput
  (** Will expect the compilation to fail. *)
  | CompilationWillFail
  (** Like CompilerError but with "warning" instead of error *)
  | CompilerWarning
  (** Like CompilerError but with "info" instead of error *)
  | CompilerInfo
  (** Expect the execution of the test to print a line matching this. *)
  | RuntimePrint
  (** Execution of the test case will exit with a specific exit code which is
      given as the single argument. If the number is prefixed with "!" any exit
      code but the given number will be expected. *)
  | TestCaseExitCode

  (** variant, command name, description
      When changing this, also update zomp-testsuite-directives in zomp.el *)
  let metadata = [
    CompilerError, "error", "compiler error";
    CompilerErrorNoLoc, "error-no-location", "compiler error w/o location";
    CompilerOutput, "compiler-output", "compiler output";
    NotCompilerOutput, "not-compiler-output", "compiler output not to happen";
    CompilationWillFail, "compilation-fails", "compilation to fail";
    CompilerWarning, "warning", "compiler warning";
    CompilerInfo, "info", "compiler info";
    RuntimePrint, "print", "test case to print line";
    TestCaseExitCode, "exit-code", "test case to exit with";]

  let parse str =
    try
      let (variant, _, _) = List.find (fun (_, cmd, _) -> cmd = str) metadata in
      variant
    with Not_found ->
      failwith "Expectation.parse"

  let validExpectationsEnumDescr =
    verbalConcat "or" (List.map (fun (_, cmd, _) -> cmd) metadata)

  let kindDescription command =
    let (_, _, description) = List.find (fun (cmd, _, _) -> cmd = command) metadata in
    description

  let description kind words =
    let quote = sprintf "\"%s\"" in
    (sprintf "%s containing word%s %s"
       (kindDescription kind)
       (match words with [_] -> "" | _ -> "s")
       (verbalConcat "and" (List.map quote words)))

  let locationDescription fileName lineNum kind =
    match kind with
      | CompilerErrorNoLoc
      | CompilerOutput
      | RuntimePrint ->
        "any-file:any-line"
      | _ ->
        sprintf "%s:%d" fileName lineNum
end

module Expectation =
struct
  type t = {
    kind : ExpectationKind.t;
    words : string list;
    line : int;
    found : bool ref;
  }
end

module Exit_code = struct
  type condition =
    | MustBe of int
    | MustNotBe of int

  let toString = function
    | MustBe exitCode -> sprintf "%d" exitCode
    | MustNotBe exitCode -> sprintf "anything other than %d" exitCode
end

let writeHtmlHeader outFile zompFileName =
  let monospace = "font-family", "monospace" in
  let lightLineLeft = [
    "border-left", "1px solid gray";
    "padding-left", "5px";
    "margin-left", "10px"]
  in
  let ulClass cssClass = [
    sprintf ".%s" cssClass, [monospace];
    sprintf ".%s ul" cssClass, [
      "padding-left", "20px";
      "margin-left", "5px";
      "list-style-type", "square"];
  ]
  in
  let inlineDiagnostic background color = [
    "color", "black";
    "background", background;
    "border", sprintf "1px solid %s" color;
    "border-radius", "3px";
    "-moz-border-radius", "3px";
    "padding-left", "5px";
    "padding-right", "5px";
    "margin-left", "30px";
  ] in
  let diagnostic color = [
    "color", color;
  ] in
  let cssElements = [
    "h1", ["margin-bottom", "0"];
    "h2", ["margin-bottom", "0"];
    ".ok", ["color", "green"];
    ".failed", ["color", "red"];
    ".inline-error", inlineDiagnostic "#ffe3e3" "#f00";
    ".compiler-error", diagnostic "#f00";
    ".inline-warning", inlineDiagnostic "#ffe4d9" "#c50";
    ".compiler-warning", diagnostic "#c50";
    ".inline-info", inlineDiagnostic "#EAF6F0" "#080";
    ".compiler-info", diagnostic "#080";
    ".console-output", monospace :: lightLineLeft;
    ".file-link", ["color", "gray"; "margin-bottom", "10px"; "font-size", "90%"];
    "a.file-link:link", ["color", "gray"];
    "a.file-link:visited", ["color", "#868"];
    "a.file-link:hover", ["color", "blue"];
  ] @ [
    ".source ol", [
      monospace;
      "color", "gray";
      "display", "inline-block"];
    ".source li", [
      "background", "#fff";
      "padding-left", "5px";
      "border-left", "1px solid gray"];
    ".source ol li code", [
      "color", "black"];
    ".source li:hover", [
      "background", "#eee"];

    ".source-comment", [
      "color", "#427548"];
    ".source-string", [
      "color", "#af8730"]
  ]
    @ ulClass "expectations"
    @ ulClass "results"
  in

  fprintf outFile "<!DOCTYPE html>\n";
  fprintf outFile "<html lang=\"en-US\">\n";
  fprintf outFile "  <head>\n";
  fprintf outFile "    <meta charset=\"utf-8\">\n";
  fprintf outFile "    <title>Report for %s</title>\n" zompFileName;
  fprintf outFile "    <style type=\"text/css\">\n";
  List.iter (fun ((path:string), attributes) ->
    let makeAttributeLine (name, value) =
      sprintf "%s: %s;" name value
    in
    let attributeLines = List.map makeAttributeLine attributes in
    fprintf outFile "      %s {\n        %s\n      }\n" path (Common.combine "\n        " attributeLines);
  ) cssElements;
  fprintf outFile "    </style>\n";
  fprintf outFile "  </head>\n";
  fprintf outFile "  <body>\n";
  ()

let inElements outFile elements ?cssClass f =
  let first = List.hd elements
  and remaining = List.tl elements
  in
  fprintf outFile "<%s%s>"
    first
    (match cssClass with None -> "" | Some name -> sprintf " class=\"%s\"" name);
  List.iter (fun element -> fprintf outFile "<%s>" element) remaining;
  fprintf outFile "\n";
  let closeElements() =
    List.iter (fun element -> fprintf outFile "</%s>" element) (List.rev remaining);
    fprintf outFile "</%s>\n" first;
  in
  try
    f();
    closeElements()
  with exn ->
    closeElements();
    raise exn
      
let writeHeader outFile n header =
  fprintf outFile "<h%d>%s</h%d>\n" n header n

let writeHeaderWithLink outFile n header target =
  fprintf outFile "<h%d>%s</h%d>\n" n header n;
  fprintf outFile "<div class=\"file-link\"><a class=\"file-link\" href=\"%s\">Raw file</a></div>\n" target

let addExpectation
    zompFileName expectedCompilationSuccess expectedErrorMessages expectedTestCaseExitCode
    kindStr args line =
  let reportWarning msg =
    printf "%s:%d: warning: %s\n" zompFileName line msg
  in
  try
    let kind = ExpectationKind.parse kindStr in
    let addExpectationForLine line shouldBeFound =
      addToList
        {
          Expectation.kind;
          words = args;
          line;
          found = ref (not shouldBeFound);
        }
        expectedErrorMessages
    in
    begin match kind with
      | ExpectationKind.CompilerError ->
        expectedCompilationSuccess := false;
        addExpectationForLine line true
      | ExpectationKind.CompilerErrorNoLoc ->
        expectedCompilationSuccess := false;
        addExpectationForLine 0 true
      | ExpectationKind.CompilerOutput ->
        addExpectationForLine 0 true
      | ExpectationKind.NotCompilerOutput ->
        addExpectationForLine 0 false
      | ExpectationKind.CompilationWillFail ->
        expectedCompilationSuccess := false
      | ExpectationKind.CompilerWarning
      | ExpectationKind.CompilerInfo
      | ExpectationKind.RuntimePrint ->
        addExpectationForLine line true
      | ExpectationKind.TestCaseExitCode ->
        begin match args with
          | [lineString] ->
            begin
              try
                expectedTestCaseExitCode :=
                  if lineString.[0] = '!' then
                    Exit_code.MustNotBe (int_of_string (Str.string_after lineString 1))
                  else
                    Exit_code.MustBe (int_of_string lineString);
              with Failure "int_of_string" | Invalid_argument _ ->
                reportWarning "could not parse line number, expected number or !number"
            end
          | _ ->
            reportWarning "wrong number or arguments, expected 'line' number or '!line'"
        end
    end
  with Failure _ ->
    reportWarning
      (sprintf "invalid expectation kind '%s'. Expected %s"
         kindStr ExpectationKind.validExpectationsEnumDescr)

let collectExpectations addExpectation (lineNum :int) line =
  if line =~ ".*//// +\\([^ ]+\\) ?\\(.*\\)" then begin
    let kindStr = Str.matched_group 1 line in
    let args = Str.split (Str.regexp " +") (Str.matched_group 2 line) in
    addExpectation kindStr args lineNum
  end

let reportError outFile zompFileName errorCount tag ?line msg =
  let line = match line with Some l -> l | _ -> 0 in
  let msg = sprintf "%s:%d: error: %s" zompFileName line msg in
  begin match tag with
    | `Br ->
      fprintf outFile "%s<br />\n" msg
    | `Li ->
      fprintf outFile "  <li>%s</li>\n" msg
  end;
  fprintf stderr "%s\n" msg;
  incr errorCount

let parseDiagnostics zompFileName line =
  Common.applyIfSome
    (fun (location, kind, message as result) ->
      if location.fileName = zompFileName then
        Some result
      else
        None)
    (Basics.parseDiagnostics line)

let cssClassForInlineDiagnosticsKind = function
  | DiagnosticKind.Error -> "inline-error"
  | DiagnosticKind.Warning -> "inline-warning"
  | DiagnosticKind.Info
  | DiagnosticKind.Other _ -> "inline-info"

let cssClassForDiagnosticsKind = function
  | DiagnosticKind.Error -> "compiler-error"
  | DiagnosticKind.Warning -> "compiler-warning"
  | DiagnosticKind.Info
  | DiagnosticKind.Other _ -> "compiler-info"
    
let () =
  if Array.length Sys.argv != 4 then
    failWith InvalidArguments;

  let outputFileName = Sys.argv.(1) in
  let zompFileName = replaceExtension outputFileName "zomp" in

  let compileCommand = Sys.argv.(2) in
  let runTestCaseCommand = Sys.argv.(3) in

  let expectedErrorMessages = ref [] in
  let expectedCompilationSuccess = ref true in
  let expectedTestCaseExitCode = ref (Exit_code.MustBe 0) in
  let addExpectation =
    addExpectation zompFileName expectedCompilationSuccess expectedErrorMessages expectedTestCaseExitCode
  in
  let collectExpectations = collectExpectations addExpectation in

  withOpenFileOut outputFileName (fun outFile ->
    let errorCount = ref 0 in
    let reportError = reportError outFile zompFileName errorCount in

    let writeHeader = writeHeader outFile in
    let writeHeaderWithLink = writeHeaderWithLink outFile in
    let inElements = inElements outFile in

    let writeExpectation expectation =
      let { Expectation.kind; line = lineNum; words } = expectation in
      fprintf outFile "  <li>%s: expect %s</li>\n"
        (ExpectationKind.locationDescription zompFileName lineNum kind)
        (ExpectationKind.description kind words)
    in
    let checkExpectation message diagnosticLoc diagnosticKind expectation =
      let module E = Expectation in
      let containsWord word =
        Str.string_match (Str.regexp_case_fold (".*" ^ Str.quote word)) message 0
      in
      let locationMatches diagnosticLoc =
        diagnosticLoc.line = expectation.E.line
        && zompFileName = diagnosticLoc.fileName
      in
      let setIfAllWordsMatch value =
        if List.for_all containsWord expectation.E.words then
          expectation.E.found := value
      in

      let module Expect = ExpectationKind in
      let module Diag = DiagnosticKind in

      match expectation.E.kind, diagnosticKind, diagnosticLoc with
        | Expect.CompilerError, Diag.Error, Some diagnosticLoc
        | Expect.CompilerWarning, Diag.Warning, Some diagnosticLoc ->
          if locationMatches diagnosticLoc then begin
            setIfAllWordsMatch true
          end
        | Expect.CompilerErrorNoLoc, Diag.Error, Some _ ->
          setIfAllWordsMatch true
        | Expect.CompilerInfo, Diag.Info, Some diagnosticLoc ->
          if locationMatches diagnosticLoc then
            setIfAllWordsMatch true
        | Expect.CompilerOutput, _, _ ->
          setIfAllWordsMatch true
        | Expect.NotCompilerOutput, _, _ ->
          setIfAllWordsMatch false

        | Expect.CompilationWillFail, _, _
        | Expect.TestCaseExitCode, _, _
        | Expect.RuntimePrint, _, _
        | _, Diag.Other _, _
        | Expect.CompilerInfo, Diag.Info, _
        | Expect.CompilerInfo, (Diag.Warning | Diag.Error), _
        | Expect.CompilerWarning, (Diag.Info | Diag.Error), _
        | Expect.CompilerWarning, Diag.Warning, None
        | Expect.CompilerError, Diag.Error, None
        | Expect.CompilerError, (Diag.Warning | Diag.Info), _
        | Expect.CompilerErrorNoLoc, Diag.Error, None
        | Expect.CompilerErrorNoLoc, (Diag.Info | Diag.Warning), _
          -> ()
    in

    let diagnostics = ref [] in

    let checkCompilerExpectationsAndPrintLine _ line =
      match parseDiagnostics zompFileName line with
        | Some (loc, kind, message) ->
          if loc.fileName = zompFileName then begin
            diagnostics := (loc, kind, message) :: !diagnostics;
          end;
          let escapedText = sprintf "%s: <span class=\"%s\">%s</span>: %s<br />\n"
            (locationToString loc)
            (cssClassForDiagnosticsKind kind)
            (DiagnosticKind.toString kind)
            (escapeHtmlText message)
          in
          fprintf outFile "%s" escapedText;
          List.iter (checkExpectation message (Some loc) kind) !expectedErrorMessages
        | None ->
          fprintf outFile "%s<br />\n" (escapeHtmlText line);
          List.iter (checkExpectation line None (DiagnosticKind.Other "")) !expectedErrorMessages
    in

    let checkRuntimeExpectationsAndPrintLine _ line =
      fprintf outFile "%s<br />\n" (escapeHtmlText line);

      let checkPrintExpectation expectation =
        let module E = Expectation in
        let containsWord word =
          Str.string_match (Str.regexp (".*" ^ Str.quote word)) line 0
        in
        match expectation.E.kind with
          | ExpectationKind.RuntimePrint ->
            begin
              if List.for_all containsWord expectation.E.words then
                expectation.E.found := true
            end
          | _ -> ()
      in
      List.iter checkPrintExpectation !expectedErrorMessages
    in

    let reportIfMissing expectation =
      let { Expectation.kind; line; words; found } = expectation in
      if !found = false then
        reportError `Li ~line
          (sprintf "expected %s but didn't happen"
             (ExpectationKind.description kind words))
    in

    (** code *)

    writeHtmlHeader outFile zompFileName;
    writeHeaderWithLink 1 (sprintf "Test report for %s" zompFileName) (Filename.basename zompFileName);
    fprintf outFile "Executed at %s<br />\n"
      (timeStr (Unix.localtime (Unix.gettimeofday())));

    forEachLineInFile zompFileName collectExpectations;
    expectedErrorMessages := List.rev !expectedErrorMessages;

    writeHeader 2 "Expectations";
    inElements ["div"; "ul"] ~cssClass:"expectations" (fun () ->
      fprintf outFile "  <li>Expect compilation to %s.</li>\n"
        (if !expectedCompilationSuccess then "succeed" else "fail");
      if !expectedCompilationSuccess then
        fprintf outFile "  <li>Expect test to exit with code %s.</li>\n"
          (Exit_code.toString !expectedTestCaseExitCode);
      List.iter writeExpectation !expectedErrorMessages);

    let compilerMessagesOutputFile = replaceExtension outputFileName compilationOutputExt in
    let compilerError =
      let cmd = sprintf "(%s 2>&1) > %s" compileCommand compilerMessagesOutputFile in
      printf "%s\n" cmd;
      flush stdout;
      Sys.command cmd
    in

    writeHeaderWithLink 2 "Compiler output" (replaceExtension (Filename.basename zompFileName) "compile_output");
    inElements ["p"] ~cssClass:"console-output" (fun () ->
      forEachLineInFile compilerMessagesOutputFile checkCompilerExpectationsAndPrintLine);
    fprintf outFile "Compiler exited with code %d<br />\n" compilerError;

    if compilerError == 0 then begin
      let testrunOutputFile = replaceExtension zompFileName testOutputExt in
      let cmd = sprintf "(%s 2>&1) > %s" runTestCaseCommand testrunOutputFile in
      printf "%s\n" cmd;
      flush stdout;
      let runReturnCode = Sys.command cmd in

      writeHeaderWithLink 2 "Output" (replaceExtension (Filename.basename zompFileName) "test_output");
      inElements ["p"] ~cssClass:"console-output" (fun () ->
        forEachLineInFile testrunOutputFile checkRuntimeExpectationsAndPrintLine);

      fprintf outFile "Exited with code %d<br />\n" runReturnCode;
      match !expectedTestCaseExitCode with
        | Exit_code.MustBe expectedCode ->
          if expectedCode != runReturnCode then
            reportError `Br (sprintf "exited with code %d instead of %d" runReturnCode expectedCode)
        | Exit_code.MustNotBe unexpectedCode ->
          if runReturnCode = unexpectedCode then
            reportError `Br (sprintf "exited with code %d but was expected to exit with anything else" runReturnCode);
    end;

    writeHeader 2 "Results";

    inElements ["div"; "ul"] ~cssClass:"results" (fun () ->
      if !expectedCompilationSuccess && compilerError <> 0 then begin
        reportError `Li "compilation failed, but no errors expected";
        let printLine _ line = print_string line; print_newline() in
        forEachLineInFile compilerMessagesOutputFile printLine;
        print_newline();
      end else if not !expectedCompilationSuccess && compilerError = 0 then begin
        reportError `Li "compilation succeeded, but unit test expected errors";
      end;

      List.iter reportIfMissing !expectedErrorMessages);

    let cssClass, result =
      if !errorCount > 0 then
        "failed", formatQuantity !errorCount "error"
      else
        "ok", "ok"
    in
    fprintf outFile "Test case <span class=\"%s\">%s</span>\n<br />\n"
      cssClass result;

    let resultFile = replaceExtension zompFileName "result" in
    let result =
      match readOneLineFileIfPresent (replaceExtension zompFileName "last_result") with
        | Some line ->
          let lastStatus = withoutSuffix line '!' in
          if lastStatus <> result then
            result ^ "!"
          else
            result
        | None ->
          result
    in
    begin match Sys.command (sprintf "echo %s > %s" result resultFile) with
      | 0 -> ()
      | error ->
        printf "error: failed to create file %s\n" resultFile;
    end;

    begin
      writeHeaderWithLink 2 "Stats" (replaceExtension (Filename.basename zompFileName) statsExt);
      let statsFile = replaceExtension zompFileName statsExt in
      try
        inElements ["p"] ~cssClass:"console-output" (fun () ->
          let printLineToReport _ line =
            fprintf outFile "%s<br />\n" (escapeHtmlText line)
          in
          forEachLineInFile statsFile printLineToReport);
      with Sys_error _ ->
        inElements ["br"] (fun () ->
          fprintf outFile "File <span class=\"failed\">%s</span> missing." statsFile)
    end;

    writeHeader 2 "Source";

    let compareLine ({ line = lhsLine }, _, _) ({ line = rhsLine }, _, _) =
      compare lhsLine rhsLine
    in
    diagnostics := List.sort compareLine !diagnostics;
    let remDiagnostics = ref !diagnostics in

    let rec emitDiagnosticsUntil lineNum =
      begin match !remDiagnostics with
        | (loc, kind, message) :: tail when loc.line <= lineNum ->
          remDiagnostics := tail;
          if loc.line != 0 then begin
            let cssClass = cssClassForInlineDiagnosticsKind kind in
            let kindName = DiagnosticKind.toString kind in
            fprintf outFile "<br />\n    <span class=\"%s\">%s: %s</span>\n  " cssClass kindName message;
          end;
          emitDiagnosticsUntil lineNum
        | _ -> ()
      end;
    in

    let printLineAndDiagnostics lineNum line =
      fprintf outFile "  <li><code>%s</code>" line;
      emitDiagnosticsUntil lineNum;
      fprintf outFile "</li>\n";
    in

    inElements ["div"; "ol"] ~cssClass:"source" (fun () ->
      let source = readFile zompFileName in
      try
        let write, getLines = Basics.makeHtmlSourceWriter() in
        parseCommentsAndStrings write zompFileName source;
        listIteri printLineAndDiagnostics 1 (getLines());
      with Basics.CommentError (location, msg) ->
        let printLineToReport _ line =
          fprintf outFile "  <li><code>%s</code></li>\n" (escapeHtmlText line)
        in
        forEachLineInFile zompFileName printLineToReport);

    fprintf outFile "</html>")

