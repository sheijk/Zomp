(**
   This program is used to run the testsuite integration tests. Call it using
   'check_test foo.testresult make-command' to run the tests in foo.zomp.
   make-command is a command used to invoke make, it should be "$(MAKE)" if you
   call this from a makefile. The tool will call '$make-command foo.exe' to
   build the executable and '$make-command foo.test_output' to run the test.
 *)

open Printf

let scriptName = "check_test"

let testOutputExt = "test_output"
let reportOutputExt = "testreport"

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
      f stream;
      close_in stream
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

  let forEachLineInFile fileName f =
    let rec read lineNum stream =
      try
        let line = input_line stream in
        let () = f lineNum line in
        read (lineNum+1) stream
      with End_of_file -> ()
    in
    withOpenFileIn fileName (read 1)

  let replaceExtension fileName ext =
    let noExt =
      try
        Filename.chop_extension fileName
      with Invalid_argument _ ->
        fileName
    in
    sprintf "%s.%s" noExt ext

  let safeParseInt str =
    try
      int_of_string str
    with Failure _ ->
      -1

  let (=~) str re = Str.string_match (Str.regexp re) str 0

  let stringToList str =
    let strLength = String.length str in
    let rec loop lst i =
      if i >= strLength then lst
      else (String.unsafe_get str i) :: loop lst (i+1)
    in
    loop [] 0

  let escapeHtmlText str =
    let replaceChar chr =
      let between min max = (chr >= min) && (chr <= max) in
      let isin chars = try ignore (String.index chars chr); true with Not_found -> false in
      if chr = ' ' then
        "&nbsp;"
      else if between 'a' 'z' || between 'A' 'Z' || between '0' '9' || isin ".,_-$~()" then
        String.make 1 chr
      else
        let ascii = int_of_char chr in
        sprintf "&#%d;" ascii
    in
    String.concat "" (List.map replaceChar (stringToList str))

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
end

open Utils

module Expectation =
struct
  type t = CompilerError | CompilerWarning | CompilerInfo | RuntimePrint
  let compilerErrorCommand = "error"
  let compilerWarningCommand = "warning"
  let compilerInfoCommand = "info"
  let runtimePrintCommand = "print"

  let parse str =
    if str = compilerErrorCommand then CompilerError
    else if str = compilerWarningCommand then CompilerWarning
    else if str = compilerInfoCommand then CompilerInfo
    else if str = runtimePrintCommand then RuntimePrint
    else (failwith "Expectation.parse")

  let validExpectationsEnumDescr =
    verbalConcat "or"
      [compilerErrorCommand; compilerWarningCommand; compilerInfoCommand;
       runtimePrintCommand]

  let kindDescription = function
    | CompilerError -> "compiler error"
    | CompilerWarning -> "compiler warning"
    | CompilerInfo -> "compiler info"
    | RuntimePrint -> "test case to print line"


  let description kind words =
    let quote = sprintf "\"%s\"" in
    (sprintf "%s containing word%s %s"
       (kindDescription kind)
       (match words with [_] -> "" | _ -> "s")
       (verbalConcat "and" (List.map quote words)))
end

let writeHtmlHeader outFile zompFileName =
  fprintf outFile "<html>\n";
  fprintf outFile "  <head>\n";
  fprintf outFile "    <title>Report for %s</title>\n" zompFileName;
  fprintf outFile "    <style type=\"text/css\">\n";
  fprintf outFile "      .ok { color: green; }\n";
  fprintf outFile "      .failed { color: red; }\n";
  fprintf outFile "    </style>\n";
  fprintf outFile "  </head>\n";
  fprintf outFile "  <body>\n"

let timeStr time =
  sprintf "%d-%02d-%02d %02d:%02d:%02d"
    (time.Unix.tm_year + 1900) (time.Unix.tm_mon + 1) time.Unix.tm_mday
    time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec

let deleteOutputFiles outputFileName =
  let extensions = ["bc"; "op-bc"; "ll"; "exe"; "test_output"] in
  List.iter
    (fun ext ->
      let name = replaceExtension outputFileName ext in
      if Sys.file_exists name then
        Sys.remove name)
    extensions

let addToList item refToList = refToList := item :: !refToList

let addExpectation zompFileName expectedCompilationSuccess expectedErrorMessages kindStr args lineNum =
  try
    let kind = Expectation.parse kindStr in
    begin match kind with
      | Expectation.CompilerError ->
        expectedCompilationSuccess := false
      | Expectation.CompilerWarning
      | Expectation.CompilerInfo
      | Expectation.RuntimePrint ->
        ()
    end;
    addToList (kind, args, lineNum, ref false) expectedErrorMessages
  with Failure _ ->
    printf "%s:%d: warning: invalid expectation kind '%s'. Expected %s\n"
      zompFileName lineNum kindStr Expectation.validExpectationsEnumDescr

let collectExpectations addExpectation lineNum line =
  if line =~ ".*//// \\([^ ]+\\) \\(.*\\)" then begin
    let kind = Str.matched_group 1 line in
    let args = Str.split (Str.regexp " +") (Str.matched_group 2 line) in
    addExpectation kind args lineNum
  end

let () =
  if false then begin
    printf "Called %s\n" (String.concat " " (Array.to_list Sys.argv));
    printf "From directory %s\n" (Sys.getcwd());
    flush stdout;
  end;

  if Array.length Sys.argv != 3 then
    failWith InvalidArguments;

  let outputFileName = Sys.argv.(1) in
  let zompFileName = replaceExtension outputFileName "zomp" in

  let makeCommand = Sys.argv.(2) in

  deleteOutputFiles outputFileName;

  let expectedErrorMessages = ref [] in
  let expectedCompilationSuccess = ref true in
  let expectedReturnValueForRun = ref 0 in
  let addExpectation =
    addExpectation zompFileName expectedCompilationSuccess expectedErrorMessages
  in
  let collectExpectations = collectExpectations addExpectation in

  withOpenFileOut outputFileName (fun outFile ->
    let errorOccured = ref false in
    let reportError msg =
      let msg = sprintf "%s:1: error: %s" zompFileName msg in
      fprintf outFile "%s<br />\n" msg;
      fprintf stderr "%s\n" msg;
      errorOccured := true;
    in

    let writeHeader n header =
      fprintf outFile "<h%d>%s</h%d>\n" n header n
    in

    let inMonospace f =
      fprintf outFile "<p><span style=\"font-family:monospace\">\n";
      f();
      fprintf outFile "</span></p>\n"
    in

    let writeExpectation (kind, args, lineNum, found) =
      fprintf outFile "%s:%d: expect %s<br />\n"
        zompFileName
        lineNum
        (Expectation.description kind args)
    in
    let checkExpectation message diagnosticLineNum (kind, args, expectedLineNum, found) =
      let containsWord word =
        Str.string_match (Str.regexp (".*" ^ Str.quote word)) message 0
      in
      match kind with
        | Expectation.CompilerError
        | Expectation.CompilerWarning ->
          if diagnosticLineNum = expectedLineNum then
            if List.for_all containsWord args then begin
              found := true
            end
        | Expectation.CompilerInfo ->
          if List.for_all containsWord args then begin
            found := true
          end
        | Expectation.RuntimePrint ->
          ()
    in

    let errorRe =
      let re = (sprintf "^%s:\\([0-9]\\)+: .*error: \\(.*\\)" (Str.quote zompFileName)) in
      Str.regexp re
    in
    let checkCompilerExpectationsAndPrintLine _ line =
      fprintf outFile "%s<br />\n" (escapeHtmlText line);

      let isErrorMessage = Str.string_match errorRe line 0 in
      if isErrorMessage then begin
        let diagnosticLineNum = safeParseInt (Str.matched_group 1 line) in
        let message = Str.matched_group 2 line in
        List.iter (checkExpectation message diagnosticLineNum) !expectedErrorMessages
      end else begin
        List.iter (checkExpectation line 0) !expectedErrorMessages
      end
    in

    let checkRuntimeExpectationsAndPrintLine _ line =
      fprintf outFile "%s<br />\n" (escapeHtmlText line);

      let checkPrintExpectation (kind, args, expectedLineNum, found) =
        let containsWord word =
          Str.string_match (Str.regexp (".*" ^ Str.quote word)) line 0
        in
        match kind with
          | Expectation.RuntimePrint ->
            begin
              if List.for_all containsWord args then
                found := true
            end
          | _ -> ()
      in
      List.iter checkPrintExpectation !expectedErrorMessages
    in

    let reportMissingDiagnostic (kind, args, lineNum, found) =
      if !found = false then
        reportError
          (sprintf "expected %s but didn't happen"
             (Expectation.description kind args))
    in

    (** code *)

    writeHtmlHeader outFile zompFileName;
    fprintf outFile "<h1>Test report for <a href=\"%s\">%s</a></h1>"
      (Filename.basename zompFileName) zompFileName;
    fprintf outFile "Executed at %s</br>\n"
      (timeStr (Unix.localtime (Unix.gettimeofday())));

    forEachLineInFile zompFileName collectExpectations;
    expectedErrorMessages := List.rev !expectedErrorMessages;
    writeHeader 2 "Expectations";
    inMonospace (fun () ->
      fprintf outFile "Expect compilation to %s. <br />\n"
        (if !expectedCompilationSuccess then "succeed" else "fail");
      if !expectedCompilationSuccess then
        fprintf outFile "Expect test to exit with code %d. <br />\n"
          !expectedReturnValueForRun;
      List.iter writeExpectation !expectedErrorMessages);

    let compilerMessagesOutputFile = Filename.temp_file "zompc" "out" in
    let compilerError =
      let cmd =
        sprintf "( %s %s 2>&1 ) > %s"
          makeCommand
          (replaceExtension zompFileName "exe")
          compilerMessagesOutputFile
      in
      Sys.command cmd;
    in

    writeHeader 2 "Compiler output";
    inMonospace (fun () ->
      forEachLineInFile compilerMessagesOutputFile checkCompilerExpectationsAndPrintLine);
    fprintf outFile "Compiler exited with code %d</br>\n" compilerError;

    if compilerError == 0 then begin
      let testrunOutputFile = replaceExtension zompFileName testOutputExt in
      let cmd = sprintf "%s %s" makeCommand testrunOutputFile in
      let runReturnCode = Sys.command cmd in

      writeHeader 2 "Output";
      inMonospace (fun () ->
        forEachLineInFile testrunOutputFile checkRuntimeExpectationsAndPrintLine);

      fprintf outFile "Exited with code %d<br />\n" runReturnCode;
      if runReturnCode <> !expectedReturnValueForRun then
        reportError (sprintf "exited with code %d instead of %d"
                       runReturnCode !expectedReturnValueForRun);
    end;

    writeHeader 2 "Results";

    if !expectedCompilationSuccess && compilerError <> 0 then begin
      reportError "compilation failed, but no errors expected";
      let printLine _ line = print_string line; print_newline() in
      forEachLineInFile compilerMessagesOutputFile printLine;
      print_newline();
    end else if not !expectedCompilationSuccess && compilerError = 0 then begin
      reportError "compilation succeeded, but unit test expected errors";
    end;

    List.iter reportMissingDiagnostic !expectedErrorMessages;

    let cssClass, result =
      if !errorOccured then
        "failed", "failed"
      else
        "ok", "succeeded"
    in
    fprintf outFile "Test case <span class=\"%s\">%s</span>\n<br />\n"
      cssClass result;

    let resultFile = replaceExtension zompFileName "result" in
    begin match Sys.command (sprintf "echo %s > %s" result resultFile) with
      | 0 -> ()
      | error ->
        printf "error: failed to create file %s\n" resultFile;
    end;

    fprintf outFile "</html>")

