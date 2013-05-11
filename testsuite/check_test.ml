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
let compilationOutputExt = "compile_output"
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

  let withoutSuffix line suffix =
    let lineLength = String.length line in
    if (lineLength > 0) && (line.[lineLength-1] = suffix ) then
      Str.string_before line (lineLength-1)
    else
      line
end

open Utils

module ExpectationKind =
struct
  type t = CompilerError | CompilerErrorNoLoc | CompilerWarning | CompilerInfo | RuntimePrint | TestCaseExitCode
  let compilerErrorCommand = "error"
  let compilerErrorNoLocCommand = "error-no-location"
  let compilerWarningCommand = "warning"
  let compilerInfoCommand = "info"
  let runtimePrintCommand = "print"
  let testCaseExitCodeCommand = "exit-code"

  let parse str =
    if str = compilerErrorCommand then CompilerError
    else if str = compilerErrorNoLocCommand then CompilerErrorNoLoc
    else if str = compilerWarningCommand then CompilerWarning
    else if str = compilerInfoCommand then CompilerInfo
    else if str = runtimePrintCommand then RuntimePrint
    else if str = testCaseExitCodeCommand then TestCaseExitCode
    else (failwith "Expectation.parse")

  let validExpectationsEnumDescr =
    verbalConcat "or"
      [compilerErrorCommand; compilerWarningCommand; compilerInfoCommand;
       runtimePrintCommand]

  let kindDescription = function
    | CompilerError -> "compiler error"
    | CompilerErrorNoLoc -> "compiler error w/o location"
    | CompilerWarning -> "compiler warning"
    | CompilerInfo -> "compiler info"
    | RuntimePrint -> "test case to print line"
    | TestCaseExitCode -> "test case to exit with"

  let description kind words =
    let quote = sprintf "\"%s\"" in
    (sprintf "%s containing word%s %s"
       (kindDescription kind)
       (match words with [_] -> "" | _ -> "s")
       (verbalConcat "and" (List.map quote words)))
end

module Exit_code = struct
  type condition =
    | MustBe of int
    | MustNotBe of int

  let conditionToString = function
    | MustBe exitCode -> sprintf "%d" exitCode
    | MustNotBe exitCode -> sprintf "anything other than %d" exitCode
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

let addExpectation
    zompFileName expectedCompilationSuccess expectedErrorMessages expectedTestCaseExitCode
    kindStr args lineNum =
  let reportWarning msg =
    printf "%s:%d: warning: %s\n" zompFileName lineNum msg
  in
  try
    let kind = ExpectationKind.parse kindStr in
    let add () =
      addToList (kind, args, lineNum, ref false) expectedErrorMessages
    in
    begin match kind with
      | ExpectationKind.CompilerError ->
        expectedCompilationSuccess := false;
        add()
      | ExpectationKind.CompilerErrorNoLoc ->
        expectedCompilationSuccess := false;
        addToList (kind, args, -1, ref false) expectedErrorMessages
      | ExpectationKind.CompilerWarning
      | ExpectationKind.CompilerInfo
      | ExpectationKind.RuntimePrint ->
        add()
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
    let errorOccured = ref false in
    let reportError ?line msg =
      let line = match line with Some l -> l | _ -> 0 in
      let msg = sprintf "%s:%d: error: %s" zompFileName line msg in
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
        (ExpectationKind.description kind args)
    in
    let checkExpectation message diagnosticLineNum (kind, args, expectedLineNum, found) =
      let containsWord word =
        Str.string_match (Str.regexp_case_fold (".*" ^ Str.quote word)) message 0
      in
      match kind with
        | ExpectationKind.CompilerError
        | ExpectationKind.CompilerWarning ->
          if diagnosticLineNum = expectedLineNum then begin
            if List.for_all containsWord args then begin
              found := true
            end
          end
        | ExpectationKind.CompilerErrorNoLoc ->
          if List.for_all containsWord args then begin
            found := true
          end
        | ExpectationKind.CompilerInfo ->
          if List.for_all containsWord args then begin
            found := true
          end
        | ExpectationKind.TestCaseExitCode
        | ExpectationKind.RuntimePrint ->
          ()
    in

    let diagnosticRe =
      let re = (sprintf "^%s:\\([0-9]+\\)+: .*\\(error\\|warning\\|info\\): \\(.*\\)" (Str.quote zompFileName)) in
      Str.regexp re
    in
    let checkCompilerExpectationsAndPrintLine _ line =
      fprintf outFile "%s<br />\n" (escapeHtmlText line);

      let isErrorMessage = Str.string_match diagnosticRe line 0 in
      if isErrorMessage then begin
        let diagnosticLineNum = safeParseInt (Str.matched_group 1 line) in
        let message = Str.matched_group 3 line in
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
          | ExpectationKind.RuntimePrint ->
            begin
              if List.for_all containsWord args then
                found := true
            end
          | _ -> ()
      in
      List.iter checkPrintExpectation !expectedErrorMessages
    in

    let reportIfMissing (kind, args, lineNum, found) =
      if !found = false then
        reportError ~line:lineNum
          (sprintf "expected %s but didn't happen"
             (ExpectationKind.description kind args))
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
        fprintf outFile "Expect test to exit with code %s. <br />\n"
          (Exit_code.conditionToString !expectedTestCaseExitCode);
      List.iter writeExpectation !expectedErrorMessages);

    let compilerMessagesOutputFile = replaceExtension outputFileName compilationOutputExt in
    let compilerError =
      let cmd = sprintf "(%s 2>&1) > %s" compileCommand compilerMessagesOutputFile in
      printf "%s\n" cmd;
      flush stdout;
      Sys.command cmd
    in

    writeHeader 2 "Compiler output";
    inMonospace (fun () ->
      forEachLineInFile compilerMessagesOutputFile checkCompilerExpectationsAndPrintLine);
    fprintf outFile "Compiler exited with code %d</br>\n" compilerError;

    if compilerError == 0 then begin
      let testrunOutputFile = replaceExtension zompFileName testOutputExt in
      let cmd = sprintf "(%s 2>&1) > %s" runTestCaseCommand testrunOutputFile in
      printf "%s\n" cmd;
      flush stdout;
      let runReturnCode = Sys.command cmd in

      writeHeader 2 "Output";
      inMonospace (fun () ->
        forEachLineInFile testrunOutputFile checkRuntimeExpectationsAndPrintLine);

      fprintf outFile "Exited with code %d<br />\n" runReturnCode;
      match !expectedTestCaseExitCode with
        | Exit_code.MustBe expectedCode ->
          if expectedCode != runReturnCode then
            reportError (sprintf "exited with code %d instead of %d" runReturnCode expectedCode)
        | Exit_code.MustNotBe unexpectedCode ->
          if runReturnCode = unexpectedCode then
            reportError (sprintf "exited with code %d but was expected to exit with anything else" runReturnCode);
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

    List.iter reportIfMissing !expectedErrorMessages;

    let cssClass, result =
      if !errorOccured then
        "failed", "failed"
      else
        "ok", "succeeded"
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

    fprintf outFile "</html>")

