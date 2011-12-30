open Printf

let scriptName = "check_test"

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
end

open Utils

type expectation = ErrorMessage | WarningMessage | PrintMessage
let expectationOfString str =
  if str = "error" then ErrorMessage
  else if str = "warning" then WarningMessage
  else if str = "print" then PrintMessage
  else (failwith "expectationOfString")
let expectationToString = function
  | ErrorMessage -> "error"
  | WarningMessage -> "warning"
  | PrintMessage -> "print"

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

  let expectedErrorMessages = ref [] in
  let expectedCompilationSuccess = ref true in
  let expectedReturnCode = ref 0 in
  let addToList item refToList = refToList := item :: !refToList in
  let addExpectation kindStr args lineNum =
    try
      let kind = expectationOfString kindStr in
      begin match kind with
        | ErrorMessage ->
          expectedCompilationSuccess := false
        | WarningMessage | PrintMessage ->
          ()
      end;
      addToList (kind, args, lineNum, ref false) expectedErrorMessages
    with Failure _ ->
      printf "%s:%d: warning: invalid expectation kind '%s'. Expected error, warning or print\n"
        zompFileName lineNum kindStr
  in
  let collectExpectations lineNum line =
    if line =~ ".*//// \\([^ ]+\\) \\(.*\\)" then begin
      let kind = Str.matched_group 1 line in
      let args = Str.split (Str.regexp " +") (Str.matched_group 2 line) in
      addExpectation kind args lineNum
    end
  in

  let writeReport outFile =
    let writeExpectation (kind, args, lineNum, found) =
      fprintf outFile "Expecting %s at line %d containing words %s\n"
        (expectationToString kind)
        lineNum
        (String.concat ", " args)
    in
    let checkExpectation message diagnosticLineNum (kind, args, expectedLineNum, found) =
      let containsWord word =
        let f = Str.string_match (Str.regexp (".*" ^ Str.quote word)) message 0 in
        (if not f then
            printf "Word '%s' missing in message '%s'\n" word message);
        f
      in
      if diagnosticLineNum = expectedLineNum then begin
        if List.for_all containsWord args then
          found := true
      end
    in

    let errorRe = Str.regexp
      (sprintf "error: %s:\\([0-9]\\)+: .*error \\(.*\\)" (Str.quote zompFileName))
    in
    let checkExpectations _ line =
      fprintf outFile "%s\n" line;

      let isErrorMessage = Str.string_match errorRe line 0 in
      if isErrorMessage then begin
        let diagnosticLineNum = safeParseInt (Str.matched_group 1 line) in
        let message = Str.matched_group 2 line in
        List.iter (checkExpectation message diagnosticLineNum) !expectedErrorMessages
      end
    in
    let visitOutputLine _ line =
      let checkPrintExpectation (kind, args, expectedLineNum, found) =
        let containsWord word =
          Str.string_match (Str.regexp (".*" ^ Str.quote word)) line 0
        in
        match kind with
          | PrintMessage ->
            begin
              if List.for_all containsWord args then
                found := true
            end
          | _ -> ()
      in
      List.iter checkPrintExpectation !expectedErrorMessages;
      ()
    in

    let reportMissingDiagnostic (kind, args, lineNum, found) =
      match kind with
        | ErrorMessage ->
          if !found = false then
            printf "%s:%d: failed to report error containing words %s\n"
              zompFileName lineNum
              (String.concat ", " args)
        | WarningMessage ->
          printf "%s:%d: warning: checking for warnings not supported, yet\n"
            zompFileName lineNum
        | PrintMessage ->
          if !found = false then
            printf "%s:%d: error: failed to print line containing words %s\n"
              zompFileName lineNum
              (String.concat ", " args)
    in

    forEachLineInFile zompFileName collectExpectations;
    List.iter writeExpectation !expectedErrorMessages;

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

    if !expectedCompilationSuccess && compilerError <> 0 then begin
      printf "%s:1: error: compilation failed, but no errors expected\n" zompFileName;
      let printLine _ line = print_string line; print_newline() in
      forEachLineInFile compilerMessagesOutputFile printLine;
      print_newline();
    end else if not !expectedCompilationSuccess && compilerError = 0 then begin
      printf "%s:1: error: compilation succeeded, but unit test expected errors\n" zompFileName;
    end;

    fprintf outFile "Compiler output:\n";
    forEachLineInFile compilerMessagesOutputFile checkExpectations;

    if compilerError == 0 then begin
      let testrunOutputFile = replaceExtension zompFileName "test_output" in
      let cmd = sprintf "%s %s" makeCommand testrunOutputFile in
      let runReturnCode = Sys.command cmd in
      fprintf outFile "Exited with %d\n" runReturnCode;
      if runReturnCode != !expectedReturnCode then
        printf "error: %s:1: exited with code %d instead of %d\n"
          zompFileName runReturnCode !expectedReturnCode;
      forEachLineInFile testrunOutputFile visitOutputLine;
    end;

    List.iter reportMissingDiagnostic !expectedErrorMessages;
  in
  withOpenFileOut outputFileName writeReport

    

