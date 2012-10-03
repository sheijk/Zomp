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
end

open Utils

module Expectation =
struct
  type t = ErrorMessage | WarningMessage | PrintMessage

  let parse str =
    if str = "error" then ErrorMessage
    else if str = "warning" then WarningMessage
    else if str = "print" then PrintMessage
    else (failwith "Expectation.parse")

  let verbalDescription = function
    | ErrorMessage -> "error"
    | WarningMessage -> "warning"
    | PrintMessage -> "to print line"
end

let writeHtmlHeader outFile zompFileName =
  fprintf outFile "<html>\n";
  fprintf outFile "  <head>\n";
  fprintf outFile "    <title>Report for %s</title>\n" zompFileName;
  fprintf outFile "  </head>\n";
  fprintf outFile "  <body>\n"

let timeStr time =
  sprintf "%d-%02d-%02d %02d:%02d:%02d"
    (time.Unix.tm_year + 1900) (time.Unix.tm_mon + 1) time.Unix.tm_mday
    time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec

let deleteOutputFiles outputFileName =
  let extensions = ["bc"; "ll"; "exe"; "test_output"] in
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
      | Expectation.ErrorMessage ->
        expectedCompilationSuccess := false
      | Expectation.WarningMessage | Expectation.PrintMessage ->
        ()
    end;
    addToList (kind, args, lineNum, ref false) expectedErrorMessages
  with Failure _ ->
    printf "%s:%d: warning: invalid expectation kind '%s'. Expected error, warning or print\n"
      zompFileName lineNum kindStr

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
  let addExpectation =
    addExpectation zompFileName expectedCompilationSuccess expectedErrorMessages
  in
  let collectExpectations = collectExpectations addExpectation in

  withOpenFileOut outputFileName (fun outFile ->
    let writeHeader n header =
      fprintf outFile "<h%d>%s</h%d>\n" n header n
    in

    let inMonospace f =
      fprintf outFile "<p><span style=\"font-family:monospace\">\n";
      f();
      fprintf outFile "</span></p>\n"
    in

    let writeExpectation (kind, args, lineNum, found) =
      fprintf outFile "%s:%d: expect %s containing word%s %s<br />\n"
        zompFileName
        lineNum
        (Expectation.verbalDescription kind)
        (if List.length args > 1 then "s" else "")
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
      fprintf outFile "%s<br />\n" (escapeHtmlText line);

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
          | Expectation.PrintMessage ->
            begin
              if List.for_all containsWord args then
                found := true
            end
          | _ -> ()
      in
      List.iter checkPrintExpectation !expectedErrorMessages;
      fprintf outFile "%s<br />\n" (escapeHtmlText line);
      ()
    in

    let reportMissingDiagnostic (kind, args, lineNum, found) =
      match kind with
        | Expectation.ErrorMessage ->
          if !found = false then
            printf "%s:%d: failed to report error containing words %s\n"
              zompFileName lineNum
              (String.concat ", " args)
        | Expectation.WarningMessage ->
          printf "%s:%d: warning: checking for warnings not supported, yet\n"
            zompFileName lineNum
        | Expectation.PrintMessage ->
          if !found = false then
            printf "%s:%d: error: failed to print line containing words %s\n"
              zompFileName lineNum
              (String.concat ", " args)
    in

    (** code *)

    writeHtmlHeader outFile zompFileName;
    fprintf outFile "<h1>Test report for %s</h1>" zompFileName;
    fprintf outFile "Executed at %s</br>\n"
      (timeStr (Unix.localtime (Unix.gettimeofday())));

    forEachLineInFile zompFileName collectExpectations;
    expectedErrorMessages := List.rev !expectedErrorMessages;
    writeHeader 2 "Expectations";
    inMonospace (fun () ->
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

    if !expectedCompilationSuccess && compilerError <> 0 then begin
      printf "%s:1: error: compilation failed, but no errors expected\n" zompFileName;
      let printLine _ line = print_string line; print_newline() in
      forEachLineInFile compilerMessagesOutputFile printLine;
      print_newline();
    end else if not !expectedCompilationSuccess && compilerError = 0 then begin
      printf "%s:1: error: compilation succeeded, but unit test expected errors\n"
        zompFileName;
    end;

    writeHeader 2 "Compiler output";
    inMonospace (fun () ->
      forEachLineInFile compilerMessagesOutputFile checkExpectations);

    if compilerError == 0 then begin
      let testrunOutputFile = replaceExtension zompFileName testOutputExt in
      let cmd = sprintf "%s %s" makeCommand testrunOutputFile in
      let runReturnCode = Sys.command cmd in

      writeHeader 2 "Output";
      inMonospace (fun () ->
        forEachLineInFile testrunOutputFile visitOutputLine);

      fprintf outFile "Exited with %d<br />\n" runReturnCode;
      let testF, expectedReturn =
        if !expectedCompilationSuccess then
          ((==) 0), "0"
        else
          ((!=) 0), "not 0"
      in
      if (not (testF runReturnCode)) then
        printf "error: %s:1: exited with code %d instead of %s<br />\n"
          zompFileName runReturnCode expectedReturn
    end;

    List.iter reportMissingDiagnostic !expectedErrorMessages;

    fprintf outFile "</html>")

