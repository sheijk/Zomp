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

let readingFile fileName f =
  let stream = open_in fileName in
  try
    f stream;
    close_in stream
  with exn ->
    close_in stream;
    raise exn

let writingFile fileName f =
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
    readingFile filename printStream
  with End_of_file ->
    ()

let readingLines fileName f =
  let rec read lineNum stream =
    try
      let line = input_line stream in
      let () = f lineNum line in
      read (lineNum+1) stream
    with End_of_file -> ()
  in
  readingFile fileName (read 1)

let replaceExtension fileName ext =
  let noExt =
    try
      Filename.chop_extension fileName
    with Invalid_argument _ ->
      fileName
  in
  sprintf "%s.%s" noExt ext

let (=~) str re = Str.string_match (Str.regexp re) str 0

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

let safeParseInt str =
  try
    int_of_string str
  with Failure _ ->
    -1

let () =
  if false then begin
    printf "Called %s\n" (String.concat " " (Array.to_list Sys.argv));
    printf "From directory %s\n" (Sys.getcwd());
    flush stdout;
  end;
  
  if Array.length Sys.argv != 2 then
    failWith InvalidArguments;

  let outputFileName = Sys.argv.(1) in
  let zompFileName = replaceExtension outputFileName "zomp" in
  let compilerMessagesOutputFile = Filename.temp_file "zompc" "out" in
  let error =
    Sys.command (sprintf "./zompc.native -c %s > %s 2> %s"
                   zompFileName
                   compilerMessagesOutputFile
                   compilerMessagesOutputFile)
  in
  ignore error;
  (* let error = 0 in *)
  (* if error != 0 then begin *)
  (*   printf "Compiler output:\n"; *)
  (*   printFile compilerMessagesOutputFile; *)
  (*   failWith (CompilerFailed error) *)
  (* end; *)

  let expectedErrorMessages = ref [] in
  let addExpectation kindStr args lineNum =
    try
      let kind = expectationOfString kindStr in
      expectedErrorMessages := (kind, args, lineNum, ref false) :: !expectedErrorMessages
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
  readingLines zompFileName collectExpectations;
  
  writingFile outputFileName
    (fun outFile ->
       List.iter (fun (kind, args, lineNum, found) ->
                    fprintf outFile "Expecting %s at line %d containing words %s\n"
                      (expectationToString kind)
                      lineNum
                      (String.concat ", " args);)
         !expectedErrorMessages;
       
       let errorRe = Str.regexp
         (sprintf "%s:\\([0-9]\\)+: .*error \\(.*\\)" (Str.quote zompFileName))
       in
       let checkExpectations _ line =
         fprintf outFile "%s\n" line;
         
         if Str.string_match errorRe line 0 then begin
           let diagnosticLineNum = safeParseInt (Str.matched_group 1 line) in
           let message = Str.matched_group 2 line in
           
           let checkExpectation (kind, args, expectedLineNum, found) =
             if diagnosticLineNum = expectedLineNum then begin
               let containsWord word =
                 let f = Str.string_match (Str.regexp (".*" ^ Str.quote word)) message 0 in
                 (if not f then
                    printf "Word '%s' missing in message '%s'\n" word message);
                 f
               in
               if List.for_all containsWord args then
                 found := true
             end
           in
           List.iter checkExpectation !expectedErrorMessages
         end
       in
       fprintf outFile "Compiler output:\n";
       readingLines compilerMessagesOutputFile checkExpectations;
       List.iter
         (fun (kind, args, lineNum, found) ->
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
                  printf "%s:%d: warning: checking for print not supported, yet\n"
                    zompFileName lineNum)
         !expectedErrorMessages;
    )
  (* printf "----- Compiler output -----\n"; *)
  (* readingLines compilerMessagesOutputFile *)
  (*   (fun _ line -> print_string "  "; print_string line; print_newline()); *)
  (* printf "----- end -----\n" *)

