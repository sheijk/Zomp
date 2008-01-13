
open Printf

let withFileIn fileName func =
  let stream = open_in fileName in
  try
    let result = func stream in
    close_in stream;
    result
  with | _ as exc ->
    close_in stream;
    raise exc

let leadingSpaces str =
  let strLength = String.length str in
  let rec checkPos n =
    if n < strLength && str.[n] = ' ' then checkPos (n+1)
    else n
  in
  checkPos 0

let splitLine line =
  let indent = leadingSpaces line in
  (indent, String.sub line indent (String.length line - indent))

let classifyLine line =
  let lineLength = String.length line in
  let rec check index =
    if index >= lineLength then `EmptyLine
    else if line.[index] = ' ' then check (index+1)
    else `IndentedLine (index, String.sub line index (lineLength-index))
  in
  check 0

let iterBack f list = List.iter f (List.rev list)

let lastMatchedString = ref ""
let (=~) string regexp =
  lastMatchedString := string;
  Str.string_match (Str.regexp regexp) string  0
let nthmatch n =
  Str.matched_group n !lastMatchedString

let collectTestErrors f tests =
  let testF prevErrors (input, expected) =
    let result = f input in
    if result = expected then prevErrors
    else (input, expected, result) :: prevErrors
  in
  let errors = List.fold_left testF [] tests in
  errors


let printReport testSets =
  let printTestSetReport (testF, inputToString, outputToString) =
    let errors = testF() in
    let printError (input, expected, result) =
      printf "Test case error:\n  Input: %s\n  Found: %s\n  Expected: %s\n"
        (inputToString input) (outputToString result) (outputToString expected)
    in
    List.iter printError errors;
    List.length errors
  in
  let errorCounts = List.map printTestSetReport testSets in
  let errorCount = List.fold_left (+) 0 errorCounts in
  if errorCount = 0 then printf "No errors\n"
  else printf "%d errors\n" errorCount


    
let blockEndRE name =
  sprintf "end\\( +%s\\)? *$" name
(*   sprintf "end\\(\\s+%s\\)?\\s*$" *)
    
let () =
  let testCases = [
    "iff", "end", true;
    "iff", "end iff", true;
    "iff", "end wrong", false;
    "iff", "end iff  ", true;
    "iff", "end iff blah", false;
    "iff", "end   iff", true;
  ] in
  
  printf "\n";
  let boolToString b = if b then "true" else "false" in
  let errorOccured = ref false in
  let testF (blockName, line, shallEnd) =
    if shallEnd != (line =~ blockEndRE blockName) then begin
      errorOccured := true;
      printf "Failure in blockEndRE:\n  Input = %s\n  Expected = %s\n  Found = %s\n"
        (sprintf "%s, '%s'" blockName line) (boolToString shallEnd) (boolToString (not shallEnd))
    end
  in
  List.iter testF testCases;
(*   if not !errorOccured then *)
(*     printf "No errors occured\n" *)
  



exception WhitespaceError of string


let readLines fileName =
  let revLines =
    withFileIn fileName
      (fun inStream ->
         let linesCollected = ref [] in
         let rec collectLines () =
           let line = input_line inStream in
           linesCollected := line :: !linesCollected;
           collectLines()
         in
         try
           collectLines()
         with End_of_file ->
           !linesCollected)
  in
  List.rev revLines
      
let classifyIndent lines = 
  let rec worker acc activeIndents = function
    | [] ->
        acc
    | [singleLine] ->
        (`SameIndent, 0, singleLine) :: acc
    | prevLine :: nextLine :: rem ->
        let prevIndent, prevUnindented = splitLine prevLine in
        let nextIndent, nextUnindented = splitLine nextLine in
        let indentDiff, newActiveIndents = 
          if prevIndent = nextIndent then begin
            `SameIndent, activeIndents
          end else if prevIndent < nextIndent then begin
            let newActiveIndents = nextIndent :: activeIndents in
            (*             if prevUnindented =~ "\\([a-zA-Z0-9_:.]+\\):.*" then *)
            (*               `MoreIndentMulti (nthmatch 1), newActiveIndents *)
            (* else *)
            `MoreIndent, newActiveIndents
          end else (* if prevIndent > nextIndent then *) begin
            let rec unindent count = function
              | [] ->
                  1000, []
              | indent :: rem when nextIndent < indent ->
                  unindent (count+1) rem
              | indents ->
                  count, indents
            in
            let unindentCount, remainingIndents = unindent 0 activeIndents in
            `LessIndent unindentCount, remainingIndents
          end
        in
        worker
          ((indentDiff, prevIndent, prevUnindented) :: acc)
          newActiveIndents
          (nextLine :: rem)
  in
  List.rev (worker [] [0] lines)

let printIndentInfo classifiedLines = 
  List.iter
    (fun (indentDiff, indent, unindentedLine) ->
       printf "%s%s %s\n"
         (String.make indent ' ')
         unindentedLine
         (match indentDiff with
            | `SameIndent -> ""
            | `MoreIndent -> "->"
            | `LessIndent count -> "<- " ^ string_of_int count))
    classifiedLines
  
let multiBlockRE = "\\([a-zA-Z0-9_:.]+\\):"
  
let printLines clines =
  let printSingleLine indent unindentedLine =
    printf "%s(%s)\n" (String.make indent ' ') unindentedLine
  in
  
  let rec printIndentBlock callerF = function
    | [] -> ()
    | (indentDiff, indent, unindentedLine) :: remLines ->
        let indentString = String.make indent ' ' in
        match indentDiff with
          | `SameIndent ->
              printSingleLine indent unindentedLine;
              printIndentBlock callerF remLines
          | `MoreIndent ->
              if unindentedLine =~ multiBlockRE then begin
                let blockName = nthmatch 1 in
                printMultiBlockFirstLine
                  blockName
                  (printIndentBlock callerF)
                  indentString unindentedLine
                  remLines
              end else begin
                printf "%s(%s (\n" indentString unindentedLine;
                printIndentBlock (printIndentBlock callerF) remLines
              end
          | `LessIndent (count :int) ->
              printf "%s(%s) %s\n" indentString unindentedLine (String.make count ')');
              callerF remLines
                
  and printMultiBlockFirstLine blockName callerF indentString unindentedLine lines =
    printf "%s(%s (\n" indentString unindentedLine;
    printIndentBlock (printMultiBlock blockName callerF) lines
      
  and printMultiBlock blockName callerF = function
    | [] ->
        raise (WhitespaceError (sprintf "EOF while expecting closing block '%s'" blockName))
    | (indentDiff, indent, unindentedLine) :: remLines ->
        let indentString = String.make indent ' ' in
        match indentDiff with
          | `MoreIndent ->
              printf "%s%s (\n" indentString unindentedLine;
              printIndentBlock (printMultiBlock blockName callerF) remLines
          | `LessIndent (count :int) ->
              raise (WhitespaceError
                       (sprintf "Reduced indent while expecting closing block '%s'" blockName))
          | `SameIndent ->
              if unindentedLine =~ blockEndRE blockName then begin
                printf "%s)\n" indentString;
                callerF remLines
              end else begin
                printSingleLine indent unindentedLine;
                printMultiBlock blockName callerF remLines
              end
  in
  
  let returnedToOften remLines =
    printf "\n%s:\n%s\n----\n"
      "Internal error. More unindenting than indenting, remaining lines"
      (Common.combine "\n" (List.map (fun (_, _, line) -> line) remLines));
    flush stdout;
    raise (WhitespaceError "More unindenting than indenting")
  in
  
  printIndentBlock returnedToOften clines

  
let () =
  let sourceFile = "indent.zomp" in

  let lines = readLines sourceFile in
  let classifiedLines = classifyIndent lines in
  (* printIndentInfo classifiedLines; *)
  printLines classifiedLines;

  ()


    
(*   let rec bracketLines acc = function *)
(*     | [] -> *)
(*         acc *)
(*     | (indentDiff, indent, unindentedLine) :: rem -> *)
(*         let indentString = String.make indent ' ' in *)
(*         let terminator, parseF = *)
(*           match indentDiff with *)
(*             | `SameIndent -> ")", bracketLines *)
(*             | `MoreIndentMulti name -> " (" ^ name ^ ":", bracketLinesBlock *)
(*             | `MoreIndent -> " (", bracketLines *)
(*             | `LessIndent count -> ") " ^ String.make (2 * count) ')', bracketLines *)
(*         in *)
(*         let line = indentString ^ "(" ^ unindentedLine ^ terminator in *)
(*         parseF (line :: acc) rem *)
(*   and bracketLinesBlock acc x = bracketLines acc x *)
(*   in *)


    
(*   let rec bracketLines acc = function *)
(*     | [] -> *)
(*         [], [] *)
(*     | (indentDiff, indent, unindentedLine) :: rem -> *)
(*         let indentString = String.make indent ' ' in *)
(*         match indentDiff with *)
(*           | `SameIndent -> *)
(*               let line = sprintf "%s(%s)\n" indentString unindentedLine in *)
(*               bracketLines (line :: acc) rem *)
(*           | `MoreIndent -> *)
(*               let line = sprintf "%s(%s (\n" indentString unindentedLine in *)
(*               bracketLines (line :: acc) rem *)
(*           | `MoreIndentMulti name -> *)
(*               let line = sprintf "%s(%s (\n" indentString unindentedLine in *)
(*               bracketLinesMulti name (line :: acc) rem *)
(*           | `LessIndent count -> *)
(*               let line = *)
(*                 sprintf "%s(%s) %s\n" *)
(*                   indentString *)
(*                   unindentedLine *)
(*                   (String.make count ')') *)
(*               in *)
(*               (line :: acc), rem *)
(*   and bracketLinesMulti (name :string) (acc :string list) = function *)
(*     | [] -> *)
(*         acc, [] *)
(*     | (indentDiff, indent, unindentedLine) :: rem -> *)
(*         let indentString = String.make indent ' ' in *)
(*         match indentDiff with *)
(*           | `LessIndent _ -> *)
(*               raise (WhitespaceError (sprintf "Block %s not terminated" name)) *)
(*           | `MoreIndentMulti newBlock -> *)
(*               raise (WhitespaceError *)
(*                        (sprintf "New block '%s'started before end of block '%s'" *)
(*                           newBlock name)) *)
(*           | `MoreIndent -> *)
(*               let line = sprintf "%s(%s (\n" indentString unindentedLine in *)
(*               bracketLines (line :: acc) rem *)
(*           | `SameIndent -> *)
(*               if unindentedLine =~ blockEndRE name then *)
(*                 let line = sprintf "%s))\n" indentString in *)
(*                 (line :: acc), rem *)
(*               else *)
(*                 raise (WhitespaceError (sprintf "Expected end of block '%s'" name)) *)
(*   in *)
(*   let bracketLines lines = *)
(*     let bracketed, rem = bracketLines [] lines in *)
(*     let unhandledLinesCount = List.length rem in *)
(*     if unhandledLinesCount > 0 then begin *)
(*       printf "%d lines not handled:\n" unhandledLinesCount; *)
(*       List.iter (fun (_, indent, line) -> printf "%s%s\n" (String.make indent ' ') line) rem; *)
(*       printf "-------------------\n"; *)
(*     end; *)
(*     bracketed *)
(*   in *)
