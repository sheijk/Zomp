
open Printf

type lineNumber = int
exception WhitespaceError of string * lineNumber

let raiseWSError line message = raise (WhitespaceError (message, line))
let raiseWSErrorNoLine message = raise (WhitespaceError (message, -1))

(* Utils ---------------------------------------------------------------------*)

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

(* let classifyLine line = *)
(*   let lineLength = String.length line in *)
(*   let rec check index = *)
(*     if index >= lineLength then `EmptyLine *)
(*     else if line.[index] = ' ' then check (index+1) *)
(*     else `IndentedLine (index, String.sub line index (lineLength-index)) *)
(*   in *)
(*   check 0 *)

let iterBack f list = List.iter f (List.rev list)

let lastMatchedString = ref ""
let (=~) string regexp =
  lastMatchedString := string;
  Str.string_match (Str.regexp regexp) string  0
let nthmatch n =
  Str.matched_group n !lastMatchedString

(* let collectTestErrors f tests = *)
(*   let testF prevErrors (input, expected) = *)
(*     let result = f input in *)
(*     if result = expected then prevErrors *)
(*     else (input, expected, result) :: prevErrors *)
(*   in *)
(*   let errors = List.fold_left testF [] tests in *)
(*   errors *)


(* let printReport testSets = *)
(*   let printTestSetReport (testF, inputToString, outputToString) = *)
(*     let errors = testF() in *)
(*     let printError (input, expected, result) = *)
(*       printf "Test case error:\n  Input: %s\n  Found: %s\n  Expected: %s\n" *)
(*         (inputToString input) (outputToString result) (outputToString expected) *)
(*     in *)
(*     List.iter printError errors; *)
(*     List.length errors *)
(*   in *)
(*   let errorCounts = List.map printTestSetReport testSets in *)
(*   let errorCount = List.fold_left (+) 0 errorCounts in *)
(*   if errorCount = 0 then printf "No errors\n" *)
(*   else printf "%d errors\n" errorCount *)

let repeatedList n v =
  let rec worker n acc =
    if n <= 0 then acc
    else worker (n-1) (v :: acc)
  in
  worker n []
    

(* type indentDelta = [`EmptyLine | `LessIndent of int | `MoreIndent | `SameIndent ] *)
(* type indentF = int -> (indentDelta * int * string) list -> unit *)

(* Program ------------------------------------------------------------------ *)
  
let blockEndRE name =
  sprintf "end\\( +%s\\)? *$" name

let whitespaceLineRE = "^[ 	]*$"
  
let multiBlockRE =
  let idchars = "a-zA-Z0-9_." in
  sprintf "\\([%s:]*[%s]\\):\\(.*\\)$" idchars idchars
  
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
  let rec worker acc prevLine activeIndents = function
    | [] ->
        acc
    | (_ :: _) as allLines ->
        let emptyLineCount, nextLine, rem = skipEmptyLines 0 allLines in
        let prevIndent, prevUnindented = splitLine prevLine in
        let nextIndent, nextUnindented = splitLine nextLine in
        let indentDiff, newActiveIndents =
          if prevLine =~ whitespaceLineRE then begin
            `EmptyLine, activeIndents
          end else if prevIndent = nextIndent then begin
            `SameIndent, activeIndents
          end else if prevIndent < nextIndent then begin
            let newActiveIndents = nextIndent :: activeIndents in
            `MoreIndent, newActiveIndents
          end else (* if prevIndent > nextIndent then *) begin
            let rec unindent count = function
              | [] ->
                  raiseWSErrorNoLine
                    (sprintf "%s. Current line: %s"
                       "Internal error. Less active indent levels than expected"
                       nextLine)
              | indent :: rem when nextIndent < indent ->
                  unindent (count+1) rem
              | indents ->
                  count, indents
            in
            let unindentCount, remainingIndents = unindent 0 activeIndents in
            `LessIndent unindentCount, remainingIndents
          end
        in
        let emptyLines = repeatedList emptyLineCount (`EmptyLine, 0, "") in
        worker
          (emptyLines @ ((indentDiff, prevIndent, prevUnindented) :: acc))
          nextLine
          newActiveIndents
          rem
  and skipEmptyLines count = function
    | [] ->
        count, "=eof=", []
    | nextLine :: rem ->
        if nextLine =~ whitespaceLineRE then
          skipEmptyLines (count + 1) rem
        else
          count, nextLine, rem
  in
  List.rev (worker [] "" [0] lines)
  
let printIndentInfo classifiedLines =
  printf "---- Indent info -----\n";
  List.iter
    (fun (indentDiff, indent, unindentedLine) ->
       printf "%s%s %s\n"
         (String.make indent ' ')
         unindentedLine
         (match indentDiff with
            | `EmptyLine -> "-x-"
            | `SameIndent -> ""
            | `MoreIndent -> "->"
            | `LessIndent count -> "<- " ^ string_of_int count))
    classifiedLines;
  printf "-----------------------\n"
  
let printLines clines =
  let result : string list ref = ref [] in
  let (=<<) result newLine = result := newLine :: !result in
  
  let printSingleLine indent unindentedLine =
    result =<< sprintf "%s(%s)\n" (String.make indent ' ') unindentedLine
  in

  let rec printIndentBlock callerF lineNum = function
    | [] -> ()
    | (indentDiff, indent, unindentedLine) :: remLines ->
        let indentString = String.make indent ' ' in
        match indentDiff with
          | `EmptyLine ->
              result =<< sprintf "\n";
              printIndentBlock callerF (lineNum+1) remLines
                
          | `SameIndent ->
              printSingleLine indent unindentedLine;
              printIndentBlock callerF (lineNum+1) remLines
                
          | `MoreIndent ->
              if unindentedLine =~ multiBlockRE then begin
                let blockName = nthmatch 1 in
                let fixedLine = nthmatch 1 ^ nthmatch 2 in
                printMultiBlockFirstLine
                  blockName
                  (printIndentBlock callerF)
                  (lineNum+1)
                  indentString fixedLine
                  remLines
              end else begin
                result =<< sprintf "%s(%s (\n" indentString unindentedLine;
                printIndentBlock (printIndentBlock callerF) (lineNum+1) remLines
              end
                
          | `LessIndent (count :int) ->
              result =<< sprintf "%s(%s) %s\n" indentString unindentedLine (String.make count ')');
              callerF (lineNum+1) remLines

                
  and printMultiBlockFirstLine blockName callerF lineNum indentString unindentedLine lines =
    result =<< sprintf "%s(%s (\n" indentString unindentedLine;
    printIndentBlock (printMultiBlock blockName callerF) lineNum lines

      
  and printMultiBlock blockName callerF lineNum = function
    | [] ->
        raiseWSError lineNum (sprintf "EOF while expecting closing block '%s'" blockName)
          
    | (indentDiff, indent, unindentedLine) :: remLines ->
        let indentString = String.make indent ' ' in
        match indentDiff with
            
          | `EmptyLine ->
              result =<< sprintf "\n";
              printMultiBlock blockName callerF (lineNum+1) remLines
                
          | `MoreIndent ->
              if unindentedLine =~ blockEndRE blockName then begin
                raiseWSError lineNum (sprintf "Indenting after end of block not allowed")
              end else begin
                result =<< sprintf "%s%s (\n" indentString unindentedLine;
                printIndentBlock (printMultiBlock blockName callerF) (lineNum+1) remLines
              end
                
          | `LessIndent (count :int) ->
              raiseWSError lineNum
                (sprintf "Reduced indent while expecting closing block '%s' at line %d"
                   blockName lineNum)
                
          | `SameIndent ->
              if unindentedLine =~ blockEndRE blockName then begin
                result =<< sprintf "%s)\n" indentString;
                callerF (lineNum+1) remLines
              end else begin
                printSingleLine indent unindentedLine;
                printMultiBlock blockName callerF (lineNum+1) remLines
              end
  in
  
  let returnedToOften lineNum remLines =
    printf "\n%s:\n%s\n----\n"
      "Internal error. More unindenting than indenting, remaining lines"
       (Common.combine "\n" (List.map (fun (_, _, line) -> line) remLines));
    flush stdout;
    raiseWSError lineNum "More unindenting than indenting"
  in
  
  printIndentBlock returnedToOften 0 clines;
  List.rev !result

let addParens lines =
  let classifiedLines = classifyIndent lines in
  let lines = printLines classifiedLines in
  lines
  
