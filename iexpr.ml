
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
  
let () =
  let sourceFile = "indent.zomp" in
  let revLines =
    withFileIn sourceFile
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
  
  let rec classifyIndent acc activeIndents = function
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
            `MoreIndent, (nextIndent :: activeIndents)
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
        classifyIndent
          ((indentDiff, prevIndent, prevUnindented) :: acc)
          newActiveIndents
          (nextLine :: rem)
  in

  (*   List.iter (fun line -> printf "%s\n" line) revLines; *)
  
  let linesWithIndentInfoRev = classifyIndent [] [0] ("" :: List.rev revLines) in

(*   iterBack *)
(*     (fun (indentDiff, indent, unindentedLine) -> *)
(*        printf "%s%s %s\n" *)
(*          (String.make indent ' ') *)
(*          unindentedLine *)
(*          (match indentDiff with *)
(*             | `SameIndent -> "" *)
(*             | `MoreIndent -> "->" *)
(*             | `LessIndent count -> "<- " ^ string_of_int count)) *)
(*     linesWithIndentInfoRev; *)

  let rec bracketLines acc = function
    | [] ->
        acc
    | (indentDiff, indent, unindentedLine) :: rem ->
        let indentString = String.make indent ' ' in
        let terminator =
          match indentDiff with
            | `SameIndent -> ")"
            | `MoreIndent -> " ("
            | `LessIndent count -> ") " ^ String.make (2 * count) ')'
        in
        let line = indentString ^ "(" ^ unindentedLine ^ terminator in
        bracketLines (line :: acc) rem
  in

  let bracketedLines = bracketLines [] linesWithIndentInfoRev in

  List.iter (printf "%s\n") bracketedLines

          
(* let () = *)
(*   let sourceFile = "indent.zomp" in *)
(*   withFileIn sourceFile *)
(*     (fun inStream -> *)
(*        let outStream = stdout in *)
(*        let printString = output_string outStream *)
(*        and printChar = output_char outStream *)
(*        in *)
(*        try *)
(*          let rec processLine previousIndent = *)
(*            let line = input_line inStream in *)
(*            let indent = *)
(*              match classifyLine line with *)
(*                | `EmptyLine -> *)
(*                    previousIndent *)
(*                | `IndentedLine (indent, unindentedLine) -> *)
(*                    let leadingWhitespace = String.make indent ' ' in *)
(*                    let printLine() = *)
(*                      printString leadingWhitespace; *)
(*                      printChar '('; *)
(*                      printString unindentedLine; *)
(*                    in *)
(*                    if indent = previousIndent then begin *)
(*                      printString ")\n"; *)
(*                      printLine(); *)
(*                    end else if indent > previousIndent then begin *)
(*                      printString " (\n"; *)
(*                      printLine(); *)
(*                    end else if indent < previousIndent then begin *)
(*                      printString "[\n"; *)
(*                      printLine(); *)
(*                      printString " >"; *)
(*                    end; *)
(*                    indent *)
(*            in *)
(*            printString "("; *)
(*            processLine indent; *)
(*            printChar '\n'; *)
(*          in *)
(*          processLine 0 *)
(*        with End_of_file -> () *)
(*     ); *)
(*   flush stdout *)

