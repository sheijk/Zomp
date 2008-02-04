
open Printf

let recreateLines fromPos text =
  let textLength = String.length text in
  let rec worker pos currentLine =
    if pos >= textLength then
      [currentLine]
    else if text.[pos] = '\n' then
      currentLine :: worker (pos+1) ""
    else
      worker (pos+1) (currentLine ^ String.make 1 text.[pos])
  in
  worker fromPos ""
  
let () =
  try
    let rec parse prevLines =
      flush stdout;
      let line = input_line stdin in
      let lines = prevLines @ [line] in
      let parentedLines = Iexpr.addParens (lines @ [""]) in
      let input = Common.combine "\n" parentedLines in
      let lexbuf = Lexing.from_string input in
      
      printf "lines ->\n";
      List.iter (fun line -> printf "%s\n" line) lines;
      printf "parented ->\n%s\n---\n" input; flush stdout;
      
      try
        let expr = Newparser.main Newlexer.token lexbuf in
        printf "=> %s\n" (Ast2.toString expr); flush stdout;
        (* let pos = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum in *)
        (* let inputLength = String.length input in *)
        (* if pos < inputLength then *)
        (* printf "Warning: ignored %d chars\n" (inputLength - pos); *)
        parse []
            (* parse (recreateLines pos (Common.combine "\n" lines)) *)
      with
        | Newlexer.Eof ->
            parse (prevLines @ lines)
        | _ as e ->
            raise e
    in
    parse []
  with Newlexer.Eof ->
    printf "Eof\n"

      
