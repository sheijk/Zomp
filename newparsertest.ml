
open Printf


let readBlock channel =
  let rec readLine lineAcc =
    flush stdout;
    let line = input_line channel in
    if line = "" then begin
      let line2 = input_line channel in
      if line2 = "" then
        line2 :: lineAcc
      else
        readLine (line :: line2 :: lineAcc)
    end else begin
      readLine (line :: lineAcc)
    end
  in
  Common.combine "\n" (List.rev (readLine []))

let tokenToString = function
  | Newparser.END -> "`nl"
  | Newparser.IDENTIFIER id -> id
  | Newparser.BLOCK_BEGIN -> "{"
  | Newparser.BLOCK_END [] -> "}"
  | Newparser.BLOCK_END params -> sprintf "}(%s)" (Common.combine ", " params)
  | Newparser.WHITESPACE count -> String.make count '_'
      
let () =
  let rec parse() =
    flush stdout;
    let block = readBlock stdin in
    (* printf "block:\n%s---\n" block; *)
    let lexbuf = Lexing.from_string block in
    begin try while true do
      (* let t = Newlexer.token lexbuf in *)
      (* printf "%s " (tokenToString t); *)
      let expr = Newparser.main Newlexer.token lexbuf in
      printf "=>\n%s\n---\n" (Ast2.toString expr)
    done with
      | End_of_file -> ()
      | Newparser.Error -> printf "Parser error\n"
    end;
    parse()
  in
  parse()
      
      
(* let recreateLines fromPos text = *)
(*   let textLength = String.length text in *)
(*   let rec worker pos currentLine = *)
(*     if pos >= textLength then *)
(*       [currentLine] *)
(*     else if text.[pos] = '\n' then *)
(*       currentLine :: worker (pos+1) "" *)
(*     else *)
(*       worker (pos+1) (currentLine ^ String.make 1 text.[pos]) *)
(*   in *)
(*   worker fromPos "" *)
  
(* let () = *)
(*   try *)
(*     let rec parse prevLines = *)
(*       flush stdout; *)
(*       let line = input_line stdin in *)
(*       let lines = prevLines @ [line] in *)
(*       let parentedLines = Iexpr.addParens (lines @ [""]) in *)
(*       let input = Common.combine "\n" parentedLines in *)
(*       let lexbuf = Lexing.from_string input in *)
      
(*       printf "lines ->\n"; *)
(*       List.iter (fun line -> printf "%s\n" line) lines; *)
(*       printf "parented ->\n%s\n---\n" input; flush stdout; *)
      
(*       try *)
(*         let expr = Newparser.main Newlexer.token lexbuf in *)
(*         printf "=> %s\n" (Ast2.toString expr); flush stdout; *)
(*         (\* let pos = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum in *\) *)
(*         (\* let inputLength = String.length input in *\) *)
(*         (\* if pos < inputLength then *\) *)
(*         (\* printf "Warning: ignored %d chars\n" (inputLength - pos); *\) *)
(*         parse [] *)
(*             (\* parse (recreateLines pos (Common.combine "\n" lines)) *\) *)
(*       with *)
(*         | Newlexer.Eof -> *)
(*             parse (prevLines @ lines) *)
(*         | _ as e -> *)
(*             raise e *)
(*     in *)
(*     parse [] *)
(*   with Newlexer.Eof -> *)
(*     printf "Eof\n" *)

      
