{
  open Sexprparser
  exception Eof
  exception UnknowChar of char
}

let whitespace = [' ' '\t' '\n']*

rule token = parse
  | whitespace
      { token lexbuf }
  | '('
      { PAREN_OPEN }
  | ')'
      { PAREN_CLOSE }
  | ([':' 'a'-'z' '0'-'9' 'A'-'Z' '_' '.']+ as id)
      { IDENTIFIER(id) }
  | eof
      { raise Eof }
  | _ as c
      { raise (UnknowChar c) }
(*   | ';' *)
(*       { END_SYMBOL } *)
    
