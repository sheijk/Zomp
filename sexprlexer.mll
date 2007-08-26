{
  open Sexprparser
  exception Eof
  exception UnknowChar of char
}

let whitespace = [' ' '\t' '\n']*

let identifierChar = [':' 'a'-'z' '0'-'9' 'A'-'Z' '_' '.']

rule token = parse
  | whitespace
      { token lexbuf }
  | '('
      { PAREN_OPEN }
  | ')'
      { PAREN_CLOSE }
  | (identifierChar+ as id) | (('-' identifierChar+) as id)
      { IDENTIFIER(id) }
  | eof
      { raise Eof }
  | _ as c
      { raise (UnknowChar c) }
(*   | ';' *)
(*       { END_SYMBOL } *)
    
