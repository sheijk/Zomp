(* File lexer.mll *)
{
  open Parser2
  exception Eof
}

let whitespace = ['\n' ' ' '\t']*
let operator = ['+' '-' '*' '/']
let opsuffix = (['a'-'z']+)?

rule token = parse
  | whitespace
      { token lexbuf }
  | ';'
      { SEPERATOR }
  | '{' | '('
      { BLOCK_BEGIN }
  | '}' | ')'
      { BLOCK_END }
  | '+' opsuffix as name
      { OP_PLUS(name) }
  | ("op" operator) as name
      { IDENTIFIER(name) }
  | (('"' [^'\"']* '"') as str)
      { IDENTIFIER(str) }
  | (['a'-'z' '0'-'9' 'A'-'Z' '_' '.']+ as id)
      { IDENTIFIER(id) }
  | "//" [^'\n']* '\n'
      { token lexbuf }
  |  "/*"
      { mlcomment lexbuf }
  | '!'
      { raise Eof }
  | eof
      { raise Eof }
and mlcomment = shortest 
  | (_)* "*/"
      { token lexbuf }

