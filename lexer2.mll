(* File lexer.mll *)
{
  open Parser2
  exception Eof
}

let whitespace = ['\n' ' ' '\t']*

rule token = parse
    whitespace ';'
      { SEPERATOR }
  | whitespace '{' | whitespace '('
      { BLOCK_BEGIN }
  | whitespace '}' | whitespace ')'
      { BLOCK_END }
  | whitespace (('"' [^'\"']* '"') as str)
      { IDENTIFIER(str) }
  | whitespace (['a'-'z' '0'-'9' 'A'-'Z' '=' '+' '-' '*' '/' '_']+ as id) 
      { IDENTIFIER(id) }
  | whitespace '!'
      { raise Eof }
  | whitespace eof
      { raise Eof }

