
TYPE_CONV_PATH "Sexplib"
  
open Printf

type foo = A | B
with sexp
    
let foo_to_string f = Sexplib.Conv.string_of_sexp (sexp_of_foo f)
  
let _ =
  printf "hello\n";
  printf "%s\n" (foo_to_string B);
  flush stdout;
  ()
    
