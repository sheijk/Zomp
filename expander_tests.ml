
open Expander
open Common
open Lang
open Genllvm
  
let parseSources sources =
  let lexbuf = Lexing.from_string sources in
  let rec collect lexbuf exprs =
    try
      let e = Parser2.main Lexer2.token lexbuf in
      e :: collect lexbuf exprs
    with
        _ -> exprs
  in
  let expressions = collect lexbuf [] in
  expressions

let source2tl source =
  let exprs = parseSources source in
  translatelst translateTL defaultBindings exprs
    
let _ =
  let testcases = [
    (** test global variables *)
    "std_var int foo 10;",
    [GlobalVar (variable "foo" `Int (IntVal 10))];

    (** test functions *)
    "std_func float five {} {};",
    [DefineFunc (func "five" `Float [] (Some (Sequence [])))];

    (** test parameters and local variables *)
    "std_func int plus {int l; int r;} { std_var int t 0; };",
    [DefineFunc (func "plus" `Int
                   ["l", `Int; "r", `Int]
                   (Some (Sequence [
                      DefineVariable (variable "t" `Int (IntVal 0));
                    ])))];

    (** test global bindings *)
    "std_var int bar 10;" ^
      "std_func int getbar {} { bar; };",
    (let v = (variable "bar" `Int (IntVal 10)) in
    [GlobalVar v;
     DefineFunc (func "getbar" `Int [] (Some (Sequence [Variable v])))]);

    (** test constant expressions *)
    "std_func int one {} { 1; };",
    [DefineFunc (func "one" `Int [] (Some (Sequence [Constant (IntVal 1)])))];
    
    "std_func string hello {} { \"hello\"; };",
    [DefineFunc (func "hello" `String [] (Some (Sequence [Constant (StringVal "hello")])))];
    (*TODO: bool, float *)

    (** test whether parameters can be accessed *)
    "std_func int id {int n;} { n; };",
    [DefineFunc (funcDef "id" `Int ["n", `Int]
                   (Sequence [Variable (variable "n" `Int (defaultValue `Int)) ]))];

    (** test function calls *)
    "std_func int five {int x;} { 5; };"
    ^ "std_func int five2 {} { five 3; };",
    [DefineFunc (funcDef "five" `Int ["x", `Int] (Sequence [Constant (IntVal 5)]));
     DefineFunc (funcDef "five2" `Int []
                   (Sequence [FuncCall {
                                fcname = "five";
                                fcrettype = `Int;
                                fcparams = [`Int];
                                fcargs = [Constant (IntVal 3)] }]) ) ];

    "std_func int testif {} {" ^
      "  std_ifthenelse { true; } { 1; } { 2; };" ^
      "};",
    [DefineFunc (funcDef "testif" `Int []
                   (Sequence [
                      IfThenElse ({ cond = Constant (BoolVal true);
                                    trueCode = Constant (IntVal 1);
                                    falseCode = Constant (IntVal 2) })]) )];

    "std_func float testloop {} {" ^
      "  std_loop {} { false; } { \"while\"; };" ^
      "  std_loop { \"do\"; } { true; } {};" ^
      "};",
    [DefineFunc (funcDef "testloop" `Float []
                   (Sequence [
                      Loop { preCode = Sequence [];
                             abortTest = Constant (BoolVal false);
                             postCode = Constant (StringVal "while"); };
                      Loop { preCode = Constant (StringVal "do");
                             abortTest = Constant (BoolVal true);
                             postCode = Sequence []; } ]))];
  ]
  in
  let source2res source =
    try
      let _, tl = (source2tl source) in
      `Result tl
    with _ as e ->
      `Exception e
  in
  let rec test = function
    | [] -> []
    | (source, expected) :: tail -> begin
        let result = source2res source in
        match result with
          | `Result r when r = expected -> test tail
          | _ -> (`Source source, `Expected expected, result) :: test tail
      end
  in
  match test testcases with
    | [] -> `NoErrors
    | _ as errorList -> `Errors errorList

        
