#directory "../v3";;
#load "lexer2.cmo";;
#load "parser2.cmo";;
#load "lang.cmo";;
#load "ast2.cmo";;

open Lang
open Ast2

exception IllegalExpression of expression * string
let raiseIllegalExpression ~expr ~msg = raise (IllegalExpression (expr, msg))

let translateNested = function
  | Expr ( "std_var", [Expr(typeName, []); Expr(name, []); Expr(valueString, [])] ) ->
      let typ = string2integralType typeName in
      let value = parseValue typ valueString in
      DefineVariable (variable name typ value)
        
  | _ as expr -> raiseIllegalExpression ~expr ~msg:"Not handled, yet"
    
let translateTL = function
  | Expr ( "std_var", [Expr(typeName, []); Expr(name, []); Expr(valueString, [])] ) ->
      let typ = string2integralType typeName in
      let value = parseValue typ valueString in
      GlobalVar (variable name typ value)
        
  | Expr ( "std_func", [
             Expr(typeName, []); Expr(name, []);
             Expr("std:seq", paramExprs);
             Expr("std:seq", implExprs);
           ] ) ->
      let typ = string2integralType typeName in
      let expr2param = function
        | Expr( typeName, [Expr(varName, [])] ) -> (varName, string2integralType typeName)
        | _ as expr -> raiseIllegalExpression ~expr ~msg:"Expected 'typeName varName' for param"
      in
      let params = List.map expr2param paramExprs in
      let impl = List.map translateNested implExprs in
      DefineFunc (func name typ params (Sequence impl))
        
  | _ as expr -> raiseIllegalExpression ~expr ~msg:"Not handled, yet"
    
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
  let tls = List.map translateTL exprs in
  tls
  
let _ =
  let testcases = [
    "std_var int foo 10;",
    [GlobalVar (variable "foo" Int (IntVal 10))];

    "std_func float five {} {};",
    [DefineFunc (func "five" Float [] (Sequence []))];

    "std_func int plus {int l; int r;} { std_var int t 0; };",
    [DefineFunc (func "plus" Int
                   ["l", Int; "r", Int]
                   (Sequence [
                      DefineVariable (variable "t" Int (IntVal 0));
                     ]))];
  ]
  in
  let rec test = function
    | [] -> []
    | (source, expected) :: tail -> begin
        let result = source2tl source in
        if result = expected then
          test tail
        else
          (source, expected, result) :: test tail
      end
  in
  match test testcases with
    | [] -> `NoErrors
    | _ as errorList -> `Errors errorList
  
