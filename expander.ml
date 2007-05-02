(* #directory "../v3";; *)
(* #load "lexer2.cmo";; *)
(* #load "parser2.cmo";; *)
(* #load "lang.cmo";; *)
(* #load "ast2.cmo";; *)

open Lang
open Ast2

exception IllegalExpression of expression * string
let raiseIllegalExpression ~expr ~msg = raise (IllegalExpression (expr, msg))

type symbol =
  | VarSymbol of variable
  | FuncSymbol of func
  | UndefinedSymbol

type bindings = (string * symbol) list

let defaultBindings : bindings = []
  
let rec lookup bindings name =
  match bindings with
    | [] -> UndefinedSymbol
    | (symName, sym) :: tail when symName = name -> sym
    | _ :: tail -> lookup tail name

type exprTranslateF = expression -> expr list
        
let translateSeq (translateF : exprTranslateF) = function
  | { id = "std:seq"; args = sequence } ->
      Some (List.fold_left (@) [] (List.map translateF sequence))
  | _ ->
      None

let translateVar (translateF : exprTranslateF) = function
  | { id = "std_var"; args = [
        { id = typeName; args = [] };
        { id = name; args = [] };
        { id = valueString; args = [] };
      ] } -> begin
      let typ = string2integralType typeName in
      let value = parseValue typ valueString in
      Some [ DefineVariable (variable name typ value) ]
    end
  | _ ->
      None
    
let rec translate translators  expr =
  let rec t = function
    | [] -> raiseIllegalExpression ~expr ~msg:"Expression can not be translated"
    | f :: remf -> begin
        match f (translate translators) expr with
          | Some result -> result
          | None -> t remf
      end
  in
  t translators

let translateNested = translate [translateSeq; translateVar]
  
(* let rec translateNested = function *)
(*   | Expr ("std:seq", sequence) -> *)
(*       List.fold_left (@) [] (List.map translateNested sequence) *)
        
(*   | Expr ( "std_var", [Expr(typeName, []); Expr(name, []); Expr(valueString, [])] ) -> *)
(*       let typ = string2integralType typeName in *)
(*       let value = parseValue typ valueString in *)
(*       [ DefineVariable (variable name typ value) ] *)

(*   | _ as expr -> raiseIllegalExpression ~expr ~msg:"Not handled, yet" *)

type toplevelExprTranslateF = expression -> toplevelExpr list

let translateGlobalVar (translateF : toplevelExprTranslateF) = function
  | { id = "std_var"; args = [
        { id = typeName; args = [] };
        { id = name; args = [] };
        { id = valueString; args = [] }
      ] } -> begin
      let typ = string2integralType typeName in
      let value = parseValue typ valueString in
      Some [ GlobalVar (variable name typ value) ]
    end
  | _ ->
      None

let translateFunc (translateF : toplevelExprTranslateF) = function
  | { id = "std_func"; args = [
        { id = typeName; args = [] };
        { id = name; args = [] };
        { id = "std:seq"; args = paramExprs };
        { id = "std:seq"; args = implExprs };
      ] } -> begin
      let typ = string2integralType typeName in
      let expr2param = function
        | { id = typeName; args = [{ id = varName; args = [] }] } ->
            (varName, string2integralType typeName)
        | _ as expr -> raiseIllegalExpression ~expr
            ~msg:"Expected 'typeName varName' for param"
      in
      let params = List.map expr2param paramExprs in
      let impl = List.fold_left (@) [] (List.map translateNested implExprs) in
      Some [ DefineFunc (func name typ params (Sequence impl)) ]
    end
  | _ ->
      None
    
let translateTL = translate [translateGlobalVar; translateFunc]
  
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
  let tls = List.fold_left (@) [] (List.map translateTL exprs) in
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
  
