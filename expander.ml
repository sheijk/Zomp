(* #directory "../v3";; *)
(* #load "lexer2.cmo";; *)
(* #load "parser2.cmo";; *)
(* #load "lang.cmo";; *)
(* #load "ast2.cmo";; *)

open Lang
open Ast2
open Common

exception IllegalExpression of expression * string
let raiseIllegalExpression expr msg = raise (IllegalExpression (expr, msg))

type symbol =
  | VarSymbol of variable
  | FuncSymbol of func
  | UndefinedSymbol

type bindings = (string * symbol) list

let defaultBindings : bindings = []

let addVar bindings var : bindings = (var.vname, VarSymbol var) :: bindings

let addFunc bindings func : bindings = (func.fname, FuncSymbol func) :: bindings
  
let rec lookup (bindings :bindings) name =
  match bindings with
    | [] -> UndefinedSymbol
    | (symName, sym) :: tail when symName = name -> sym
    | _ :: tail -> lookup tail name

let isFunction bindings name =
  match lookup bindings name with
    | FuncSymbol _ -> true
    | _ -> false
        
        
type exprTranslateF = bindings -> expression -> bindings * expr list


let translateSeq (translateF : exprTranslateF) bindings = function
  | { id = "std:seq"; args = sequence } ->
      Some (translatelst translateF bindings sequence)
  | _ ->
      None

let translateDefineVar (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = "std_var"; args = [
        { id = typeName; args = [] };
        { id = name; args = [] };
        { id = valueString; args = [] };
      ] } -> begin
      let typ = string2integralType typeName in
      let value = parseValue typ valueString in
      let var = variable name typ value in
      Some( addVar bindings var, [ DefineVariable var ] )
    end
  | _ ->
      None

let translateFuncCall (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = name; args = args; } when isFunction bindings name ->
      let evalArg arg =
        match translateF bindings arg with
          | _, [expr] -> expr
          | _, exprList -> Sequence exprList
      in
      let argExprs = List.map evalArg args in
      Some( bindings, [ FuncCall { fcname = name; fcargs = argExprs } ] )
  | _ -> None
      
  
let translateSimpleExpr (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = name; args = [] } -> begin
      match lookup bindings name with
        | VarSymbol v -> Some (bindings, [Variable v.vname])
        | _ ->
            match string2integralValue name with
              | Some c -> Some( bindings, [Constant c] )
              | None -> None
    end
  | _ -> None

let translateLoop (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = "std_loop"; args = [preCode; abortTest; postCode] } -> begin
      let eval expr =
        let _, sf = translateF bindings expr in
        match sf with
            [e] -> e
          | _ as es -> Sequence es
      in
      Some (bindings, [Loop { preCode = eval preCode;
                                    abortTest = eval abortTest;
                                    postCode = eval postCode; } ])
    end
  | _ -> None

let translateIfThenElse (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = "std_ifthenelse"; args = [condExpr; trueExpr; falseExpr] } -> begin
      let eval expr =
        let _, sf = translateF bindings expr in
        match sf with
            [e] -> e
          | _ as es -> Sequence es
      in
      Some (bindings, [IfThenElse { cond = eval condExpr;
                                    trueCode = eval trueExpr;
                                    falseCode = eval falseExpr; }])
    end
  | _ -> None      

let translateNested = translate raiseIllegalExpression
  [
    translateSeq;
    translateDefineVar;
    translateSimpleExpr;
    translateFuncCall;
    translateLoop;
    translateIfThenElse;
  ]
  
(* let rec translateNested = function *)
(*   | Expr ("std:seq", sequence) -> *)
(*       List.fold_left (@) [] (List.map translateNested sequence) *)
  
(*   | Expr ( "std_var", [Expr(typeName, []); Expr(name, []); Expr(valueString, [])] ) -> *)
(*       let typ = string2integralType typeName in *)
(*       let value = parseValue typ valueString in *)
(*       [ DefineVariable (variable name typ value) ] *)

(*   | _ as expr -> raiseIllegalExpression ~expr ~msg:"Not handled, yet" *)

type toplevelExprTranslateF = bindings -> expression -> bindings * toplevelExpr list

let translateGlobalVar (translateF : toplevelExprTranslateF) (bindings :bindings) = function
  | { id = "std_var"; args = [
        { id = typeName; args = [] };
        { id = name; args = [] };
        { id = valueString; args = [] }
      ] } -> begin
      let typ = string2integralType typeName in
      let value = parseValue typ valueString in
      let var = variable name typ value in
      let newBindings = addVar bindings var in
      Some( newBindings, [ GlobalVar var ] )
    end
  | _ ->
      None

let translateFunc (translateF : toplevelExprTranslateF) (bindings :bindings) = function
  | { id = "std_func"; args = [
        { id = typeName; args = [] };
        { id = name; args = [] };
        { id = "std:seq"; args = paramExprs };
        { id = "std:seq"; args = _ } as implExpr;
      ] } -> begin
      let typ = string2integralType typeName in
      let expr2param = function
        | { id = typeName; args = [{ id = varName; args = [] }] } ->
            (varName, string2integralType typeName)
        | _ as expr ->
            raiseIllegalExpression expr "Expected 'typeName varName' for param"
      in
      let params = List.map expr2param paramExprs in
      let rec localBinding bindings = function
        | [] -> bindings
        | (name, typ) :: tail ->
            let var = variable name typ (defaultValue typ) in
            localBinding (addVar bindings var) tail
      in
      let innerBindings = localBinding bindings params in
      let _, impl = translateNested innerBindings implExpr in
      let f = func name typ params (Sequence impl) in
      let newBindings = addFunc bindings f in
      Some( newBindings, [ DefineFunc f ] )
    end
  | _ ->
      None
        
let translateTL = translate raiseIllegalExpression [translateGlobalVar; translateFunc]
  
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
    [GlobalVar (variable "foo" Int (IntVal 10))];

    (** test functions *)
    "std_func float five {} {};",
    [DefineFunc (func "five" Float [] (Sequence []))];

    (** test parameters and local variables *)
    "std_func int plus {int l; int r;} { std_var int t 0; };",
    [DefineFunc (func "plus" Int
                   ["l", Int; "r", Int]
                   (Sequence [
                      DefineVariable (variable "t" Int (IntVal 0));
                    ]))];

    (** test global bindings *)
    "std_var int bar 10;" ^
      "std_func int getbar {} { bar; };",
    [GlobalVar (variable "bar" Int (IntVal 10));
     DefineFunc (func "getbar" Int [] (Sequence [Variable "bar"]))];

    (** test constant expressions *)
    "std_func int one {} { 1; };",
    [DefineFunc (func "one" Int [] (Sequence [Constant (IntVal 1)]))];
    
    "std_func string hello {} { \"hello\"; };",
    [DefineFunc (func "hello" String [] (Sequence [Constant (StringVal "hello")]))];
    (*TODO: bool, float *)

    (** test whether parameters can be accessed *)
    "std_func int id {int n;} { n; };",
    [DefineFunc (func "id" Int ["n", Int] (Sequence [Variable "n"]))];

    (** test function calls *)
    "std_func int five {int x;} { 5; };"
    ^ "std_func int five2 {} { five 3; };",
    [DefineFunc (func "five" Int ["x", Int] (Sequence [Constant (IntVal 5)]));
     DefineFunc (func "five2" Int []
                   (Sequence [FuncCall {
                                fcname = "five";
                                fcargs = [Constant (IntVal 3)] }]) ) ];

    "std_func int testif {} {" ^
      "  std_ifthenelse { true; } { 1; } { 2; };" ^
      "};",
    [DefineFunc (func "testif" Int []
                   (Sequence [
                      IfThenElse ({ cond = Constant (BoolVal true);
                                    trueCode = Constant (IntVal 1);
                                    falseCode = Constant (IntVal 2) })]) )];

    "std_func float testloop {} {" ^
      "  std_loop {} { false; } { \"while\"; };" ^
      "  std_loop { \"do\"; } { true; } {};" ^
      "};",
    [DefineFunc (func "testloop" Float []
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
  
