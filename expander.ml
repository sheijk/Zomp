(* #directory "../v3";; *)
(* #load "lexer2.cmo";; *)
(* #load "parser2.cmo";; *)
(* #load "lang.cmo";; *)
(* #load "ast2.cmo";; *)
(* #load "common.cmo";; *)
(* #load "bindings.cmo";; *)

open Lang
open Ast2
open Common
open Bindings

exception IllegalExpression of expression * string
let raiseIllegalExpression expr msg = raise (IllegalExpression (expr, msg))

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
  | { id = name; args = args; } ->
      match lookup bindings name with
        | FuncSymbol func ->
            let evalArg arg =
              match translateF bindings arg with
                | _, [expr] -> expr
                | _, exprList -> Sequence exprList
            in
            let argExprs = List.map evalArg args in
            Some( bindings, [ FuncCall {
                                fcname = name;
                                fcrettype = func.rettype;
                                fcparams = List.map snd func.fargs;
                                fcargs = argExprs;
                              } ] )
        | _ -> None
  
let translateSimpleExpr (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = name; args = [] } -> begin
      match lookup bindings name with
        | VarSymbol v -> Some (bindings, [Variable v])
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
  
  
