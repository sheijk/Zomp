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

let macroVar = "var"
and macroFunc = "func"
and macroIfThenElse = "ifthenelse"
and macroLoop = "loop"

exception IllegalExpression of expression * string
let raiseIllegalExpression expr msg = raise (IllegalExpression (expr, msg))

type typecheckResult =
  | TypeOf of integralType
      (** error message, found type expected type *)
  | TypeError of string * integralType * integralType
      
let rec typeOfTL = function
  | GlobalVar var -> TypeOf var.typ
  | DefineFunc f ->
      match typeOf f.impl with
        | TypeOf implType when implType = f.rettype -> TypeOf f.rettype
        | TypeOf wrongType -> TypeError (
            "Function's return type is not equal to it's implementation",
            f.rettype,
            wrongType)
        | TypeError _ as e -> e
and typeOf = function
  | Sequence [] -> TypeOf Void
  | Sequence [expr] -> typeOf expr
  | Sequence (_ :: tail) -> typeOf (Sequence tail)
  | DefineVariable var -> TypeOf var.typ
  | Variable var -> TypeOf var.typ
  | Constant value -> TypeOf (integralValue2Type value)
  | FuncCall call -> TypeOf call.fcrettype
  | IfThenElse { cond = cond; trueCode = trueCode; falseCode = falseCode } -> begin
      match typeOf cond with
        | TypeOf Bool -> begin
            match typeOf trueCode, typeOf falseCode with
              | (TypeError _) as e, _ -> e
              | _, ((TypeError _) as e) -> e
              | trueType, falseType when trueType = falseType -> trueType
              | TypeOf trueType, TypeOf falseType -> TypeError ("Types don't match in if/then/else", trueType, falseType)
          end
        | TypeOf condType -> TypeError (
            "Expected cond of type Bool in 'if cond then ... else ..'",
            condType, Bool)
        | TypeError _ as e -> e
    end
  | Loop l ->
      match typeOf l.abortTest with
        | TypeOf Bool -> begin
            match typeOf l.preCode with
              | TypeOf Void -> typeOf l.postCode
              | TypeOf wrongType -> TypeError ("PreCode in loop must have type void", wrongType, Void)
              | _ as e -> e
          end
        | TypeOf wrongType -> TypeError ("Abort condition in loop must have type Bool", wrongType, Bool)
        | _ as e -> e
  
type exprTranslateF = bindings -> expression -> bindings * expr list

let translateSeq (translateF : exprTranslateF) bindings = function
  | { id = "std:seq"; args = sequence } ->
      Some (translatelst translateF bindings sequence)
  | _ ->
      None

let translateDefineVar (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [
        { id = typeName; args = [] };
        { id = name; args = [] };
        { id = valueString; args = [] };
      ] } when id = macroVar -> begin
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
  | { id = id; args = [preCode; abortTest; postCode] } when id = macroLoop -> begin
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
  | { id = id; args = [condExpr; trueExpr; falseExpr] } when id = macroIfThenElse -> begin
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
  

type toplevelExprTranslateF = bindings -> expression -> bindings * toplevelExpr list

let translateGlobalVar (translateF : toplevelExprTranslateF) (bindings :bindings) = function
  | { id = id; args = [
        { id = typeName; args = [] };
        { id = name; args = [] };
        { id = valueString; args = [] }
      ] } when id = macroVar -> begin
      let typ = string2integralType typeName in
      let value = parseValue typ valueString in
      let var = variable name typ value in
      let newBindings = addVar bindings var in
      Some( newBindings, [ GlobalVar var ] )
    end
  | _ ->
      None

let translateFunc (translateF : toplevelExprTranslateF) (bindings :bindings) = function
  | { id = id; args = [
        { id = typeName; args = [] };
        { id = name; args = [] };
        { id = "std:seq"; args = paramExprs };
        { id = "std:seq"; args = _ } as implExpr;
      ] } as expr when id = macroFunc -> begin
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
      let funcDef = DefineFunc f in
      match typeOfTL funcDef with
        | TypeOf _ -> Some( newBindings, [ funcDef ] )
        | TypeError (msg, declaredType, returnedType) ->
            raiseIllegalExpression
              expr
              (Printf.sprintf "Function has return type %s but returns %s"
                      (integralType2String declaredType)
                      (integralType2String returnedType))
    end
  | _ ->
      None
        
let translateTL = translate raiseIllegalExpression [translateGlobalVar; translateFunc]
  
  
