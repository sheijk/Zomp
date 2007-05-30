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
and macroAssign = "assign"

exception IllegalExpression of expression * string
let raiseIllegalExpression expr msg = raise (IllegalExpression (expr, msg))

type typecheckResult =
  | TypeOf of composedType
      (** error message, found type, expected type *)
  | TypeError of string * composedType * composedType
      
let rec typeOf = function
  | Sequence [] -> TypeOf `Void
  | Sequence [expr] -> typeOf expr
  | Sequence (_ :: tail) -> typeOf (Sequence tail)
  | DefineVariable (var, expr) -> begin
      match typeOf expr with
        | TypeOf exprType when exprType = var.typ -> TypeOf var.typ
        | TypeOf wrongType -> TypeError (
            "Types need to be the same in assignment", wrongType, var.typ)
        | _ as e -> e
    end      
  | Variable var -> TypeOf var.typ
  | Constant value -> TypeOf (integralValue2Type value)
  | FuncCall call -> TypeOf (call.fcrettype :> composedType)
(*   | AssignVar (v, expr) -> begin *)
(*       match typeOf expr with *)
(*         | TypeOf exprType when exprType = v.typ -> TypeOf `Void *)
(*         | TypeOf exprType -> TypeError ( *)
(*             "Cannot assign result of expression to var because types differ", *)
(*             exprType, v.typ) *)
(*         | _ as typeError -> typeError *)
(*     end *)
  | IfThenElse { cond = cond; trueCode = trueCode; falseCode = falseCode } -> begin
      match typeOf cond with
        | TypeOf `Bool -> begin
            match typeOf trueCode, typeOf falseCode with
              | (TypeError _) as e, _ -> e
              | _, ((TypeError _) as e) -> e
              | trueType, falseType when trueType = falseType -> trueType
              | TypeOf trueType, TypeOf falseType -> TypeError ("Types don't match in if/then/else", trueType, falseType)
          end
        | TypeOf condType -> TypeError (
            "Expected cond of type Bool in 'if cond then ... else ..'",
            condType, `Bool)
        | TypeError _ as e -> e
    end
  | Loop l ->
      match typeOf l.abortTest with
        | TypeOf `Bool -> begin
            match typeOf l.preCode with
              | TypeOf `Void -> typeOf l.postCode
              | TypeOf wrongType -> TypeError ("PreCode in loop must have type void", wrongType, `Void)
              | _ as e -> e
          end
        | TypeOf wrongType -> TypeError ("Abort condition in loop must have type Bool", wrongType, `Bool)
        | _ as e -> e
  
let rec typeOfTL = function
  | GlobalVar var -> TypeOf var.typ
  | DefineFunc f ->
      match f.impl with
        | None -> TypeOf f.rettype
        | Some impl ->
            match typeOf impl with
              | TypeOf implType when implType = f.rettype -> TypeOf f.rettype
              | TypeOf wrongType -> TypeError (
                  "Function's return type is not equal to it's implementation",
                  f.rettype,
                  wrongType)
              | TypeError _ as e -> e
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
        valueExpr
      ] } when id = macroVar -> begin
      let _, simpleform = translateF bindings valueExpr in
      let typ = string2integralType typeName in
(*       let value = parseValue typ valueString in *)
      let value = defaultValue typ in
      let var = localVar name typ value in
      Some( addVar bindings var, [ DefineVariable (var, Sequence simpleform) ] )
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

let translateMacro (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = macroName; args = args; } ->
      match lookup bindings macroName with
        | MacroSymbol macro ->
            let transformedExpr = macro.mtransformFunc args in
            Some (translateF bindings transformedExpr)
        | _ -> None
            
let translateSimpleExpr (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = name; args = [] } -> begin
      match lookup bindings name with
        | VarSymbol v ->
            Some (bindings, [Variable v])
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

(* let translateAssignVar (translateF :exprTranslateF) (bindings :bindings) = function *)
(*   | { id = id; args = [ *)
(*         { id = varName; args = [] }; *)
(*         rightHandExpr; *)
(*       ] } when id = macroAssign -> begin *)
(*       let _, rightHandSimpleform = translateF bindings rightHandExpr in *)
(*       match lookup bindings varName with *)
(*         | VarSymbol v -> *)
(*             Some (bindings, [AssignVar (v, Sequence rightHandSimpleform)]) *)
(*         | _ -> None *)
(*     end *)
(*   | _ -> None *)
  
let translateNested = translate raiseIllegalExpression
  [
    translateSeq;
    translateDefineVar;
    translateSimpleExpr;
    translateFuncCall;
    translateLoop;
    translateIfThenElse;
    translateMacro;
(*     translateAssignVar; *)
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
      let var = globalVar name typ value in
      let newBindings = addVar bindings var in
      Some( newBindings, [ GlobalVar var ] )
    end
  | _ ->
      None

let translateFunc (translateF : toplevelExprTranslateF) (bindings :bindings) expr =
  let buildFunction typeName name paramExprs implExprOption =
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
          let var = localVar name typ (defaultValue typ) in
          localBinding (addVar bindings var) tail
    in
    let innerBindings = localBinding bindings params in
    let impl = match implExprOption with
      | Some implExpr -> Some (Sequence (snd (translateNested innerBindings implExpr)))
      | None -> None
    in
    let f = func name typ params impl in
    let newBindings = addFunc bindings f in
    let funcDef = DefineFunc f in
    newBindings, funcDef
  in
  match expr with
    | { id = id; args = [
          { id = typeName; args = [] };
          { id = name; args = [] };
          { id = "std:seq"; args = paramExprs };
          { id = "std:seq"; args = _ } as implExpr;
        ] } when id = macroFunc ->
        begin
          let newBindings, funcDef = buildFunction typeName name paramExprs (Some implExpr) in
          match typeOfTL funcDef with
            | TypeOf _ -> Some( newBindings, [ funcDef ] )
            | TypeError (msg, declaredType, returnedType) ->
                raiseIllegalExpression
                  expr
                  (Printf.sprintf "Function has return type %s but returns %s"
                     (composedType2String declaredType)
                     (composedType2String returnedType))
        end
    | { id = id; args = [
          { id = typeName; args = [] };
          { id = name; args = [] };
          { id = "std:seq"; args = paramExprs };
        ] } when id = macroFunc ->
        let newBindings, funcDecl = buildFunction typeName name paramExprs None in
        Some (newBindings, [funcDecl])
    | _ ->
        None
        
let translateTL = translate raiseIllegalExpression [translateGlobalVar; translateFunc]
  
  
