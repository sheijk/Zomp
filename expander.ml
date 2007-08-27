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
and macroMutableVar = "mvar"
and macroFunc = "func"
and macroIfThenElse = "ifthenelse"
and macroLoop = "loop"
and macroAssign = "assign"
and macroSequence = "std:seq"
and macroTypedef = "type"
and macroRecord = "record"
and macroField = "field"
and macroPtr = "ptr"

exception IllegalExpression of expression * string
let raiseIllegalExpression expr msg = raise (IllegalExpression (expr, msg))

let raiseInvalidType typeExpr = raise (UnknownType (Ast2.expression2string typeExpr))

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
  | AssignVar (v, expr) -> begin
      match typeOf expr with
        | TypeOf exprType when exprType = v.typ -> TypeOf `Void
        | TypeOf exprType -> TypeError (
            "Cannot assign result of expression to var because types differ",
            exprType, v.typ)
        | _ as typeError -> typeError
    end
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
            "Expected cond of type Bool in 'if/then/else'",
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


let rec translateType bindings typeExpr =
  let lookupType bindings name =
    try
      Some (string2composedType name)
    with
      | UnknownType _ ->
          match lookup bindings name with
            | TypedefSymbol t -> Some t
            | _ -> None
  in
  match typeExpr with  
    | { id = id; args = [targetTypeExpr]; } when id = macroPtr ->
        begin
          match translateType bindings targetTypeExpr with
            | Some t -> Some (`Pointer t)
            | None -> None
        end
    | { id = name; args = [] } ->
        begin
          lookupType bindings name
        end
    | _ -> None

type exprTranslateF = bindings -> expression -> bindings * expr list

let translateSeq (translateF : exprTranslateF) bindings = function
  | { id = id; args = sequence } when id = macroSequence ->
      Some (translatelst translateF bindings sequence)
  | _ ->
      None

let rec expr2value typ expr =
  match typ with
    | #integralType -> begin
        match expr with
          | { id = value; args = [] } -> string2integralValue value
          | _ -> raiseIllegalExpression expr
              (Printf.sprintf "expected value of type %s" (composedType2String typ))
      end
    | `Record components -> begin
(*         let translateField (name, fieldValueExpr) = *)
(*           name, expr2value `Int fieldValueExpr *)
(*         in *)
(*         let componentValues = List.map translateField components in *)
(*         `RecordVal componentValues *)
        raiseIllegalExpression expr "records not supported"
      end
    | _ -> raiseIllegalExpression expr "unsupported value expression"
        
let translateDefineVar (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [
        typeExpr;
        { id = name; args = [] };
        valueExpr
      ] } as expr
      when (id = macroVar) || (id = macroMutableVar) -> begin
        let _, simpleform = translateF bindings valueExpr in
        match translateType bindings typeExpr with
          | Some ctyp -> begin
              match ctyp with
                | #integralType | (`Pointer _) as typ ->
                    let value = defaultValue typ in
                    let storage = if id = macroVar then RegisterStorage else MemoryStorage in
                    let var = variable name typ value storage false in
                    Some( addVar bindings var, [ DefineVariable (var, Sequence simpleform) ] )
(*                 | `Record components -> begin *)
(*                     match valueExpr with *)
(*                       | { id = id; args = values; } when id = macroSequence -> begin *)
(*                           None *)
(*                         end *)
(*                       | _ -> raiseIllegalExpression expr "expecting { field0 = val0; ... }" *)
(*                   end *)
                | _ -> raiseIllegalExpression expr
                    (Printf.sprintf "Only integral and pointer types legal for vars. %s is not valid"
                       (composedType2String ctyp))
            end
          | _ -> raise (UnknownType (Ast2.expression2string typeExpr))
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

let translateAssignVar (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [
        { id = varName; args = [] };
        rightHandExpr;
      ] } when id = macroAssign -> begin
      let _, rightHandSimpleform = translateF bindings rightHandExpr in
      match lookup bindings varName with
        | VarSymbol v ->
            Some (bindings, [AssignVar (v, Sequence rightHandSimpleform)])
        | _ -> None
    end
  | _ -> None

let translateTypedef translateF (bindings :bindings) = function
  | { id = id; args = [
        { id = newTypeName; args = [] };
(*         { id = targetTypeName; args = [] } *)
        targetTypeExpr;
      ] } when id = macroTypedef -> begin
(*       match lookupType bindings targetTypeName with *)
      match translateType bindings targetTypeExpr with
        | None -> raiseInvalidType targetTypeExpr
        | Some t -> Some (addTypedef bindings newTypeName t, [])
    end
(*   | { id = id; args = [ *)
(*         { id = typeName; args = []; }; *)
(*         { id = seq; args = compExprs }; *)
(*       ] } as expr *)
(*       when id = macroTypedef && seq = macroSequence -> begin *)
(*         let expr2comp = function *)
(*           | { id = typeName; args = [{ id = compName; args = []; }]; } -> begin *)
(*               match lookupType bindings typeName with *)
(*                 | Some typ -> compName, typ *)
(*                 | None -> raise (UnknownType typeName) *)
(*             end *)
(*           | _ -> raiseIllegalExpression expr "type foo { typename compname;* } expected" *)
(*         in *)
(*         let comps = List.map expr2comp compExprs in *)
(*         Some (addTypedef bindings typeName (`Record comps), []) *)
(*       end *)
  | _ -> None

(* let translateRecord (translateF :exprTranslateF) (bindings :bindings) = function *)
(*   | { id = id; args = [ *)
(*         { id = name; args = []; }; *)
(*         { id = seq; args = fields; }; *)
(*       ] } when id = macroRecord && seq = macroSequence -> *)
(*       begin *)
(*         Some (bindings, []) *)
(*       end *)
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
    translateAssignVar;
    translateTypedef;
(*     translateRecord; *)
  ]

type toplevelExprTranslateF = bindings -> expression -> bindings * toplevelExpr list

let translateGlobalVar (translateF : toplevelExprTranslateF) (bindings :bindings) = function
  | { id = id; args = [
        typeExpr;
        { id = name; args = [] };
        { id = valueString; args = [] }
      ] } as expr
      when id = macroVar || id = macroMutableVar ->
      begin
        match translateType bindings typeExpr with
          | Some ctyp -> begin
              match ctyp with
                | #integralType as typ ->
                    let value = parseValue typ valueString in
                    let var = globalVar name typ value in
                    let newBindings = addVar bindings var in
                    Some( newBindings, [ GlobalVar var ] )
                | `Pointer targetType ->
                    if valueString = "null" then
                      let var = globalVar name (`Pointer targetType) (PointerVal (targetType, None)) in
                      let newBindings = addVar bindings var in
                      Some( newBindings, [GlobalVar var] )
                    else
                      raiseIllegalExpression expr "only null values supported for pointers currently"
                | _ -> raiseIllegalExpression expr
                    "only integral types legal for variables at this time"
            end
          | None -> raiseInvalidType typeExpr
      end
  | _ ->
      None

let translateFunc (translateF : toplevelExprTranslateF) (bindings :bindings) expr =
  let buildFunction typ name paramExprs implExprOption =
    (*     let typ = match lookupType bindings typeName with *)
    (*       | Some t -> t *)
    (*       | None -> raise (UnknownType typeName) *)
    (*     in *)
    let expr2param = function
      | { id = typeName; args = [{ id = varName; args = [] }] } ->
          begin
            let typeExpr = { id = typeName; args = [] } in
            match translateType bindings typeExpr with
              | Some typ -> (typeName, typ)
              | None -> raiseInvalidType typeExpr
          end
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
          typeExpr;
          { id = name; args = [] };
          { id = "std:seq"; args = paramExprs };
          { id = "std:seq"; args = _ } as implExpr;
        ] } when id = macroFunc ->
        begin
          match translateType bindings typeExpr with
            | Some typ -> begin
                let newBindings, funcDef = buildFunction typ name paramExprs (Some implExpr) in
                match typeOfTL funcDef with
                  | TypeOf _ -> Some( newBindings, [ funcDef ] )
                  | TypeError (msg, declaredType, returnedType) ->
                      raiseIllegalExpression
                        expr
                        (Printf.sprintf "Function has return type %s but returns %s"
                           (composedType2String declaredType)
                           (composedType2String returnedType))
              end
            | None -> raiseInvalidType typeExpr
        end
    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
          { id = "std:seq"; args = paramExprs };
        ] } when id = macroFunc ->
        begin
          match translateType bindings typeExpr with
            | Some typ ->
                begin
                  let newBindings, funcDecl = buildFunction typ name paramExprs None in
                  Some (newBindings, [funcDecl])
                end
            | None ->
                raiseInvalidType typeExpr
        end
    | _ ->
        None
        
let translateTL = translate raiseIllegalExpression [
  translateGlobalVar;
  translateFunc;
  translateTypedef;
]
  
  
