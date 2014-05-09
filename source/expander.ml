open Lang
open Types
open Semantic
open Ast2
open Common
open Bindings
open Printf
open Basics

let typeErrorMessage bindings (fe, msg, foundType, expectedType) =
  let typeName = function
    | `Any description -> description
    | #Lang.typ as t -> typeNameExplicit t
  in
  sprintf "type error: %s, expected %s but found %s in expression %s"
    msg
    (typeName expectedType)
    (typeName foundType)
    (match fe with
    | Semantic.Form form -> Lang.formToString form
    | Semantic.Ast ast -> Ast2.toString ast)

let serrorFromTypeError bindings expr (fe,m,f,e) =
  let msg = typeErrorMessage bindings (fe,m,f,e) in
  Serror.fromExpr expr msg

let formatFileNotFoundInPathError fileName directories =
  Common.combine "\n  "
    (sprintf "file '%s' could not be found" fileName
     :: sprintf "pwd = %s" (Sys.getcwd())
     :: List.map (sprintf "or in dir %s") directories)

exception IllegalExpression of sexpr * Serror.t list

module Translation_utils =
struct
  (* TODO: use in translateDefineVar and translateGlobalVar + test *)
  let determineStorage typ =
    match typ with
      | `Pointer _ -> MemoryStorage
      | _ -> RegisterStorage

  let lookupType bindings name =
    try
      (** TODO: bug, this prevents built-in types to be shadowed *)
      Some (parseType name)
    with
      | Types.CouldNotParseType _ ->
          match lookup bindings name with
            | TypedefSymbol t -> Some t
            | _ -> None

  let raiseIllegalExpression expr msg = raise (IllegalExpression (expr, [Serror.fromExpr expr msg]))
  let raiseIllegalExpressions expr errors = raise (IllegalExpression (expr, errors))

  let raiseIllegalExpressionFromTypeError expr (formOrExpr, msg, found, expected) =
    raiseIllegalExpression expr
      (typeErrorMessage Bindings.defaultBindings (formOrExpr, msg,found,expected))

  let raiseInvalidType typeExpr =
    raise (Types.CouldNotParseType (Ast2.expression2string typeExpr))

  type formWithTLsEmbedded = [`ToplevelForm of toplevelExpr | form]

  let formWithTLsEmbeddedToString = function
    | #Lang.form as form -> Lang.formToString form
    | `ToplevelForm tlform -> Lang.toplevelFormToString tlform

  type 'a someTranslateF = bindings -> sexpr -> bindings * 'a list
  type exprTranslateF = formWithTLsEmbedded someTranslateF

  let rec extractToplevelForms = function
    | [] -> [], []
    | head :: rem ->
        begin
          let remTL, remImpl = extractToplevelForms rem in
          match head with
            | `ToplevelForm #toplevelExpr as tlform -> tlform::remTL, remImpl
            | #Lang.form as implF -> remTL, implF::remImpl
        end


  let translatelst translateF bindings expr =
    let rec worker bindings = function
      | [] -> bindings, []
      | expr :: tail ->
          let newBindings, sf = translateF bindings expr in
          let resultingBindings, sfuncs = worker newBindings tail in
          resultingBindings, (sf @ sfuncs)
    in
    worker bindings expr

  let tryGetFunctionAddress bindings name =
    match lookup bindings name with
      | FuncSymbol f ->
        let name = f.fname in
        let typ = `Function {
          returnType = f.rettype;
          argTypes = List.map snd f.Lang.fargs;
        } in
        let var = variable name (`Pointer typ) RegisterStorage true None in
        Some (`Variable var)
      | _ ->
        None

  let lastTempVarNum = ref 0

  let getUnusedName ?(prefix = "temp") bindings =
    let rec tryTempVar num =
      let name = (Printf.sprintf "%s_%d" prefix num) in
      match lookup bindings name with
        | UndefinedSymbol -> name
        | _ -> tryTempVar (num + 1)
    in
    incr lastTempVarNum;
    tryTempVar !lastTempVarNum

  let getNewGlobalVar bindings typ =
    let newVarName = getUnusedName ~prefix:"tempvar" bindings in
    let var = globalVar ~name:newVarName ~typ ~location:None in
    (addVar bindings var, var)

  let getNewLocalVar bindings typ =
    let newVarName = getUnusedName bindings in
    (* let var = localVar ~name:newVarName ~typ in *)
    let var = Lang.variable
      ~name:newVarName
      ~typ
      ~storage:MemoryStorage
      ~global:false
      ~location:None
    in
    (addVar bindings var, var)

  let translateToForms (translateF :exprTranslateF) bindings expr =
    let newBindings, xforms = translateF bindings expr in
    let tlforms, forms = extractToplevelForms xforms in
    newBindings, toSingleForm forms, tlforms

  let rec flattenLeft = function
    | [] -> [], []
    | (list, x) :: rem ->
        let remList, remX = flattenLeft rem in
        (list @ remList, x :: remX)

  let flattenNestedTLForms = List.map (fun (`ToplevelForm tlform) -> tlform)

  type toplevelExprTranslateF = toplevelExpr someTranslateF
end

open Translation_utils

module Mayfail : sig
  type 'a mayfail = Result of 'a | Error of Serror.t list

  val errorFromString : Basics.location -> string -> 'a mayfail
  val errorFromExpr : Ast2.sexpr -> string -> 'a mayfail
  val singleError : Serror.t -> 'a mayfail
  val multipleErrors : Serror.t list -> 'a mayfail

  val result : 'a -> 'a mayfail

  val combineResults : 'a mayfail list -> 'a list mayfail
  val mapResult : ('m -> 'n) -> 'm mayfail -> 'n mayfail
  val extractErrors : 'o mayfail -> Serror.t list
  val flattenResult : 'p mayfail mayfail -> 'p mayfail
end = struct
  type 'a mayfail =
    | Result of 'a
    | Error of Serror.t list

  let errorFromString location msg = Error [Serror.fromMsg (Some location) msg]
  let errorFromExpr expr msg = Error [Serror.fromExpr expr msg]
  let singleError error = Error [error]
  let multipleErrors errors = Error errors
  let result r = Result r

  let combineResults mayfails =
    let errors = ref [] in
    let addTo listRef e = listRef := e :: !listRef in
    let results = mapFilter
      (function Error msgs -> List.iter (addTo errors) msgs; None | Result r -> Some r)
      mayfails
    in
    match !errors with
      | [] -> Result results
      | messages -> Error messages

  let mapResult f = function
    | Result r -> Result (f r)
    | Error errors -> Error errors

  let extractErrors = function
    | Result _ -> []
    | Error errors -> errors

  let flattenResult = function
    | Result (Result r) -> Result r
    | Result (Error errors)
    | Error errors -> Error errors
end

open Mayfail

let errorFromTypeError bindings expr (fe,m,f,e) =
  Mayfail.singleError (serrorFromTypeError bindings expr (fe,m,f,e))

exception MayfailError of Serror.t list

let rec translateType bindings emitWarning typeExpr : Lang.typ mayfail =
  let error msg = Mayfail.errorFromExpr typeExpr msg in
  let instantiateType parametricType argumentType =
    let rec inst = function
      | `TypeParam ->
        argumentType
      | `TypeRef _
      | #integralType as t ->
        t
      | `Record rt ->
        `Record { rt with fields = List.map (map2nd inst) rt.fields }
      | `Pointer t ->
        `Pointer (inst t)
        (* | `ParametricType t -> *)
        (*     `ParametricType (inst t) *)
      | `Array (memberType, size) ->
        `Array (inst memberType, size)
      | `Function ft ->
        `Function { returnType = inst ft.returnType;
                    argTypes = List.map inst ft.argTypes }
      | `ParametricType `Record rt ->
        `Record { rt with fields = List.map (map2nd inst) rt.fields }
      | `ParametricType `Pointer t ->
        `Pointer (inst t)
      | `ErrorType _ as t ->
        t
    in
    inst parametricType
  in

  let translatePtr targetTypeExpr =
    match translateType bindings emitWarning targetTypeExpr with
      | Result t -> Result (`Pointer t)
      | error -> error
  in
  let translateArray memberTypeExpr sizeExpr =
    let potentialSize =
      match sizeExpr with
        | { id = number; args = [] } ->
          begin try
                  Some(int_of_string number)
            with Failure _ ->
              None
          end
        | _ -> None
    in
    match translateType bindings emitWarning memberTypeExpr, potentialSize with
      | Result t, Some size -> Result (`Array(t, size))
      | errorM, _ -> errorM
  in
  let translateFunction returnTypeExpr argTypeExprs =
    try
      let translate typ =
        match translateType bindings emitWarning typ with
          | Result t -> t
          | Error errors ->
            raise (MayfailError errors)
      in
      begin
        Result (`Function {
          returnType = translate returnTypeExpr;
          argTypes = List.map translate argTypeExprs;
        })
      end
    with MayfailError errors ->
      Error errors
  in
  match typeExpr with
    (** function pointers *)
    | { id = "opcall"; args = returnTypeExpr :: argTypeExprs } ->
      translateFunction returnTypeExpr argTypeExprs
    | { id = "opjux"; args = {id = "fptr"; args = []} :: returnTypeExpr :: argTypeExprs } ->
      begin match translateFunction returnTypeExpr argTypeExprs with
        | Result typ ->
          let () = (* fix return type *)
            emitWarning (Serror.fromExpr typeExpr
                           (sprintf "fptr is deprecated. Use %s instead"
                              (Types.typeName typ)))
          in
          Result (`Pointer typ)
        | Error _ as errors ->
          errors
      end

    | { id = "opjux"; args = args } (* when jux = macroJuxOp *) ->
      translateType bindings emitWarning (shiftId args)

    | { id = "op!"; args =
        [{ id = paramTypeName; args = [] };
         argumentTypeExpr ] } ->
      begin
        match lookup bindings paramTypeName, translateType bindings emitWarning argumentTypeExpr with
          | TypedefSymbol ((`ParametricType _) as t), Result `TypeParam ->
            Result t
          | TypedefSymbol (`ParametricType t), Result argumentType ->
            Result (instantiateType (t :> typ) argumentType)
          | paramTypeResult, argumentResult ->
            let otherErrors = extractErrors argumentResult in
            let error = Serror.fromExpr typeExpr "could not translate type" in
            Error (error :: otherErrors)
      end

    | { id = "postop*"; args = [targetTypeExpr] }
    | { id = "ptr"; args = [targetTypeExpr]; } ->
      translatePtr targetTypeExpr

    | { id = "postop[]"; args = [memberTypeExpr; sizeExpr] }
    | { id = "array"; args = [memberTypeExpr; sizeExpr] } ->
      translateArray memberTypeExpr sizeExpr

    | { id = name; args = [] } ->
      begin
        match lookupType bindings name with
          | Some t -> Result t
          | None -> error (sprintf "could not look up type %s" name)
      end
    | _ ->
      errorFromExpr typeExpr "don't know how to interpret as type"

(**
   This should actually return unit mayfail and only pass warnings/info to
   emitDiagnostics.

   TODO: no errors when a local hides a global!
 *)
let hasRedefinitionErrors
    (newDefinitionType : [`NewVar | `NewFuncDef | `NewFuncDecl | `NewType | `NewMacro])
    (scope : [`Local | `Global])
    name expr bindings emitDiagnostics
    =
  let reportPreviousDefinition previousDefinitionLocOpt =
    if previousDefinitionLocOpt <> None then
      emitDiagnostics
        Basics.DiagnosticKind.Info
        (Serror.fromMsg previousDefinitionLocOpt (sprintf "previous definition of %s" name))
  in
  let reportRedefinition previousDefinitionLoc diagnosticsKind =
    let error = Serror.fromExpr expr (sprintf "redefinition of %s" name) in
    emitDiagnostics diagnosticsKind error;
    reportPreviousDefinition previousDefinitionLoc;
    diagnosticsKind = Basics.DiagnosticKind.Error
  in
  let error = Basics.DiagnosticKind.Error
  and warning = Basics.DiagnosticKind.Warning in
  let previousBinding = Bindings.lookup bindings name in
  let previousBindingScope =
    match previousBinding with
      | VarSymbol { vglobal = false }
      | LabelSymbol _ ->
        `Local

      | VarSymbol { vglobal = true }
      | FuncSymbol _ ->
        `Global

      | MacroSymbol _
      | TypedefSymbol _ ->
        `Unknown

       | UndefinedSymbol ->
        `Undefined
  in

  let previousDefinitionLocOpt =
    match previousBinding with
      | VarSymbol { vlocation = locOpt }
      | FuncSymbol { flocation = locOpt }
      | MacroSymbol { mlocation = locOpt }
        ->
        locOpt

      | TypedefSymbol _
      | LabelSymbol _
      | UndefinedSymbol ->
        None
  in

  match scope, previousBindingScope with
    | _, `Undefined
    | _, `Unknown
    | `Local, `Global ->
      false

    | `Global, `Local ->
      emitDiagnostics error (Serror.fromExpr expr "internal error, global hiding local definition");
      reportPreviousDefinition previousDefinitionLocOpt;
      true

    | `Local, `Local
    | `Global, `Global ->
      begin match newDefinitionType, previousBinding with
        | `NewVar,  VarSymbol var ->
          reportRedefinition var.vlocation error
        | `NewFuncDef,  FuncSymbol { impl = Some _; flocation } ->
          if Zompvm.isInteractive() then
            false
          else
            reportRedefinition flocation error
        | `NewMacro,  MacroSymbol { mlocation } ->
          if mlocation = Some Basics.builtinLocation then
            false
          else
            reportRedefinition mlocation warning
        | `NewType,  TypedefSymbol _ ->
        (** TODO: this should be an error if the types differ *)
          false

        (** Redefining as something different than before is an error *)
        | (`NewFuncDef | `NewFuncDecl | `NewType | `NewMacro),  VarSymbol var ->
          reportRedefinition var.vlocation error
        | (`NewVar | `NewType | `NewMacro),  FuncSymbol { flocation } ->
          reportRedefinition flocation error
        | (`NewVar | `NewType | `NewFuncDecl | `NewFuncDef),  MacroSymbol { mlocation } ->
          reportRedefinition mlocation error
        | (`NewVar | `NewFuncDef | `NewFuncDecl | `NewMacro),  TypedefSymbol _ ->
          reportRedefinition None error
        | _,  LabelSymbol _ ->
          failwith "internal error, global label defined"

        | `NewFuncDef,  FuncSymbol { impl = None }
        | `NewFuncDecl,  FuncSymbol _
        (** TODO: should be an error if signatures are different *)
        | _,  UndefinedSymbol ->
          false
      end

let parseErrorExpr expr =
  match expr.args with
    | [ { id = message; args = []; location } ] ->
      Serror.fromMsg location message

    | [ { id = message; args = [] }; invalidExpr] ->
      Serror.fromExpr invalidExpr message

    | _ ->
      Serror.fromExpr expr
        "invalid error. expected 'std:base:error string-literal expr?'"

let reportDiagnostics kind diagnostic =
  eprintf "%s\n" (Serror.diagnosticsToString kind diagnostic)
let reportError = reportDiagnostics Basics.DiagnosticKind.Error
let reportWarning = reportDiagnostics Basics.DiagnosticKind.Warning
let reportInfo = reportDiagnostics Basics.DiagnosticKind.Info

(** "new" compiler types *)

type 'translateF env = {
  bindings :Bindings.t;
  translateF :exprTranslateF;
  translateExpr : exprTranslateF env -> Ast2.t -> (bindings * formWithTLsEmbedded list) mayfail;
  parseF : fileName:string -> string -> Ast2.t list option;
  reportError : Serror.t -> unit;
}

type nestedEnv = exprTranslateF env

type translationResult = (bindings * (formWithTLsEmbedded list)) mayfail

(** Also needs to be updated in zompvm_impl.cpp and prelude.zomp *)
let astType = `Record {
  rname = "ast";
  fields = [
    "id", `Pointer `Char;
    "childCount", `Int32;
    "childs", `Pointer (`Pointer (`TypeRef "ast"));
    "file", `Pointer `Char;
    "line", `Int32;
    "column", `Int32;
  ] }
let astPtrType = `Pointer astType

let macroFuncs = ref []

module Old_macro_support =
struct
  (** TODO: this can result in quadratic run time, fix source locations directly
   * after transferring from native code *)
  let rec withDefaultSourceLocation location expr =
    let fixedArgs = List.map (withDefaultSourceLocation location) expr.args in
    match expr.location with
      | Some _ ->
        { expr with args = fixedArgs }
      | None ->
        { expr with location = Some location; args = fixedArgs }

  (* todo: use env *)
  let translateMacroCall (env :exprTranslateF env) expr =
    match expr with
      | { id = macroName; args = args; } ->
        match lookup env.bindings macroName with
          | MacroSymbol macro ->
            begin
              try
                let transformedExpr =
                  let withBrokenLocations = macro.mtransformFunc env.bindings expr in
                  match expr.location with
                    | Some location ->
                      withDefaultSourceLocation location withBrokenLocations
                    | None ->
                      withBrokenLocations
                in
                Some (env.translateF env.bindings transformedExpr)
              with
                | Failure msg ->
                  raiseIllegalExpression expr ("could not expand macro: " ^ msg)
            end
          | _ -> None
end

module Macros : sig
  val translateDefineMacroHelper :
      [ `Global | `Local ] ->
      Translation_utils.exprTranslateF env ->
      Ast2.t -> Bindings.bindings Mayfail.mayfail

  val translateDefineNestedMacro :
    Translation_utils.exprTranslateF env -> Ast2.t -> (Bindings.bindings * 'form list) Mayfail.mayfail
end = struct
  let buildNativeMacroFunc
      (env :exprTranslateF env)
      (`MacroFuncName macroFuncName)
      argNames
      implExprs
      (isVariadic : [`IsVariadic | `IsNotVariadic])
      =
    let argParamName = "macro_args" in
    let bindings = Bindings.addVar env.bindings
      (Lang.variable
         ~name:argParamName
         ~typ:(`Pointer astPtrType)
         ~storage:MemoryStorage
         ~global:false
         ~location:None)
    in
    let buildParamFetchExpr num name =
      Ast2.expr "std:base:localVar" [Ast2.idExpr name;
                                     Ast2.expr "load" [
                                       Ast2.expr "ptradd" [
                                         Ast2.idExpr argParamName;
                                         Ast2.idExpr (sprintf "%d" num)]
                                     ]]
    in
    let fetchParamExprs = Common.listMapi buildParamFetchExpr argNames in
    let sexprImpl = Ast2.seqExpr (fetchParamExprs @ implExprs) in
    let _, xforms = env.translateF bindings sexprImpl in
    let initForms, implforms = extractToplevelForms xforms in
    let initForms = flattenNestedTLForms initForms in
    let astType =
      match lookupType bindings "ast" with
        | Some t -> t
        | None -> raiseIllegalExpression (Ast2.idExpr "ast") "could not find prelude type 'ast'"
    in
    let macroFunc =
      let fargs = [argParamName, `Pointer (`Pointer astType)]
      and impl = Some (toSingleForm implforms) in
      func macroFuncName astPtrType fargs impl Basics.fakeLocation
    in
    let tlforms = initForms @ [`DefineFunc macroFunc] in
    tlforms, macroFunc

  let translateMacroCall macroName (`MacroFuncName macroFuncName) paramCount isVariadic =
    let nativeFuncAddr = Machine.zompAddressOfMacroFunction ~name:macroFuncName in
    (fun bindings expr ->
       let result =
         let args = expr.args in
         begin
           let invokeMacro args =
             let nativeArgs = List.map Zompvm.NativeAst.buildNativeAst args in
             Machine.zompResetMacroArgs();
             List.iter (fun ptr -> Machine.zompAddMacroArg ~ptr) nativeArgs;
             let nativeResultAst = Machine.zompCallMacro nativeFuncAddr in
             let resultAst = Zompvm.NativeAst.extractSExprFromNativeAst nativeResultAst in
             resultAst
           in
           let argCount = List.length args in
           match isVariadic with
             | `IsNotVariadic -> begin
                 if argCount <> paramCount then begin
                   raiseIllegalExpression
                     (Ast2.expr macroName args)
                     (sprintf "expected %d args but found %d" paramCount argCount);
                 end else begin
                   invokeMacro args
                 end
               end
             | `IsVariadic -> begin
                 if argCount < paramCount-1 then begin
                   raiseIllegalExpression
                     (Ast2.expr macroName args)
                     (sprintf "expected at least %d args but found only %d"
                        (paramCount-1) argCount)
                 end;
                 let declaredArgs, variadicArgs = Common.splitAfter (paramCount-1) args in
                 let inflatedArgs = declaredArgs @ [Ast2.seqExpr variadicArgs] in
                 invokeMacro inflatedArgs
               end
         end
       in
       match expr.location, result.location with
         | Some loc, None ->
             { result with Ast2.location = Some loc }
         | Some _, Some _
         | None, _ ->
             result : bindings -> Ast2.sexpr -> Ast2.sexpr)

  let decomposeMacroDefinition expr =
    let nameParamImplOption =
      match expr with
        | {Ast2.args = {Ast2.id = name; args = []} :: paramsAndImpl} ->
          begin match List.rev paramsAndImpl with
            | {Ast2.id = seqId; args = implExprs} :: params ->
              Some (name, List.rev params, implExprs)
            | _ ->
              None
          end
        | _ ->
          None
    in

    match nameParamImplOption with
      | Some (name, params, implExprs) ->
        begin
          let decomposeMacroParam = function
            | {Ast2.id = paramName; Ast2.args = []} ->
              Result (paramName, `IsNotVariadic)
            | {Ast2.id = id; args = [{Ast2.id = paramName; Ast2.args=[]}]}
                when id = macroRest ->
              Result (paramName, `IsVariadic)
            | (_ as invalidParam) ->
              errorFromExpr invalidParam
                "invalid macro parameter, expected id or id..."
          in
          match List.rev params with
            | [] ->
              Result (name, ([] :string list), implExprs, `IsNotVariadic)
            | lastParam :: frontParamsRev ->
              let combineParams names param =
                match names, param with
                  | Result names, Result (paramName, `IsNotVariadic) ->
                    Result (paramName :: names)
                  | Result names, Result (paramName, `IsVariadic) ->
                    errorFromExpr expr
                      (sprintf "parameter %s is variadic but is not the last parameter" paramName)
                  | Error msg, _
                  | _, Error msg ->
                    Error msg
              in
              let frontParamNamesOpt =
                List.fold_left combineParams
                  (Result [])
                  (List.map decomposeMacroParam frontParamsRev)
              in
              match frontParamNamesOpt with
                | Result frontParamNames ->
                  begin match decomposeMacroParam lastParam with
                    | Result (lastParamName, isVariadic) ->
                      Result (name,
                              frontParamNames @ [lastParamName],
                              implExprs,
                              isVariadic)
                    | Error msg ->
                      Error msg
                  end
                | Error msg ->
                  Error msg
        end
      | _ ->
        errorFromExpr expr (sprintf "expecting '%s name paramName* lastParamVariadic...? seq" expr.id)

  let rec removeSourceLocations ast =
    { id = ast.id; location = None; args = List.map removeSourceLocations ast.args }

  (* TODO: kick out all source location info *)
  let translateDefineMacroHelper scope (env :exprTranslateF env) expr =
    match decomposeMacroDefinition expr with
      | Result (macroName, paramNames, implExprs, isVariadic) ->
        let implExprs = List.map removeSourceLocations implExprs in
        begin
          let isRedefinitionError =
            hasRedefinitionErrors `NewMacro scope macroName expr env.bindings reportDiagnostics
          in

          if isRedefinitionError then
            Error []
          else begin
            let macroFuncName =
              let prefix = match scope with
                | `Global -> "zomp$macro$"
                | `Local -> "zomp$local-macro$"
              in
              prefix ^ macroName
            in
            let tlexprs, func =
              buildNativeMacroFunc env (`MacroFuncName macroFuncName) paramNames implExprs isVariadic
            in

            let llvmCodeFragments = List.map Genllvm.gencodeTL tlexprs in

            Zompvm.evalLLVMCodeB
              (if listContains macroFuncName !macroFuncs then
                  [macroFuncName]
               else begin
                 macroFuncs := macroFuncName :: !macroFuncs;
                 []
               end)
              (Common.combine "\n" llvmCodeFragments);

            flush stdout;

            let docstring =
              Common.combine " " paramNames ^
                match isVariadic with | `IsVariadic -> "..." | _ -> ""
            in

            let location = someOrDefault expr.location Basics.fakeLocation in
            let newBindings = Bindings.addMacro env.bindings macroName docstring location
              (translateMacroCall macroName (`MacroFuncName macroFuncName) (List.length paramNames) isVariadic)
            in

            Result newBindings
          end
        end
      | Error reasons ->
          Error reasons

  let translateDefineNestedMacro (env :exprTranslateF env) expr =
    match translateDefineMacroHelper `Local env expr with
      | Result newBindings -> Result (newBindings, [])
      | Error (_ as errors) -> Error errors
end

(** A module defining various zomp transformations *)
module type Zomp_transformer =
sig
  (** name, doc, translateFunc *)
  val register : (string -> (string * (exprTranslateF env -> Ast2.t -> translationResult)) -> unit) -> unit
end

module Base : Zomp_transformer =
struct
  let reportErrorE env expr msg = env.reportError (Serror.fromExpr expr msg)
  let reportErrorM env messages = List.iter (fun msg -> env.reportError (Serror.fromMsg None msg)) messages

  let translateType bindings expr =
    translateType bindings reportWarning expr

  (** translates expressions of the form x = xExpr, y = yExpr etc. *)
  let translateStructLiteralArgs bindings structName loc fields fieldExprs (translateExprF : Ast2.t -> 'a mayfail) =
    let rec handleFieldExprs errors fieldValues undefinedFields =
      let continueWithErrors newErrors remArgs =
        handleFieldExprs (newErrors@errors) fieldValues undefinedFields remArgs
      in
      function
        | [] ->
          let errors = match undefinedFields with
            | [] -> errors
            | missing ->
              errors @
                List.map (fun (name, _) -> Serror.fromMsg loc $ sprintf "field %s is missing" name) missing
          in
          begin match errors with
            | [] -> Result (List.rev fieldValues)
            | _ -> Error errors
          end
        | { id = "op=";
            args = [
              {id = fieldName; args= []};
              rhs]
          }
            as fieldValueExpr :: remArgs ->
          begin
            if List.exists (fun (name, _) -> name = fieldName) fieldValues then
              let newError = Serror.fromExpr fieldValueExpr $ sprintf "field %s has been defined multiple times" fieldName in
              continueWithErrors [newError] remArgs
            else match List.partition (fun (name, _) -> name = fieldName) undefinedFields with
              | [_, fieldType], undefinedFields ->
                begin match translateExprF rhs with
                  | Error rhsErrors ->
                    continueWithErrors rhsErrors remArgs
                  | Result (tlforms, valueForms) ->
                    begin
                      let errors, fieldValues =
                        match typeCheck bindings (`Sequence valueForms) with
                          | TypeOf valueType when equalTypes bindings valueType fieldType ->
                            errors, ((fieldName, (tlforms, valueForms)) :: fieldValues)
                          | TypeOf invalidValueType ->
                            (Serror.fromExpr fieldValueExpr
                               (sprintf "value for field %s has type %s but expected type %s" fieldName
                                  (typeName invalidValueType)
                                  (typeName fieldType)))
                            :: errors,
                            fieldValues
                          | TypeError (form,msg,found,expected) ->
                            Serror.fromExpr fieldValueExpr (typeErrorMessage bindings (form,msg,found,expected)) :: errors,
                            fieldValues
                      in
                      handleFieldExprs errors fieldValues undefinedFields remArgs
                    end
                end
              | [], _ ->
                let newError =
                  Serror.fromExpr fieldValueExpr
                    (sprintf "field %s is not a member of struct %s" fieldName structName)
                in
                continueWithErrors [newError] remArgs
              | (_ :: _ :: _), _ ->
                let newError =
                  Serror.fromExpr fieldValueExpr
                    (sprintf "internal error, field %s contained multiple times in struct" fieldName)
                in
                continueWithErrors [newError] remArgs
          end
        | unexpectedExpr :: remArgs ->
          let newError = Serror.fromExpr unexpectedExpr "expected expression of the form 'id = expr'" in
          continueWithErrors [newError] remArgs
    in
    handleFieldExprs [] [] fields fieldExprs

  let translateDefineLocalVar env expr =
    let transform id name typeExpr valueExpr =
      let declaredType = match typeExpr with
        | Some e ->
            begin
              match translateType env.bindings e with
                | Result t -> Some t
                | Error _ -> raise (CouldNotParseType (Ast2.expression2string e))
            end
        | None -> None
      in
      let valueType, toplevelForms, implForms =
        match valueExpr with
          | Some valueExpr ->
              begin
                let _, simpleform = env.translateF env.bindings valueExpr in
                let toplevelForms, implForms = extractToplevelForms simpleform in
                match typeCheck env.bindings (`Sequence implForms) with
                  | TypeOf t ->
                      Some t, toplevelForms, implForms
                  | TypeError (fe,m,f,e) ->
                      raiseIllegalExpressionFromTypeError valueExpr (fe,m,f,e)
              end
          | None -> None, [], []
      in
      let varType =
        match declaredType, valueType with
          | Some declaredType, Some valueType
              when equalTypes env.bindings declaredType valueType
                -> declaredType
          | Some declaredType, Some valueType ->
              raiseIllegalExpressionFromTypeError
                expr (Semantic.Ast expr, "types do not match",declaredType,valueType)
          | None, Some valueType ->
              valueType
          | Some declaredType, None ->
              declaredType
          | None, None ->
              raiseIllegalExpression
                expr "var needs either a default value or declare a type"
      in
      match varType with
        | #integralType | `Pointer _ | `Function _ as typ ->
            begin
              let var = variable name typ MemoryStorage false expr.location in
              let defvar = `DefineVariable (var, Some (`Sequence implForms))
              in
              match typeCheck env.bindings defvar with
                | TypeOf _ -> Result( addVar env.bindings var, toplevelForms @ [defvar] )
                | TypeError (fe, m,f,e) -> raiseIllegalExpressionFromTypeError expr (fe, m,f,e)
            end
        | (`Record _ as typ) ->
            begin
              match valueExpr with
                | None ->
                    let var = variable name typ MemoryStorage false expr.location in
                    Result( addVar env.bindings var, [ `DefineVariable (var, None) ] )
                | Some valueExpr ->
                  raiseIllegalExpression
                    valueExpr
                    "record type var must not have a default value"
            end
        | `TypeRef _ ->
            raiseIllegalExpression expr "internal error: received unexpected type ref"
        | `Array _ ->
            raiseIllegalExpression expr "array vars not supported, yet"
        | `TypeParam ->
            raiseIllegalExpression expr "cannot define local variable of type parameter type"
        | `ParametricType _ ->
            raiseIllegalExpression expr "cannot define local variable of parametric type"
        | `ErrorType _ as t ->
          raiseIllegalExpression expr (sprintf "cannot define local variable of %s"
                                         (typeName t))
    in
    match expr with
      | { args = [
            { id = name; args = [] };
            valueExpr
          ] } ->
          transform id name None (Some valueExpr)

      | _ ->
          errorFromExpr expr (sprintf "expecting '%s name valueExpr'" expr.id)
  let translateDefineLocalVarD = "name, value", translateDefineLocalVar

  (** exprTranslateF env -> Ast2.sexpr -> translationResult *)
  let translateAssignVar (env :exprTranslateF env) expr =
    let doTranslation id varName rightHandExpr =
      let _, rightHandForm, toplevelForms =
        translateToForms env.translateF env.bindings rightHandExpr
      in
      match lookup env.bindings varName with
        | VarSymbol lhsVar ->
            begin
              match typeCheck env.bindings rightHandForm with
                | TypeOf rhsType when rhsType = lhsVar.typ ->
                    Result (env.bindings,
                            toplevelForms @ [`AssignVar (lhsVar, rightHandForm)] )
                | TypeOf invalidRhsType ->
                    errorFromExpr rightHandExpr
                      (sprintf "type error: cannot assign %s to %s"
                         (typeName invalidRhsType) (typeName lhsVar.typ))
                | TypeError (fe,m,f,e) ->
                    errorFromTypeError env.bindings rightHandExpr (fe,m,f,e)
            end
        | _ ->
            errorFromExpr expr (sprintf "could not find variable %s" varName)
    in

    match expr with
      | { id = id; args = [
            { id = varName; args = [] };
            rightHandExpr;
          ] } when id = macroAssign ->
          doTranslation id varName rightHandExpr
      | _ ->
        errorFromExpr expr "expected 'assign varName valueExpr'"
  let translateAssignVarD = "var, value", translateAssignVar

  let translateSeq (env :exprTranslateF env) expr =
    Result (translatelst env.translateF env.bindings expr.args)
  let translateSeqD = "ast...", translateSeq

  let translateReturn (env :exprTranslateF env) = function
    | { id = id; args = [expr] } ->
        begin
          let _, form, toplevelExprs = translateToForms env.translateF env.bindings expr in
          Result (env.bindings, toplevelExprs @ [`Return form])
        end
    | { args = [] } ->
      begin
        Result (env.bindings, [`Return (`Constant VoidVal)])
      end
    | expr ->
      errorFromExpr expr
        (sprintf "expected zero or one argument instead of %d" (List.length expr.args))
  let translateReturnD = "[value]", translateReturn

  let translateLabel (env :exprTranslateF env) expr =
    match expr.args with
      | [ {id = name; args = [] }] ->
          Result( addLabel env.bindings name, [`Label (Lang.label name)] )
      | _ ->
        errorFromExpr expr ("expecting one argument which is an identifier")
  let translateLabelD = "name", translateLabel

  let translateBranch (env :exprTranslateF env) expr =
    match expr.args with
      | [{ id = labelName; args = [] }] ->
          begin
            Result( env.bindings, [`Jump (Lang.label labelName)] )
          end
      | [invalidBranchTarget] ->
          errorFromExpr expr
            "expecting 'branch labelName' where labelName is an identifier denoting an existing label"
      | [
          { id = condVarName; args = [] };
          { id = trueLabelName; args = [] };
          { id = falseLabelName; args = [] };
        ] ->
          begin
            match lookup env.bindings condVarName with
              | VarSymbol var when var.typ = `Bool ->
                  begin
                    Result( env.bindings, [`Branch (Lang.branch
                                                      (varWithType var `Bool)
                                                      (Lang.label trueLabelName)
                                                      (Lang.label falseLabelName))])
                  end
              | _ ->
                errorFromExpr expr
                  "first argument of conditional branch should be bool variable"
          end
      | _ ->
        errorFromExpr expr
          "expected either 'branch labelName' or 'branch boolVar labelOnTrue labelOnFalse'"
  let translateBranchD = "label | boolValue labelOnTrue labelOnFalse", translateBranch

  let translateLoad (env :exprTranslateF env) expr =
    match expr.args with
      | [ptrExpr] ->
          begin
            let _, ptrForm, toplevelForms = translateToForms env.translateF env.bindings ptrExpr in
            let loadForm = `LoadIntrinsic ptrForm in
            match typeCheck env.bindings loadForm with
              | TypeOf _ ->
                  Result( env.bindings, toplevelForms @ [loadForm] )
              | TypeError (fe,m,f,e) ->
                  errorFromTypeError env.bindings ptrExpr (fe,m,f,e)
          end
      | _ ->
          errorFromExpr expr "expecting only one argument"
  let translateLoadD = "pointer", translateLoad

  let translateStore (env :exprTranslateF env) expr =
    match expr.args with
      | [ptrExpr; rightHandExpr]->
          begin
            let _, ptrForm, toplevelForms = translateToForms env.translateF env.bindings ptrExpr in
            let _, rightHandForm, toplevelForms2 = translateToForms env.translateF env.bindings rightHandExpr in
            let storeInstruction = `StoreIntrinsic (ptrForm, rightHandForm) in
            match typeCheck env.bindings storeInstruction with
              | TypeOf _ ->
                  Result( env.bindings, toplevelForms @ toplevelForms2 @ [storeInstruction] )
              | TypeError (fe,m,f,e) ->
                  errorFromTypeError env.bindings rightHandExpr (fe,m,f,e)
          end
      | _ ->
          errorFromExpr expr "expected two arguments: 'store ptrExpr valueExpr'"
  let translateStoreD = "pointer, value", translateStore

  let translateMalloc (env :exprTranslateF env) expr =
    let buildMallocInstruction typeExpr countExpr =
      match translateType env.bindings typeExpr with
        | Error _ as err ->
          err
        | Result typ ->
            begin
              let _, rightHandForm, toplevelForms = translateToForms env.translateF env.bindings countExpr in
              let mallocForm = `MallocIntrinsic (typ, rightHandForm) in
              match typeCheck env.bindings mallocForm with
                | TypeOf _ -> Result( env.bindings, toplevelForms @ [mallocForm] )
                | TypeError (fe,m,f,e) ->
                    errorFromTypeError env.bindings expr (fe,m,f,e)
            end
    in
    match expr with
      | { id = id; args = [typeExpr] } when id = macroMalloc ->
          buildMallocInstruction typeExpr (Ast2.idExpr "1")
      | { id = id; args = [typeExpr; countExpr] } when id = macroMalloc ->
          buildMallocInstruction typeExpr countExpr
      | _ ->
          errorFromExpr expr "expected 'malloc typeExpr countExpr', countExpr being optional"
  let translateMallocD = "type count?", translateMalloc

  let translateNullptr (env :exprTranslateF env) expr =
    match expr.args with
      | [typeExpr] ->
          begin match translateType env.bindings typeExpr with
            | Result typ -> Result (env.bindings, [`Constant (NullpointerVal typ)] )
            | Error msgs -> Error msgs
          end
      | _ ->
          errorFromExpr expr "expected one argument denoting a type: 'nullptr typeExpr'"
  let translateNullptrD = "type", translateNullptr

  let translateGetaddr (env :exprTranslateF env) expr =
    match expr with
      | { args = [{ id = varName; args = [] }] } ->
          begin
            match lookup env.bindings varName with
              | VarSymbol var -> Result (env.bindings, [`GetAddrIntrinsic var] )
              | FuncSymbol _ ->
                begin match tryGetFunctionAddress env.bindings varName with
                  | Some form -> Result (env.bindings, [form])
                  | None ->
                    raiseIllegalExpression expr (sprintf "could not get address of function")
                end
              | _ -> raiseIllegalExpression expr (sprintf "could not find variable %s" varName)
          end
      | { id = "ptr"; args = [] } ->
          (** could actually be a variable called 'ptr', handle this for backwards compatibility *)
          begin match lookup env.bindings "ptr" with
            | VarSymbol v -> Result (env.bindings, [`Variable v])
            | _ -> errorFromExpr expr "meh. ptr is not a variable"
          end
      | _ ->
          errorFromExpr expr "expected one argument denoting an lvalue: 'ptr lvalueExpr'"
  let translateGetaddrD = "lvalue", translateGetaddr

  let translatePtradd (env :exprTranslateF env) expr =
    match expr.args with
      | [ptrExpr; indexExpr] ->
          begin
            let _, ptrForm, toplevelForms = translateToForms env.translateF env.bindings ptrExpr in
            let _, indexForm, toplevelForms2 = translateToForms env.translateF env.bindings indexExpr in
            let ptradd = `PtrAddIntrinsic (ptrForm, indexForm) in
            match typeCheck env.bindings ptradd with
              | TypeOf _ -> Result( env.bindings, toplevelForms @ toplevelForms2 @ [ptradd] )
              | TypeError (fe,m,f,e) ->
                  errorFromTypeError env.bindings expr (fe,m,f,e)
          end
      | _ ->
          errorFromExpr expr "expected two arguments: 'ptradd ptrExpr intExpr'"
  let translatePtraddD = "ptr, intExpr", translatePtradd

  let translatePtrDiff (env :exprTranslateF env) expr =
    match expr.args with
      | [lhsExpr; rhsExpr] ->
        begin
          let _, lhsForm, lhsTLForms =
            translateToForms env.translateF env.bindings lhsExpr
          in
          let _, rhsForm, rhsTLForms =
            translateToForms env.translateF env.bindings rhsExpr
          in
          let ptrdiff = `PtrDiffIntrinsic (lhsForm, rhsForm) in
          match typeCheck env.bindings ptrdiff with
            | TypeOf _ -> Result( env.bindings, lhsTLForms @ rhsTLForms @ [ptrdiff] )
            | TypeError (fe,m,f,e) ->
              errorFromTypeError env.bindings expr (fe,m,f,e)
        end
      | _ ->
        errorFromExpr expr "expected two pointers as arguments"
  let translatePtrDiffD = "ptrExpr, ptrExpr", translatePtrDiff

  let translateGetfieldptr (env :exprTranslateF env) expr =
    match expr.args with
      | [
          recordExpr;
          { id = fieldName; args = [ ] };
        ] ->
          begin
            let _, recordForm, toplevelForms = translateToForms env.translateF env.bindings recordExpr in
            let (moreForms : formWithTLsEmbedded list), (recordForm' :Lang.form) =
              match recordForm with
                | `Variable ({ typ = `Record _; } as recordVar) ->
                    [], `GetAddrIntrinsic recordVar
                | _ ->
                    begin match typeCheck env.bindings recordForm with
                      | TypeOf (`Record _ as typ) ->
                          let newBindings, tempVar = getNewLocalVar env.bindings typ in
                          [((`DefineVariable (tempVar, Some recordForm)) :> formWithTLsEmbedded)],
                          `GetAddrIntrinsic tempVar
                      | TypeOf `Pointer `Record _ -> [], recordForm
                      | TypeOf invalidType ->
                          raiseIllegalExpression expr
                            (sprintf "%s but found %s"
                               ("can only access struct members from a var of [pointer to] record type")
                               (typeName invalidType))
                      | TypeError (fe,m,f,e) ->
                          raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
                    end
            in
            let fieldptr = `GetFieldPointerIntrinsic (recordForm', fieldName) in
            match typeCheck env.bindings fieldptr with
              | TypeOf _ -> Result( env.bindings, toplevelForms @ moreForms @ [fieldptr] )
              | TypeError (fe,m,f,e) ->
                  begin
                    raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
                  end
          end
      | _ ->
          errorFromExpr expr "expected two arguments: 'fieldptr structExpr id'"
  let translateGetfieldptrD = "structExpr, fieldName", translateGetfieldptr

  let translateCast (env :exprTranslateF env) expr =
    match expr.args with
      | [targetTypeExpr; valueExpr] ->
          begin
            match translateType env.bindings targetTypeExpr with
              | Result targetType ->
                begin
                  let _, valueForm, toplevelForms = translateToForms env.translateF env.bindings valueExpr in
                  match typeCheck env.bindings valueForm with
                    | TypeOf valueType ->
                      if Semantic.equalTypes env.bindings targetType valueType then
                        Result( env.bindings, toplevelForms @ [(valueForm :> formWithTLsEmbedded)] )
                      else
                        let castForm = `CastIntrinsic( targetType, valueForm ) in
                        Result( env.bindings, toplevelForms @ [castForm] )
                    | TypeError (fe,m,f,e) ->
                      errorFromTypeError env.bindings valueExpr (fe,m,f,e)
                end
              | Error msgs ->
                Error msgs
          end
      | _ ->
          errorFromExpr expr "expected 'cast typeExpr valueExpr'"
  let translateCastD = "typeExpr, valueExpr", translateCast

  let translateDefineVar (env :exprTranslateF env) expr :translationResult =
    let transformUnsafe id name typeExpr valueExpr :translationResult =
      let declaredType = match typeExpr with
        | Some e ->
            begin
              match translateType env.bindings e with
                | Result t -> Some t
                | Error _ -> raise (CouldNotParseType (Ast2.expression2string e))
            end
        | None ->
          None
      in
      let valueType, toplevelForms, implForms =
        match valueExpr with
          | Some valueExpr ->
              begin
                let _, simpleform = env.translateF env.bindings valueExpr in
                let toplevelForms, implForms = extractToplevelForms simpleform in
                match typeCheck env.bindings (`Sequence implForms) with
                  | TypeOf t -> Some t, toplevelForms, implForms
                  | TypeError (fe, m,f,e) -> raiseIllegalExpressionFromTypeError valueExpr (fe,m,f,e)
              end
          | None -> None, [], []
      in
      let varType =
        match declaredType, valueType with
          | Some declaredType, Some valueType
            when equalTypes env.bindings declaredType valueType ->
              declaredType
          | Some declaredType, Some valueType ->
              raiseIllegalExpressionFromTypeError expr
                (Semantic.Ast expr, "types do not match",declaredType,valueType)
          | None, Some valueType -> valueType
          | Some declaredType, None -> declaredType
          | None, None ->
              raiseIllegalExpression expr "var needs either a default value or declare a type"
      in
      match varType with
        | #integralType | `Pointer _ | `Function _ as typ ->
            begin
              let var = variable name typ MemoryStorage false expr.location in
              let defvar = `DefineVariable (var, Some (`Sequence implForms))
              in
              match typeCheck env.bindings defvar with
                | TypeOf _ -> Result( addVar env.bindings var, toplevelForms @ [defvar] )
                | TypeError (fe,m,f,e) -> raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
            end
        | `Array(memberType, size) as typ ->
            begin
              let var = variable name typ MemoryStorage false expr.location in
              let defvar = `DefineVariable (var, match implForms with [] -> None | _ -> Some (`Sequence implForms)) in
              match typeCheck env.bindings defvar with
                | TypeOf _ -> Result( addVar env.bindings var, toplevelForms @ [defvar] )
                | TypeError (fe,m,f,e) -> raiseIllegalExpressionFromTypeError expr (fe,m,f,e)
            end
        | (`Record _ as typ) ->
            begin
              let value =
                match valueExpr with
                  | None -> None
                  | Some valueExpr -> Some (`Sequence implForms)
              in
              let var = variable name typ MemoryStorage false expr.location in
              Result( addVar env.bindings var, toplevelForms @ [`DefineVariable (var, value)] )
            end
        | `TypeRef _ ->
            raiseIllegalExpression expr "internal error: received unexpected type ref"
        | `TypeParam ->
            raiseIllegalExpression expr "cannot define local variable of type parameter type"
        | `ParametricType _ ->
            raiseIllegalExpression expr "cannot define local variable of parametric type"
        | `ErrorType _ as t ->
          raiseIllegalExpression expr (sprintf "cannot define local variable of %s"
                                         (typeName t))
    in
    let transform id name typeExpr valueExpr :translationResult =
      try
        let isRedefinitionError =
          hasRedefinitionErrors `NewVar `Local name expr env.bindings reportDiagnostics
        in

        if isRedefinitionError then
          Error []
        else
          transformUnsafe id name typeExpr valueExpr
      with
          IllegalExpression (_, errors) ->
            Error errors
    in
    match expr with
      | { id = id; args = [
            typeExpr;
            { id = name; args = [] };
            valueExpr
          ] }
          when (id = macroVar) ->
          transform id name (Some typeExpr) (Some valueExpr)

      | { id = id; args = [
            typeExpr;
            { id = name; args = [] };
          ] }
          when (id = macroVar) ->
          transform id name (Some typeExpr) None

      | { id = id; args = [
            { id = name; args = [] };
            valueExpr
          ] }
          when id = "var2" ->
          transform id name None (Some valueExpr)

      | _ ->
          if expr.id == "var" then
            errorFromExpr expr "expected var typeExpr nameId valueExpr"
          else if expr.id == "var2" then
            errorFromExpr expr "expected var2 nameId valueExpr"
          else
            errorFromExpr expr
              (sprintf "%s, invoked handler for '%s' but can only handle %s and %s"
                 "internal compiler error"
                 expr.id macroVar macroVar2)
  let translateDefineVarD = "type, name, [value] | var2 name, value", translateDefineVar

  let translateFuncCall (env : 'a env)  expr :translationResult =
    let buildCall name rettype argTypes isPointer hasVarArgs bindings args =
      let evalArg (argExpr :Ast2.sexpr) paramType =
        let _, xforms = env.translateF bindings argExpr in
        let toplevelForms, forms = extractToplevelForms xforms in
        let argForm =
          match paramType with
            | Some t when isTypeParametric t ->
                (* | Some ((`ParametricType _) as t) -> *)
              printf "parametric type param %s\n" (typeDescr (t :> typ));
              `CastIntrinsic (t, toSingleForm forms)
            | _ ->
              toSingleForm forms
        in
        (** TODO add cast if the parameter is a parametric type *)
        toplevelForms, argForm
      in
      let rec evalArgs argExprs paramTypes =
        match argExprs, paramTypes with
          | argExpr :: remArgs, paramType :: remParams ->
            evalArg argExpr (Some paramType) :: evalArgs remArgs remParams
          | argExpr :: remArgs, [] ->
            evalArg argExpr None :: evalArgs remArgs []
          | [], []->
            []
          | [], _ ->
            failwith "too many argument types in translateFuncCall"
      in
      let paramCount = List.length argTypes
      and argCount = List.length args in
      if (hasVarArgs && argCount < paramCount) then
        errorFromExpr expr
          (sprintf "expected %d parameters or more, but called with %d args"
             paramCount argCount)
      else if (not hasVarArgs && argCount != paramCount) then
        errorFromExpr expr
          (sprintf "expected %d parameters, but called with %d args"
             paramCount argCount)
      else
        let x = evalArgs args argTypes in
        let toplevelForms, argForms = flattenLeft x in
        let funccall = `FuncCall 
          (Lang.funcCall
             ~name
             ~rettype
             ~params:argTypes
             ~args:argForms
             ~ptr:isPointer
             ~varargs:hasVarArgs)
        in
        match typeCheck bindings funccall with
          | TypeOf _ ->
            Result( bindings, toplevelForms @ [funccall] )
          | TypeError (fe,msg,f,e) ->
            errorFromExpr expr (typeErrorMessage bindings (fe,msg,f,e))
    in
    match expr with
      | { id = funcall;
          args = { id = name; args = [] } :: args }
          when funcall = macroFunCall ->
        begin
          match lookup env.bindings name with
            | FuncSymbol func ->
              begin
                buildCall name func.rettype
                  (List.map snd func.fargs)
                  `NoFuncPtr
                  func.cvarargs
                  env.bindings
                  args
              end
            | VarSymbol var ->
              begin
                match var.typ with
                  | `Pointer `Function ft ->
                    begin
                      buildCall name ft.returnType ft.argTypes
                        `FuncPtr false env.bindings args
                    end
                  | _ ->
                    errorFromExpr
                      expr
                      (sprintf "trying to call variable '%s' as a function but has type %s"
                         var.vname
                         (typeName var.typ))
              end
            | _ ->
              errorFromExpr expr (sprintf "%s is neither a function nor a macro" name)
        end
      | _ ->
        errorFromExpr expr "unrecognized AST shape"
  let translateFuncCallD = "name, args...", translateFuncCall

  (** called by translateApply, not registered under any name *)
  let translateRecordLiteral (env :exprTranslateF env) typeExpr argExprs :translationResult =
    let translate recordType =
      let translateField expr : 'a mayfail =
        let bindings, formsWTL = env.translateF env.bindings expr in
        let tlforms, forms = extractToplevelForms formsWTL in
        Result (tlforms, forms)
      in
      begin match translateStructLiteralArgs env.bindings recordType.rname typeExpr.location recordType.fields argExprs translateField with
        | Result fieldFormsTL ->
          let alltlformsLst, fieldForms = List.split
            (List.map (fun (name, (tl, f)) -> tl, (name, f)) fieldFormsTL)
          in
          let alltlforms = List.flatten alltlformsLst in

          let translate nameAndFormList =
            let rec onlyConstantsSoFar nameAndFormList accum =
              match nameAndFormList with
                | [] ->
                  `AllConstant (List.rev accum)
                | (name, [`Constant value]) :: rem ->
                  onlyConstantsSoFar rem ((name, value) :: accum)
                | _ ->
                  let valueToFormList (name, value) =
                    name, [(`Constant value :> Lang.form)]
                  in
                  hadComplexExprs nameAndFormList (List.map valueToFormList accum)
            and hadComplexExprs nameAndFormList accum =
              match nameAndFormList with
                | [] ->
                  `ComplexExprs accum
                | nameAndForm :: rem ->
                  hadComplexExprs rem (nameAndForm :: accum)
            in
            onlyConstantsSoFar nameAndFormList []
          in

          begin match translate fieldForms with
            | `AllConstant nameAndValueList ->
              let c : formWithTLsEmbedded =
                `Constant (RecordVal (typeName (`Record recordType), nameAndValueList))
              in
              Result (env.bindings, alltlforms @ [c])

            | `ComplexExprs fieldsAndExprs ->
              let newBindings, recordVar =
                getNewLocalVar env.bindings (`Record recordType)
              in
              let recordVarAddress = `GetAddrIntrinsic recordVar in
              let makeFieldAssignment (name, forms) =
                let ptr = `GetFieldPointerIntrinsic (recordVarAddress, name) in
                `StoreIntrinsic (ptr, toSingleForm forms)
              in
              let assignments = List.map makeFieldAssignment fieldsAndExprs in
              Result (newBindings,
                      alltlforms
                      @ [`DefineVariable (recordVar, None)]
                      @ assignments
                      @ [`Variable recordVar])
          end
        | Error msgs ->
          Error msgs
      end
    in
    let fail () =
      errorFromExpr
        (Ast2.expr "(record literal)" (typeExpr :: argExprs))
        (sprintf "%s is not a struct" (Ast2.toString typeExpr))
    in
    match lookup env.bindings typeExpr.id with
      | TypedefSymbol (`Record recordType) ->
        translate recordType
      | UndefinedSymbol when typeExpr.id = "op!" ->
        begin match translateType env.bindings typeExpr with
          | Result (`Record recordType) ->
            translate recordType
          | _ ->
            fail()
        end
      | _ ->
        fail()

  let translateApply translateRecordF (env :('a someTranslateF) env) expr =
    let allAreFieldAssignments exprs =
      let isFieldAssignment = function
        | { id = "op="; args = [{ args = [] }; _] } ->
          true
        | _ ->
          false
      in
      List.for_all isFieldAssignment exprs
    in

    match expr with
      | { args = firstArg :: remArgs } ->
        let translateConstructorCall() =
          let toConstructorExpr expr =
            { expr with
              Ast2.id = macroConstructor;
              args = { firstArg with args = []} :: remArgs }
          in
          Result (env.translateF env.bindings $ toConstructorExpr expr)
        in

        let translateNonConstructorCall() =
          match firstArg.args, lookup env.bindings firstArg.id with
            | [], FuncSymbol _
            | [], VarSymbol { typ = `Pointer `Function _ } ->
              Result( env.translateF env.bindings { expr with id = Lang.macroFunCall } )

            | [], TypedefSymbol _ ->
              Error [Serror.fromExpr expr "internal error"]

            | [], _ ->
              let r = env.translateF env.bindings { (Ast2.shiftLeft expr.args) with location = expr.location } in
              Result r

            | (_::_), _ ->
              let tmpName = getUnusedName ~prefix:"opcall_func" env.bindings in
              let tempVar = Ast2.expr "std:base:localVar" [Ast2.idExpr tmpName; firstArg] in
              let callExpr = Ast2.expr "std:base:apply" (Ast2.idExpr tmpName :: remArgs) in
              let r = env.translateF env.bindings { (Ast2.seqExpr [tempVar; callExpr]) with location = expr.location } in
              Result r
        in
        begin match translateType env.bindings firstArg with
          | Result `Record _ when allAreFieldAssignments remArgs ->
            translateRecordF env firstArg remArgs

          | Result _ ->
            translateConstructorCall()

          | _ ->
            translateNonConstructorCall()
        end

      | { args = [] } ->
        errorFromExpr expr (sprintf "expected 'std:base:apply expr args?'")

  let translateError (env :exprTranslateF env) expr =
    Error [parseErrorExpr expr]
  let translateErrorD = "string-literal expr?", translateError

  let register addF =
    addF "std:base:localVar" translateDefineLocalVarD;
    addF macroAssign translateAssignVarD;
    addF macroSequence translateSeqD;
    addF macroSeqOp translateSeqD;
    addF macroReturn translateReturnD;
    addF macroLabel translateLabelD;
    addF macroBranch translateBranchD;
    addF macroLoad translateLoadD;
    addF macroStore translateStoreD;
    addF macroMalloc translateMallocD;
    addF macroCast translateCastD;
    addF macroFieldptr translateGetfieldptrD;
    addF macroPtradd translatePtraddD;
    addF macroPtrDiff translatePtrDiffD;
    addF macroGetaddr translateGetaddrD;
    addF macroNullptr translateNullptrD;
    addF macroVar translateDefineVarD;
    addF macroVar2 translateDefineVarD;
    addF macroFunCall translateFuncCallD;
    addF macroApply ("ast...", translateApply translateRecordLiteral);
    addF macroError translateErrorD
end

module Compiler_environment : Zomp_transformer =
struct
  let translateFileName (env :exprTranslateF env) (expr :Ast2.sexpr)  :translationResult =
    let newBindings, var = getNewGlobalVar env.bindings (`Pointer `Char) in
    let value = StringLiteral (Ast2.fileName expr) in
    let gvar = Lang.globalVarDef ~var ~initial:value ~location:expr.location in
    Result (newBindings, [`ToplevelForm (`GlobalVar gvar); `Variable var])

  let translateLineNumber (env :exprTranslateF env) (expr :Ast2.sexpr)  :translationResult =
    Result (env.bindings, [`Constant (Int32Val (Int32.of_int (Ast2.lineNumber expr)))])

  let singleArgWithLoc = function
    | { id = _; args = [{ location = Some loc } as arg]} ->
      Result arg
    | { id = _; args = [{ location = None } as arg]} ->
      errorFromExpr arg "source location got lost"
    | arg ->
      errorFromExpr arg "expected single argument"

  let translateFileNameOf env expr =
    match singleArgWithLoc expr with
      | Result expr -> translateFileName env expr
      | Error errors -> Error errors

  let translateLineNumberOf env expr =
    match singleArgWithLoc expr with
      | Result expr -> translateLineNumber env expr
      | Error errors -> Error errors

  let register addF =
    addF "std:env:file" ("char*", translateFileName);
    addF "std:env:fileOf" ("char*", translateFileNameOf);
    addF "std:env:line" ("int", translateLineNumber);
    addF "std:env:lineOf" ("int", translateLineNumberOf)
end

module Arrays : Zomp_transformer =
struct
  let arraySize (env: exprTranslateF env) expr =
    match expr with
      | { id = _; args = [arrayExpr] } ->
        let _, rightHandForm, toplevelForms =
          translateToForms env.translateF env.bindings arrayExpr
        in
        begin match typeCheck env.bindings rightHandForm with
          | TypeOf `Array(_, size) ->
            Result (env.bindings,
                    toplevelForms @ [(rightHandForm :> formWithTLsEmbedded);
                                     `Constant (Int32Val (Int32.of_int size))])
          | TypeOf invalidType ->
            errorFromTypeError env.bindings arrayExpr
              (Semantic.Ast expr,
               "cannot get size of array",
               invalidType,
               `Any "array")
          | TypeError (fe,m,f,e) ->
            errorFromTypeError env.bindings arrayExpr (fe,m,f,e)
        end
      | _ ->
        errorFromExpr expr "expected 'zmp:array:size arrayExpr'"
  let arraySizeD = "array", arraySize

  let arrayAddr (env: exprTranslateF env) = function
    | {args = [arrayPtrExpr]} as expr ->
      let _, arrayPtrForm, tlforms =
        translateToForms env.translateF env.bindings arrayPtrExpr
      in
      begin match typeCheck env.bindings arrayPtrForm with
        | TypeOf `Pointer `Array(memberType,_) ->
          begin
            Result(
              env.bindings, tlforms @ [
                `CastIntrinsic(`Pointer memberType, arrayPtrForm)])
          end
        | TypeOf invalidType ->
          errorFromTypeError env.bindings arrayPtrExpr
            (Semantic.Ast expr,
             "cannot get address of first element",
             invalidType, `Any "Pointer to array")
        | TypeError (fe, m,f,e) ->
          errorFromTypeError env.bindings expr (fe,m,f,e)
      end
    | invalidExpr ->
      errorFromExpr invalidExpr "expected 'zmp:array:addr arrayExpr'"
  let arrayAddrD = "arrayPtr", arrayAddr

  let register addF =
    addF "zmp:array:size" arraySizeD;
    addF "zmp:array:addr" arrayAddrD
end

module Overloaded_ops : Zomp_transformer =
struct
  (** creates a macro which turns baseName(l,r) into
    * baseName_ltype_rtype(l,r). Has special handling for op+/op- and pointer
    * arguments *)
  let overloadedOperator baseName (env :exprTranslateF env) = function
    | {args = [leftExpr; rightExpr]} as expr ->
        begin
          let _, leftForm, toplevelFormsLeft =
            translateToForms env.translateF env.bindings leftExpr
          in
          let _, rightForm, toplevelFormsRight =
            translateToForms env.translateF env.bindings rightExpr
          in
          let tc = typeCheck env.bindings in
          match baseName, tc leftForm, tc rightForm with
            | "op+", TypeOf `Pointer _, TypeOf `Int32 ->
                begin
                  Result(env.bindings,
                         toplevelFormsLeft @ toplevelFormsRight @
                         [`PtrAddIntrinsic (leftForm, rightForm)])
                end
            | "op-", TypeOf `Pointer _, TypeOf `Int32 ->
                begin
                  Result(env.bindings,
                         toplevelFormsLeft @ toplevelFormsRight @
                           [`PtrAddIntrinsic 
                               (leftForm,
                                `FuncCall (Lang.funcCall 
                                             ~name:"u32:neg"
                                             ~rettype:`Int32
                                             ~params:[`Int32]
                                             ~args:[rightForm]
                                             ~ptr:`NoFuncPtr
                                             ~varargs:false))])
                end
            | "op-", TypeOf `Pointer _, TypeOf `Pointer _ ->
              begin
                Result(env.bindings,
                       toplevelFormsLeft @ toplevelFormsRight @
                       [`PtrDiffIntrinsic (leftForm, rightForm)])
              end
            | _, TypeOf leftType, TypeOf rightType ->
                begin
                  let postfixAndImplConv form = function
                    | `Pointer _ ->
                        "ptr", `CastIntrinsic(`Pointer `Void, form)
                    | typ ->
                        typeName typ, form
                  in
                  let typenameL, castFormL = postfixAndImplConv leftForm leftType in
                  let typenameR, castFormR = postfixAndImplConv rightForm rightType in
                  let funcName = baseName ^ "_" ^ typenameL ^ "_" ^ typenameR in
                  match Bindings.lookup env.bindings funcName with
                    | FuncSymbol func -> begin
                        Result(env.bindings,
                               toplevelFormsLeft @ toplevelFormsRight @
                                 [`FuncCall (Lang.funcCall
                                              ~name:func.fname
                                              ~rettype:func.rettype
                                              ~params:(List.map snd func.fargs)
                                              ~args:[castFormL; castFormR]
                                              ~ptr:`NoFuncPtr
                                              ~varargs:false)])
                      end
                    | _ -> begin
                      errorFromExpr expr
                        (sprintf "no overload for %s(%s, %s) found (expected function %s)"
                           baseName typenameL typenameR funcName)
                      end
                end
            | _, lresult, rresult ->
                let typeErrorMessagePotential potError =
                  match potError with
                    | TypeError (fe,msg,found,expected) ->
                      Some (serrorFromTypeError env.bindings expr (fe, msg, found, expected))
                    | _ ->
                      None
                in
                Error (mapFilter typeErrorMessagePotential [lresult; rresult])
        end
    | invalidExpr ->
      errorFromExpr invalidExpr "expected two arguments"

  let overloadedFunction baseName (env :exprTranslateF env) = function
    | {args = [argExpr]} as expr ->
        begin
          let _, argForm, toplevelForms =
            translateToForms env.translateF env.bindings argExpr
          in
          match typeCheck env.bindings argForm with
            | TypeOf argType ->
              begin
                let funcName = baseName ^ "_" ^ typeName argType in
                match Bindings.lookup env.bindings funcName with
                  | FuncSymbol func ->
                    Result(env.bindings,
                           toplevelForms @
                             [`FuncCall (Lang.funcCall
                                          ~name:func.fname
                                          ~rettype:func.rettype
                                          ~params:(List.map snd func.fargs)
                                          ~args:[argForm]
                                          ~ptr:`NoFuncPtr
                                          ~varargs:false)])
                  | _ ->
                    let genericFuncName = baseName ^ "_ptr" in
                    match Bindings.lookup env.bindings genericFuncName with
                      | FuncSymbol func ->
                        Result(env.bindings,
                               toplevelForms @
                                 [`FuncCall (Lang.funcCall
                                              ~name:func.fname
                                              ~rettype:func.rettype
                                              ~params:(List.map snd func.fargs)
                                              ~args:[`CastIntrinsic (`Pointer `Void, argForm)]
                                              ~ptr:`NoFuncPtr
                                              ~varargs:false)])
                      | _ ->
                        errorFromExpr expr
                          (sprintf
                              "no overload for %s(%s) found (expected function %s or %s)"
                                  baseName (typeName argType) funcName genericFuncName)
              end
            | TypeError (fe,m,f,e) ->
                errorFromTypeError env.bindings expr (fe,m,f,e)
        end
    | invalidExpr ->
      errorFromExpr invalidExpr "expected one argument"

  let register addF =
    let addOp (name, op) =
      addF ("zmp:cee:" ^ name) ("T, T", overloadedOperator op)
    in
    List.iter addOp [
      "add", "op+";
      "sub", "op-";
      "mul", "op*";
      "div", "op/";
      "equal", "op==";
      "notEqual", "op!=";
      "greater", "op>";
      "greaterEqual", "op>=";
      "less", "op<";
      "lessEqual", "op<=";
      "and", "op&";
      "or", "op|";
      "xor", "op^";
      "shl", "op<<";
      "shr", "op>>";
      "pow", "op**";
    ];
    let addFun name = addF ("zmp:cee:" ^ name) ("T", overloadedFunction name) in
    List.iter addFun
      ["print"; "write";
       "toInt"; "toFloat"; "toDouble"; "toBool"; "toChar"; "toCString";
       "neg"; "not"]
end

let traceMacroExpansion = ref (Some (fun (_ :string) (_ :sexpr) -> ()))
let setTraceMacroExpansion f = traceMacroExpansion := f

let traceToplevelFrom = ref None
let setTraceToplevelForm f = traceToplevelFrom := f

let lookupBaseInstruction, addBaseInstruction, foreachBaseInstructionDoc =
  let table = Hashtbl.create 32 in
  let documentation = Hashtbl.create 32 in
  let add name (doc, f) =
    Hashtbl.add documentation name doc;
    Hashtbl.add table name f
  in
  Base.register add;
  Arrays.register add;
  Overloaded_ops.register add;
  Compiler_environment.register add;
  let addBaseInstruction name doc f = add name (doc, f) in
  let lookup name =
    try
      Some (Hashtbl.find table name)
    with Not_found ->
      None
  in
  lookup, addBaseInstruction, (fun f -> Hashtbl.iter f documentation)

let rec translateNested
    (env :exprTranslateF env)
    (expr :Ast2.sexpr)
    : (bindings * formWithTLsEmbedded list) mayfail
    =

  let toConstructorExpr expr =
    { expr with
      Ast2.id = macroConstructor;
      args = { expr with args = []} :: expr.args }
  in

  Zompvm.currentBindings := env.bindings;
  match Bindings.lookup env.bindings expr.id with
    | VarSymbol var ->
      Mayfail.result (env.bindings, [`Variable var])

    | FuncSymbol func ->
      (** TODO: reverse this. Turn opcall into a macro *)
      translateNested env
        { expr with id = macroFunCall; args = idExpr expr.id :: expr.args }

    | MacroSymbol macro ->
      (match Old_macro_support.translateMacroCall env expr with
        | Some r -> Result r
        | None ->
          Mayfail.singleError $ Serror.fromExpr expr "failed after macro expansion")

    | TypedefSymbol typedef ->
      translateNested env $ toConstructorExpr expr

    | LabelSymbol label ->
      Mayfail.singleError $ Serror.fromExpr expr "label can only be used as argument to control flow instructions"

    | UndefinedSymbol ->
      let translateConstantOrFail bindings expr =
        let failWithInvalidId() =
          Mayfail.singleError $ Serror.fromExpr expr (sprintf "unknown identifier %s" expr.id)
        in

        match expr with
          | { id = name; args = [] } ->
            begin match string2integralValue name with
              | Some StringLiteral string ->
                begin
                  let newBindings, var = getNewGlobalVar bindings (`Pointer `Char) in
                  let value = StringLiteral string in
                  let gvar = Lang.globalVarDef ~var ~initial:value ~location:expr.location in
                  Mayfail.result (newBindings, [`ToplevelForm (`GlobalVar gvar); `Variable var])
                end
              | Some const ->
                Mayfail.result (bindings, [`Constant const])
              | None ->
                failWithInvalidId()
            end
          | _ ->
            failWithInvalidId()
      in
      match lookupBaseInstruction expr.id with
        | Some f ->
          begin
            Zompvm.currentBindings := env.bindings;
            f env expr
          end
        | None ->
          translateConstantOrFail env.bindings expr

and translateNestedNoErr bindings expr =
  let env = makeEnv bindings in
  match translateNested env expr with
    | Result (newBindings, forms) -> newBindings, forms
    | Error errors ->
      raiseIllegalExpressions expr errors

and makeEnv bindings = {
  bindings = bindings;
  translateF = translateNestedNoErr;
  translateExpr = translateNested;
  parseF = Parseutils.parseIExprsOpt;
  reportError = reportError;
}

let catchingErrorsDo f ~onErrors =
  let onErrorMsg msg = onErrors [Serror.fromMsg None msg] in
  begin
    try
      f()
    with
      | IllegalExpression (expr, errors) ->
        onErrors errors
      | CouldNotParseType descr ->
        onErrorMsg $ sprintf "unknown type: %s\n" descr
      | Genllvm.CodeGenError msg ->
        onErrorMsg $ sprintf "codegen failed: %s\n" msg
      | FailedToEvaluateLLVMCode (llvmCode, errorMsg) ->
        onErrorMsg $ sprintf "could not evaluate LLVM code: %s\n%s\n" errorMsg llvmCode
      | Failure msg ->
        onErrorMsg $ sprintf "internal error: exception Failure(%s)\n" msg
  end

module EnvTL : sig
  type t
  val create : 
    emitBackendCodeForForm:(Lang.toplevelExpr -> unit) ->
    lookupTLInstruction:(string -> (t -> Ast2.t -> unit) option) ->
    Bindings.t -> 
    t

  val bindings : t -> Bindings.t
  val hasErrors : t -> bool

  val reset : t -> Result.flag * Serror.t list * toplevelExpr list

  val emitExpr : t -> Ast2.t -> unit
  val emitExprs : t -> Ast2.t list -> unit

  val setBindings : t -> Bindings.t -> unit
  val emitError : t -> Serror.t -> unit
  val emitErrors : t -> Serror.t list -> unit
  val emitForm : t -> toplevelExpr -> unit
  val emitForms : t -> toplevelExpr list -> unit
  (** Used for legacy translator functions that fail without reporting any errors. *)
  val emitSilentError : t -> unit
end = struct
  type t = {
    mutable tlbindings :Bindings.t;
    mutable tlerrorsRev :Serror.t list;
    mutable tlexprsRev :toplevelExpr list;
    mutable tlHadSilentErrors :bool;

    tlEmitForm :toplevelExpr -> unit;
    tlEmitError :Serror.t -> unit;
    tlEmitBackendCode : Lang.toplevelExpr -> unit;
    tlLookupInstruction : string -> (t -> Ast2.t -> unit) option;
  }

  let bindings e = e.tlbindings
  let hasErrors e = e.tlHadSilentErrors || (e.tlerrorsRev <> [])

  let setBindings env bindings =
    Zompvm.currentBindings := bindings;
    env.tlbindings <- bindings

  let emitError env error =
    env.tlEmitError error
  let emitForm env form =
    env.tlEmitBackendCode form;
    env.tlEmitForm form

  let emitSilentError env =
    env.tlHadSilentErrors <- true

  let rec emitExprUnsafe tlenv expr : unit =
    begin match !traceMacroExpansion with
      | Some f -> f "translate/???" expr
      | None -> ()
    end;

    match Bindings.lookup (bindings tlenv) expr.id with
      | MacroSymbol macro ->
        let newAst = macro.mtransformFunc (bindings tlenv) expr in
          (* TODO: fix locations, see Old_macro_support.translateMacroCall *)
        emitExprUnsafe tlenv newAst
      | VarSymbol _ | FuncSymbol _ | TypedefSymbol _ | LabelSymbol _ as sym ->
        emitError tlenv (Serror.fromExpr expr $ sprintf "%s at toplevel not allowed" (kindToString sym))
      | UndefinedSymbol ->
        begin
          match tlenv.tlLookupInstruction expr.id with
            | Some handler ->
              handler tlenv expr;
            | None ->
              emitError tlenv (Serror.fromExpr expr $ sprintf "%s is undefined" expr.id)
        end

  let emitExpr env expr =
    catchingErrorsDo (fun () ->
      emitExprUnsafe env expr)
      ~onErrors:(List.iter (emitError env))

  let emitErrors env = List.iter (emitError env)
  let emitForms env = List.iter (emitForm env)
  let emitExprs env exprs = List.iter (emitExpr env) exprs

  let reset tlenv =
    let errors = List.rev tlenv.tlerrorsRev in
    let forms = List.rev tlenv.tlexprsRev in
    let flag = if hasErrors tlenv then Result.Fail else Result.Success in
    tlenv.tlerrorsRev <- [];
    tlenv.tlexprsRev <- [];
    tlenv.tlHadSilentErrors <- false;
    flag, errors, forms

  let create ~emitBackendCodeForForm ~lookupTLInstruction (initialBindings :bindings) =
    let rec env = {
      tlbindings = initialBindings;
      tlerrorsRev = [];
      tlexprsRev = [];
      tlHadSilentErrors = false;
      tlEmitError = (fun error -> env.tlerrorsRev <- error :: env.tlerrorsRev);
      tlEmitForm = (fun form -> env.tlexprsRev <- form :: env.tlexprsRev);
      tlEmitBackendCode = emitBackendCodeForForm;
      tlLookupInstruction = lookupTLInstruction;
    } in
    env
end

let () =
  addBaseInstruction macroMacro "name, args..., body" Macros.translateDefineNestedMacro

let compilationSwallowedErrors = ref false

let matchFunc tlenv expr =
  let scanParams args =
    let argCount = List.length args in
    let module StringSet = Set.Make(String) in
    let varargs = ref `NoVarArgs in

    let rec loop previousNames args =
      match args with
        | [{ id = "postop..."; args = [{ id = "cvarargs"; args = [] }] }] ->
          varargs := `HasVarArgs;
          []
        | [] ->
          []
        | { id = opjux; args = [typeExpr; {id = name; args = []; location}] } :: remArgs
            when opjux = macroJuxOp ->
          let name, names =
            if StringSet.mem name previousNames then begin
              EnvTL.emitError tlenv $ Serror.fromMsg location
                (sprintf "redefinition of parameter %s" name);
              "_", previousNames
            end else
              name, StringSet.add name previousNames
          in
          (name, typeExpr) :: loop names remArgs
        | invalidExpr :: remArgs ->
          let argNum = argCount + 1 - List.length args in
          let msg = sprintf "%s parameter needs to have shape 'typeExpr id'" $ formatNth argNum in
          let error = Serror.fromExpr invalidExpr msg in
          EnvTL.emitError tlenv error;
          ("_", Ast2.idExpr (typeName $ `ErrorType "")) :: loop previousNames remArgs
    in

    loop StringSet.empty args, !varargs
  in

  let isQuoted str =
    let strLength = String.length str in
    strLength >= 2 && str.[0] = '"' && str.[strLength-1] = '"'
  in

  let validateName name =
    let nameRE = "^[a-zA-Z0-9][^\"]*$" in
    if isQuoted name || name =~ nameRE then
      name
    else begin
      EnvTL.emitError tlenv $ Serror.fromExpr expr
        (sprintf "function names must match the following regexp: %s" nameRE);
      "_"
    end
  in

  match expr with
    (** func def from iexpr for polymorphic function *)
    | { id = id; args = [
          typeExpr;
          { id = opcall; args =
              { id = paramop; args =
                  { id = name; args = []; location }
                  :: paramTypeExprs }
              :: paramExprs };
          { id = opseq; args = _ } as implExpr
        ] }
        when id = macroFunc &&
          opcall = macroCallOp &&
          paramop = macroParamType &&
          opseq = macroSeqOp ->
        let location = someOrDefault location Basics.fakeLocation in
        begin try
          let getParametricTypeName = function
            | { id = name; args = [] } -> name
            | _ -> failwith ""
          in
          let params, hasvararg = scanParams paramExprs in
          let parametricTypes = List.map getParametricTypeName paramTypeExprs in
          `FuncDef (validateName name, typeExpr, params, hasvararg, implExpr, parametricTypes, location)
        with Failure _ ->
          `NotAFunc
        end

    (** func decl from iexpr with multiple/0 arguments **)
    | { id = id; args = [
          typeExpr;
          { id = opcall; args = { id = name; args = []; location } :: paramExprs }
        ] }
        when id = macroFunc && opcall = macroCallOp ->
        let location = someOrDefault location Basics.fakeLocation in
        begin try
          let params, hasvararg = scanParams paramExprs in
          `FuncDecl (validateName name, typeExpr, params, hasvararg, location)
        with Failure _ ->
          `NotAFunc
        end

    (** func def from iexpr with multiple/0 arguments **)
    | { id = id; args = [
          typeExpr;
          { id = opcall; args =
              { id = name; args = []; location }
              :: paramExprs };
          { id = opseq; args = _ } as implExpr
        ] }
        when id = macroFunc && opcall = macroCallOp && opseq = macroSeqOp ->
        let location = someOrDefault location Basics.fakeLocation in
        begin try
          let params, hasvararg = scanParams paramExprs in
          `FuncDef (validateName name, typeExpr, params, hasvararg, implExpr, [], location)
        with Failure _ ->
          `NotAFunc
        end

    | { id = "std:base:func"; args = _ } ->
        `NotAFunc
    | { id } when id = macroFunc ->
      `InvalidFunc "not a valid function"
    | _ ->
      `NotAFunc

let typeCheckTLImp env location form =
  match typeCheckTL (EnvTL.bindings env) form with
    | TypeOf typ ->
      typ
    | TypeError (fe, msg, found, expected) ->
      let msg = (typeErrorMessage (EnvTL.bindings env) (fe, msg, found, expected)) in
      EnvTL.emitError env $ Serror.fromMsg location msg;
      `ErrorType msg

let checkRedefinitionErrors tlenv newDefinitionType scope name expr bindings =
  let reportDiagnostics kind error =
    if kind = DiagnosticKind.Error then
      EnvTL.emitError tlenv error
    else
      reportDiagnostics kind error
  in
  hasRedefinitionErrors newDefinitionType scope name expr bindings reportDiagnostics

let translateTypeImp tlenv expr =
  match translateType (EnvTL.bindings tlenv) reportWarning expr with
    | Result typ ->
      typ
    | Error errors ->
      EnvTL.emitErrors tlenv errors;
      `ErrorType "translateTypeImp"

let checkFunctionIsValid tlenv location f =
  match Semantic.functionIsValid f with
    | `Ok -> ()
    | `Errors messages ->
      List.iter (fun msg -> EnvTL.emitError tlenv $ Serror.fromMsg location msg) messages

let rec translateFunc tlenv expr : unit =
  let bindings = EnvTL.bindings tlenv in
  let translateType bindings expr =
    translateType bindings reportWarning expr
  in
  let buildFunction bindings typ uncheckedName paramExprs hasvarargs implExprOption location =
    let name = removeQuotes uncheckedName in

    let translateParam (varName, typeExpr) =
      match translateType bindings typeExpr with
        | Result typ ->
          varName, typ
        | Error errors ->
          EnvTL.emitErrors tlenv errors;
          name, `ErrorType "arg"
    in

    let rec bindingsWithParams bindings params =
      let addParam bindings (name, typ) =
        let var =
          variable
            ~name
            ~typ
            (** all parameters are copied into a local var by genllvm *)
            ~storage:MemoryStorage
            ~global:false
            ~location:None
        in
        addVar bindings var
      in
      List.fold_left addParam bindings params
    in

    let params = List.map translateParam paramExprs in

    let innerBindings = bindingsWithParams bindings params in
    let impl = match implExprOption with
      | Some implExpr ->
        let innerEnv = makeEnv innerBindings in
        begin match translateNested innerEnv implExpr with
          | Result (_, nestedForms) ->
            let nestedTLForms, implForms = extractToplevelForms nestedForms in
            List.iter (fun (`ToplevelForm f) -> EnvTL.emitForm tlenv f) nestedTLForms;
            let implFormsWithFixedVars = moveLocalVarsToEntryBlock (`Sequence implForms) in
            Some (`Sequence implFormsWithFixedVars)
          | Error errors ->
            EnvTL.emitErrors tlenv errors;
            if errors = [] then
              EnvTL.emitError tlenv $ Serror.fromExpr expr (sprintf "internal error: failed to compile function %s" name);
            Some (`Sequence [`Return (`Constant (Types.defaultValue typ))])
        end
      | None -> None
    in
    let f = (match hasvarargs with `HasVarArgs -> varargFunc | `NoVarArgs -> func) name typ params impl location in
    checkFunctionIsValid tlenv expr.location f;
    let newBindings =
      addFunc bindings f
    in
    let funcDef = `DefineFunc f in
    newBindings, funcDef
  in
  match matchFunc tlenv expr with
    | `FuncDef (name, typeExpr, paramExprs, hasvarargs, implExpr, parametricTypes, location) ->
      begin
        let bindingsWParametricTypes =
          List.fold_left
            (fun bindings parametricTypeName ->
              addTypedef bindings parametricTypeName `TypeParam location)
            bindings
            parametricTypes
        in
        match translateType bindings typeExpr with
          | Result typ -> begin
            let isRedefinitionError =
              checkRedefinitionErrors tlenv `NewFuncDef `Global name expr bindings
            in

            (** Add function declaration so it can be called in body *)
            let tempBindings, _ =
              buildFunction bindingsWParametricTypes typ name paramExprs hasvarargs None location
            in
            (** Add function definition *)
            let newBindings, funcDef =
              buildFunction tempBindings typ name paramExprs hasvarargs (Some implExpr) location
            in

            ignore (typeCheckTLImp tlenv expr.location funcDef);

            if (not isRedefinitionError) then begin
              EnvTL.setBindings tlenv newBindings;
              EnvTL.emitForm tlenv funcDef;
            end
          end
          | Error errors ->
            EnvTL.emitErrors tlenv errors
      end
    | `FuncDecl (name, typeExpr, paramExprs, hasvarargs, location) ->
      begin
        let isRedefinitionError =
          checkRedefinitionErrors tlenv `NewFuncDecl `Global name expr bindings
        in
        let returnType = translateTypeImp tlenv typeExpr in
        let newBindings, funcDecl = buildFunction bindings returnType name paramExprs hasvarargs None location in
        if (not isRedefinitionError) then begin
          EnvTL.setBindings tlenv newBindings;
          EnvTL.emitForm tlenv funcDecl;
        end
      end
    | `NotAFunc ->
      EnvTL.emitSilentError tlenv
    | `InvalidFunc msg ->
      EnvTL.emitError tlenv $ Serror.fromExpr expr msg

let translateGlobalVar (env :EnvTL.t) expr : unit =
  match expr with
    | { id = _; args = [
      typeExpr;
      { id = name; args = [] };
      valueExpr
    ] } ->
      begin
        let isRedefinitionError =
          checkRedefinitionErrors env `NewVar `Global name expr (EnvTL.bindings env)
        in
        let typ =
          match translateTypeImp env typeExpr with
            | #integralType
            | `Pointer _
            | `Array (_, _)
            | `Record _ as typ ->
              typ
            | `Function _
            | `ParametricType _
            | `TypeParam
            | `TypeRef _ ->
              (* reportErrorE env typeExpr "type not supported for global variables"; *)
              EnvTL.emitError env $ Serror.fromExpr typeExpr "type not allowed for global variables";
              `ErrorType "translateGlobalVar"
            | `ErrorType _ as e ->
              e
        in
        let initialValue =
          match typ, valueExpr with
              (** env.translateExpr always introduces temporary for string literals *)
            | `Pointer `Char, { id = stringlit; args = [] } ->
              parseValue (`Pointer `Char) stringlit
              (** legacy special case *)
            | `Pointer targetType, { id = "null"; args = [] } ->
              NullpointerVal targetType
              (** legacy special case *)
            | `Record recordT, { id = "0"; args = [] } ->
              RecordVal (recordT.rname, [])
              (** legacy special case *)
            | `Array _, { id = "0"; args = [] } ->
              ArrayVal (typ, [])
              (** TODO: remove special cases above *)
            | _ ->
              let innerEnv = makeEnv (EnvTL.bindings env) in
              match translateNested innerEnv valueExpr with
                | Result( newBindings, formsWTL ) ->
                  begin
                    let tlforms, forms = extractToplevelForms formsWTL in
                    List.iter (fun (`ToplevelForm form) -> EnvTL.emitForm env form) tlforms;
                    EnvTL.setBindings env newBindings;
                    match forms with
                      | [`Constant value] ->
                        value
                      | _ ->
                        EnvTL.emitError env $ Serror.fromExpr expr "expecting a constant expression";
                        ErrorVal "translateGlobalVar"
                  end
                | Error errors ->
                  EnvTL.emitErrors env errors;
                  ErrorVal "translateGlobalVar"
        in
        let typeEquivalent lt rt =
          match lt, rt with
            | `Record { rname = rname; fields = [] }, `Record { rname = lname; fields = _ }
            | `Record { rname = rname; fields = _ }, `Record { rname = lname; fields = [] } ->
              (lname :string) = rname
            | _, _ ->
              lt = rt
        in
        if typeEquivalent typ (typeOf initialValue) then begin
          let var = globalVar name typ expr.location in
          let gvar = Lang.globalVarDef ~var ~initial:initialValue ~location:expr.location in
          EnvTL.setBindings env (addVar (EnvTL.bindings env) var);
          if (not isRedefinitionError) then begin
            EnvTL.emitForm env (`GlobalVar gvar :> toplevelExpr)
          end
        end else
          EnvTL.emitError env
            (Serror.fromExpr expr
               (sprintf "expected initial value to have type %s but found %s"
                  (typeDescr typ) (typeDescr (typeOf initialValue))))
      end
    | _ ->
      EnvTL.emitError env $ Serror.fromExpr expr "expected 'var typeExpr name initExpr'"

let translateApplyTL (env :EnvTL.t) expr : unit =
  try
    let argExpr = Ast2.shiftId expr.args in
    EnvTL.emitExpr env argExpr
  with Failure "shiftId" ->
      EnvTL.emitError env $ Serror.fromExpr expr "expected first argument to be an id expression"

let translateTypedef tlenv expr : unit =
  let bindings = EnvTL.bindings tlenv in

  let translateType bindings expr =
    translateType bindings reportWarning expr
  in

  let translateRecordTypedef bindings typeName location componentExprs expr =
    let isRedefinitionError =
      hasRedefinitionErrors `NewType `Global typeName expr bindings reportDiagnostics
    in

    let tempBindings = Bindings.addTypedef bindings typeName (`TypeRef typeName) location in
    let expr2component =
      let translate name typeExpr =
        match translateType tempBindings typeExpr with
          | Result typ ->
            name, typ
          | Error errors ->
            raiseIllegalExpressions typeExpr errors
      in
      function
        | { id = typeName; args = [{ id = componentName; args = []}] } ->
          translate componentName (Ast2.idExpr typeName)
        | { id = seq; args = [typeExpr; { id = componentName; args = [] }] }
            when seq = macroSequence || seq = macroJuxOp ->
          translate componentName typeExpr
        | _ ->
          raiseIllegalExpression
            expr
            "(type typeName (typeExpression componentName)* ) expected"
    in
    let components = List.map expr2component componentExprs in
    let recordType = `Record { rname = typeName; fields = components } in
    if isRedefinitionError then
      None
    else
      Some recordType
  in
  let returnRecordTypedef bindings name componentExprs location expr =
    match translateRecordTypedef bindings name location componentExprs expr with
      | Some rt ->
        let newBindings = addTypedef bindings name rt location in
        EnvTL.setBindings tlenv newBindings;
        EnvTL.emitForm tlenv (`Typedef (name, rt));
      | None ->
        EnvTL.emitError tlenv (Serror.fromMsg (Some location) "failed to translate type")
  in
  match expr with
    (** record with only one member *)
    | { id = id; args = [
      { id = typeName; args = []; location };
      { id = opseq; args = componentExprs }
    ] } as expr
        when id = macroTypedef && opseq = macroSeqOp ->
      let location = someOrDefault expr.location Basics.fakeLocation in
      returnRecordTypedef bindings typeName componentExprs location expr

    (** parametric record *)
    | { id = id; args = [
      { id = opcall; args = [
        { id = typeName; args = []; location };
        { id = "T"; args = [] } ] };
      { id = opseq; args = componentExprs }
    ] } as expr
        when id = macroTypedef && opcall = macroParamType && opseq = macroSeqOp ->
      begin
        let location = someOrDefault expr.location Basics.fakeLocation in
        let paramBindings = addTypedef bindings "T" `TypeParam location in
        match translateRecordTypedef paramBindings typeName location componentExprs expr with
          | Some recordType ->
            let parametricType = `ParametricType recordType in
            EnvTL.setBindings tlenv $ addTypedef bindings typeName parametricType location;
            EnvTL.emitForm tlenv $ `Typedef (typeName, parametricType);
          | None ->
            EnvTL.emitError tlenv (Serror.fromMsg (Some location) "failed to translate type 3")
      end

    (** type foo typeExpr *)
    | { id = id; args = [
      { id = newTypeName; args = []; location };
      targetTypeExpr;
    ] } as expr
        when id = macroTypedef ->
      begin
        let location = someOrDefault expr.location Basics.fakeLocation in
        let isRedefinitionError =
          hasRedefinitionErrors `NewType `Global newTypeName expr bindings reportDiagnostics
        in

        match translateType bindings targetTypeExpr, isRedefinitionError with
          | Error _, _ ->
            raiseInvalidType targetTypeExpr
          | Result t, false ->
            EnvTL.setBindings tlenv (addTypedef bindings newTypeName t location);
            EnvTL.emitForm tlenv (`Typedef (newTypeName, t));
          | _, true ->
            EnvTL.emitError tlenv (Serror.fromMsg (Some location) "failed to translate type2")
      end

    (** record typedef *)
    | { id = id; args =
        { id = typeName; args = []; location }
               :: componentExprs
      } as expr
        when id = macroTypedef ->
      let location = someOrDefault expr.location Basics.fakeLocation in
      returnRecordTypedef bindings typeName componentExprs location expr
    | _ ->
      EnvTL.emitError tlenv (Serror.fromExpr expr "failed to translate type3")

let translateError tlenv expr : unit =
  EnvTL.emitError tlenv $ parseErrorExpr expr

let translateDefineTLMacro env expr =
  let nestedEnv = makeEnv (EnvTL.bindings env) in
  match Macros.translateDefineMacroHelper `Global nestedEnv expr with
    | Result newBindings ->
      EnvTL.setBindings env newBindings
    | Error errors ->
      EnvTL.emitErrors env errors

let translateDefineReplacementMacro tlenv expr : unit =
  match expr with
    | { id = id; args =
        { id = name; args = [] } :: paramImpl
      } when id = macroReplacement ->
      let isRedefinitionError =
        hasRedefinitionErrors `NewMacro `Global name expr (EnvTL.bindings tlenv) reportDiagnostics
      in

      if isRedefinitionError then
        assert (EnvTL.hasErrors tlenv)
      else begin
        match List.rev paramImpl with
          | [] ->
            EnvTL.emitError tlenv $ Serror.fromExpr expr "expected body"
          | impl :: args ->
            begin
              let args = List.rev args in
              let argNames, isVariadic =
                let rec worker accum = function
                  | [] ->
                    accum, false
                  | [{ id = id; args = [{ id = name; args = [] }] }] when id = macroRest ->
                    (name :: accum), true
                  | { id = name; args = [] } :: rem ->
                    worker (name :: accum) rem
                  | (_ as arg) :: _ ->
                    raiseIllegalExpression arg "only identifiers allowed as macro parameter"
                in
                let reversed, isVariadic = worker [] args in
                List.rev reversed, isVariadic
              in
              let docstring =
                Common.combine " " argNames ^ if isVariadic then "..." else ""
              in
              let location = someOrDefault expr.location Basics.fakeLocation in
              let replace = fun bindings expr -> Ast2.replaceParams argNames expr.args impl in
              EnvTL.setBindings tlenv (Bindings.addMacro (EnvTL.bindings tlenv) name docstring location replace)
            end
      end
    | _ ->
      EnvTL.emitError tlenv (Serror.fromExpr expr (sprintf "invalid args"))

let translateSeqTL env expr =
  EnvTL.emitExprs env expr.args

module Link_clib : sig
  val translateLinkCLib : EnvTL.t -> Ast2.t -> unit
  val addDllPath : EnvTL.t -> string -> [`Front | `Back] -> unit
end = struct
  let translateLinkCLib dllPath (env : EnvTL.t) expr : unit =
    match expr with
      | { args = [{id = fileName; args = []; location}] } ->
        begin
          let location = someOrDefault location $ someOrDefault expr.location Basics.fakeLocation in
          let fileName = Common.removeQuotes fileName in

          let dllExtensions = ["dylib"; "so"; "dll"] in
          let matches re string = Str.string_match (Str.regexp re) string 0 in
          let dllPattern = sprintf ".*\\.\\(%s\\)" (Common.combine "\\|" dllExtensions) in
          if not (matches dllPattern fileName) then
            EnvTL.emitError env $ Serror.fromMsg (Some location)
              (sprintf "%s has invalid extension for a dynamic library. Supported: %s"
                 fileName (Common.combine ", " dllExtensions))
          else
            match Common.findFileIn fileName !dllPath with
              | Some absoluteFileName ->
                let handle = Machine.zompLoadLib absoluteFileName in
                if Zompvm.isNullPtr handle then
                  EnvTL.emitError env $ Serror.fromMsg (Some location)
                    (sprintf "could not load C library '%s'\n" absoluteFileName)
                else
                  ()
              | None ->
                EnvTL.emitError env $ Serror.fromMsg (Some location)
                  (formatFileNotFoundInPathError fileName !dllPath)
        end
      | invalidExpr ->
        EnvTL.emitError env $ Serror.fromExpr invalidExpr
          (sprintf "expecting '%s fileName'" invalidExpr.Ast2.id)

  let dllPath = ref ([] :string list)
  let addDllPath (env :EnvTL.t) dir where = addToList dllPath dir where

  let translateLinkCLib env expr =
    collectTimingInfo "translateLinkCLib" (fun () -> translateLinkCLib dllPath env expr)
end

module Include : sig
  val translateInclude : EnvTL.t -> Ast2.t -> unit
  val addIncludePath : EnvTL.t -> string -> [< `Back | `Front ] -> unit
end = struct
  let totalIncludeTime = ref 0.0
  let libsSection = Statistics.createSection "compiling included libraries (s)"
  let () = Statistics.createFloatCounter libsSection "total" 3 (Ref.getter totalIncludeTime)

  let translateInclude includePath (env : EnvTL.t) expr : unit =
    let importFile fileName =
      let source =
        collectTimingInfo "readFileContent"
          (fun () -> Common.readFile ~paths:!includePath fileName)
      in
      let previousTotalTime = !totalIncludeTime in
      let absoluteFileName = Common.absolutePath fileName in
      let result, time = recordTiming $ fun () ->
        match Parseutils.parseIExprs ~fileName:absoluteFileName source with
          | Parseutils.Error error ->
            EnvTL.emitError env error
          | Parseutils.Exprs exprs ->
            EnvTL.emitExprs env exprs
      in
      let nestedTime = time -. (!totalIncludeTime -. previousTotalTime) in
      Statistics.createFloatCounter libsSection absoluteFileName 3 (fun () -> nestedTime);
      totalIncludeTime := !totalIncludeTime +. nestedTime;
      result
    in
    match expr with
      | { id = id; args = [{ id = quotedFileName; args = []; location }] } when id = macroInclude ->
        begin
          let fileName =
            let fileName = Common.removeQuotes quotedFileName in
            if Common.endsWith ".zomp" fileName then fileName
            else fileName ^ ".zomp"
          in
          try
            importFile fileName
          with
            | Sys_error _ ->
              EnvTL.emitError env $ Serror.fromMsg location
                (formatFileNotFoundInPathError fileName !includePath)
            | error ->
              let msg = Printexc.to_string error in
              EnvTL.emitError env $ Serror.fromExpr expr
                (sprintf "%s, while compiling included file %s" msg fileName)
        end
      | invalidExpr ->
        EnvTL.emitError env $ Serror.fromExpr invalidExpr "expecting 'include \"fileName.zomp\"'"

  let includePath = ref ([] : string list)
  let addIncludePath (env :EnvTL.t) dir where = addToList includePath dir where

  let translateInclude env expr =
    collectTimingInfo "translateInclude"
      (fun () -> translateInclude includePath env expr)
end

let extractResultFromEnv env =
  let flag, errors, forms = EnvTL.reset env in
  let flag = if !compilationSwallowedErrors then Result.Fail else flag in
  compilationSwallowedErrors := false;
  Result.make flag ~diagnostics:errors ~results:forms

let lookupTLInstruction, addTranslateFunction, foreachToplevelBaseInstructionDoc =
  let handlers = ref [] in
  let add name ~doc instruction =
    handlers := (name, (doc, instruction)) :: !handlers
  in
  add macroFunc "typeExpr, name(typeExpr id, ...) expr" translateFunc;
  add macroTypedef "name, typeExpr" translateTypedef;
  add macroReplacement "" translateDefineReplacementMacro;
  add macroError "string, expr?" translateError;
  add macroLinkCLib "dll-name" Link_clib.translateLinkCLib;
  add macroVar "type, name, value" translateGlobalVar;
  add macroApply "expr..." translateApplyTL;
  add macroMacro "name, args..., body" translateDefineTLMacro;
  add "include" "zompSourceFile" Include.translateInclude;
  add "seq" "ast..." translateSeqTL;

  let lookup name =
    try
      let _, handler = List.assoc name (!handlers) in
      Some handler
    with Not_found ->
      None
  in
  let foreachDoc (f : string -> string -> unit) =
    List.iter (fun (name, (doc, _)) -> f name doc) !handlers
  in
  lookup, add, foreachDoc

let translate env expr =
  EnvTL.emitExpr env expr;
  extractResultFromEnv env

let translateMulti env exprs =
  EnvTL.emitExprs env exprs;
  extractResultFromEnv env

let addDllPath = Link_clib.addDllPath
let recommendedDllPath =
  ["."; ".."; "./libs"; "./tools/external/lib"]
let addIncludePath = Include.addIncludePath
let recommendedIncludePath =
  [Sys.getcwd(); Filename.dirname $ Sys.argv.(0)]

let emitBackendCodeFunc = ref (None : (string -> unit) option)
let setEmitbackendCode f = emitBackendCodeFunc := Some f
let emitBackendCode code =
  match !emitBackendCodeFunc with
    | Some f ->
      f code
    | None ->
      failwith "did not call Expander.setEmitbackendCode"

let emitBackendCodeForForm form =
  Zompvm.flushStreams();
  begin match !traceToplevelFrom with
    | Some f -> f form | None -> ();
  end;
  let llvmCode = Genllvm.gencodeTL form in
  emitBackendCode llvmCode;
  Zompvm.evalLLVMCode [form] llvmCode

type tlenv = EnvTL.t
let createEnv = EnvTL.create ~emitBackendCodeForForm ~lookupTLInstruction
let bindings = EnvTL.bindings
let setBindings = EnvTL.setBindings
let emitError = EnvTL.emitError
let emitExpr = EnvTL.emitExpr

