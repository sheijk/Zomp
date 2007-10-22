(* #directory "../v3";; *)
(* #load "lexer2.cmo";; *)
(* #load "parser2.cmo";; *)
(* #load "lang.cmo";; *)
(* #load "ast2.cmo";; *)
(* #load "common.cmo";; *)
(* #load "bindings.cmo";; *)


open Lang
open Semantic
open Ast2
open Common
open Bindings
open Printf

let macroVar = "var"
and macroMutableVar = "mvar"
and macroFunc = "func"
and macroIfThenElse = "ifthenelse"
and macroLoop = "loop"
and macroAssign = "assign"
and macroSequence = "seq"
and macroTypedef = "type"
and macroRecord = "record"
and macroField = "field"
and macroPtr = "ptr"
and macroReturn = "ret"
and macroLabel = "label"
and macroBranch = "branch"
and macroMacro = "macro"
and macroFieldptr = "fieldptr"
and macroLoad = "load"
and macroStore = "store"
and macroNullptr = "nullptr"
and macroPtradd = "ptradd"
and macroMalloc = "malloc"
and macroGetaddr = "ptr"
  
exception IllegalExpression of sexpr * string
  
let raiseIllegalExpression expr msg = raise (IllegalExpression (expr, msg))
  
let raiseIllegalExpressionFromTypeError expr (msg, found, expected) =
  raiseIllegalExpression expr (sprintf "Expected type %s but found %s: %s"
                                 (typeName expected)
                                 (typeName found)
                                 msg )

    
let raiseInvalidType typeExpr = raise (Typesystems.Zomp.CouldNotParseType (Ast2.expression2string typeExpr))


(* TODO: use in translateDefineVar and translateGlobalVar + test *)        
let determineStorage typ =
  match typ with
    | `Pointer _ -> MemoryStorage
    | _ -> RegisterStorage
        

let rec translateType bindings typeExpr =
  let lookupType bindings name =
    try
      Some (Lang.parseType name)
    with
      | Typesystems.Zomp.CouldNotParseType _ ->
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


type formWithTLsEmbedded = [`ToplevelForm of toplevelExpr | form]

let formWithTLsEmbeddedToString = function
  | #Lang.form as form -> Lang.formToString form
  | `ToplevelForm tlform -> Lang.toplevelFormToString tlform
    
type exprTranslateF = bindings -> sexpr -> bindings * formWithTLsEmbedded list
      
let rec extractToplevelForms = function
  | [] -> [], []
  | head :: rem ->
      begin
        let remTL, remImpl = extractToplevelForms rem in
        match head with
          | `ToplevelForm #toplevelExpr as tlform -> tlform::remTL, remImpl
          | #Lang.form as implF -> remTL, implF::remImpl
      end

        
let translatelst (translateF :exprTranslateF) bindings expr =
  let rec worker bindings = function
    | [] -> bindings, []
    | expr :: tail ->
        let newBindings, sf = translateF bindings expr in
        let resultingBindings, sfuncs = worker newBindings tail in
        resultingBindings, (sf @ sfuncs)
  in
  worker bindings expr
    
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
              (Printf.sprintf "expected value of type %s" (Lang.typeName typ))
      end
    | `Record components -> begin
        raiseIllegalExpression expr "records not supported"
      end
    | _ -> raiseIllegalExpression expr "unsupported value expression"
        
let translateDefineVar (translateF :exprTranslateF) (bindings :bindings) expr =
  let transform id name typeExpr valueExpr =
    match translateType bindings typeExpr with
      | Some (#integralType as typ)
      | Some (`Pointer _ as typ) ->
          begin
            let _, simpleform = translateF bindings valueExpr in
            let value = defaultValue typ in
            let storage = MemoryStorage
            in
            let var = variable name typ value storage false in
            let toplevelForms, implForms = extractToplevelForms simpleform in
            let defvar = `DefineVariable (var, Some (`Sequence implForms))
            in
            match typeCheck bindings defvar with
              | TypeOf _ -> Some( addVar bindings var, toplevelForms @ [defvar] )
              | TypeError (m,f,e) -> raiseIllegalExpressionFromTypeError expr (m,f,e)
          end
      | Some (`Record _ as typ) ->
          begin
            let var = variable name typ (defaultValue typ) MemoryStorage false in
            Some( addVar bindings var, [ `DefineVariable (var, None) ] )
          end
      | _ ->
          raise (CouldNotParseType (Ast2.expression2string typeExpr))
  in
  match expr with
    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
          valueExpr
        ] }
        when (id = macroVar) || (id = macroMutableVar) ->
        transform id name typeExpr valueExpr
    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
        ] }
        when (id = macroVar) || (id = macroMutableVar) ->
        transform id name typeExpr { id = ""; args = []}
    | _ ->
        None
          
let expr2VarOrConst (bindings :bindings) = function
  | { id = name; args = [] } -> begin
      match lookup bindings name with
        | VarSymbol v ->
            Some (`Variable v)
        | _ ->
            match string2integralValue name with
              | Some c -> Some (`Constant c)
              | None -> None
    end
  | _ -> None

      
let lastTempVarNum = ref 0

let getNewVar bindings typ =
  let newVarName =
    let rec tryTempVar num =
      let name = (Printf.sprintf "tempvar_%d" num) in
      match lookup bindings name with
        | UndefinedSymbol -> name
        | _ -> tryTempVar (num + 1)
    in
    incr lastTempVarNum;
    tryTempVar !lastTempVarNum
  in
  let value = defaultValue typ in
  let var = globalVar newVarName typ value in
  (addVar bindings var, var)
      
let translateSimpleExpr (_ :exprTranslateF) (bindings :bindings) expr =
  match expr2VarOrConst bindings expr with
    | Some varOrConst ->
        begin match varOrConst with
          | `Constant StringLiteral string ->
              begin
                let newBindings, var = getNewVar bindings (`Pointer `Char) in
                let var = { var with default = StringLiteral string } in
                Some( newBindings, [`ToplevelForm (`GlobalVar var); `Variable var] )
              end
          | _ -> Some (bindings, [varOrConst] )
        end
    | None -> None
          

let translateToForms (translateF :exprTranslateF) bindings expr =
  let newBindings, xforms = translateF bindings expr in
  let tlforms, forms = extractToplevelForms xforms in
  newBindings, toSingleForm forms, tlforms
  
let rec flattenLeft = function
  | [] -> [], []
  | (list, x) :: rem ->
      let remList, remX = flattenLeft rem in
      (list @ remList, x :: remX)
      
let translateFuncCall (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = name; args = args; } as expr ->
      match lookup bindings name with
        | FuncSymbol func ->
            begin
              let evalArg (argExpr :Ast2.sexpr) =
                let _, xforms = translateF bindings argExpr in
                let toplevelForms, forms = extractToplevelForms xforms in
                toplevelForms, toSingleForm forms
              in
              let x = List.map evalArg args in
              let toplevelForms, argForms = flattenLeft x in
              let funccall = `FuncCall {
                fcname = name;
                fcrettype = func.rettype;
                fcparams = List.map snd func.fargs;
                fcargs = argForms;
              }
              in
              match typeCheck bindings funccall with
                | TypeOf _ -> Some( bindings, toplevelForms @ [funccall] )
                | TypeError (msg, _, _) -> raiseIllegalExpression expr ("Type error: " ^ msg)
            end
        | _ -> None
            
let translateMacro translateF (bindings :bindings) = function
  | { id = macroName; args = args; } as expr ->
      match lookup bindings macroName with
        | MacroSymbol macro ->
            begin try
              let transformedExpr = macro.mtransformFunc args in
              Some (translateF bindings transformedExpr)
            with
              | Failure msg ->
                  raiseIllegalExpression expr ("Could not expand macro: " ^ msg)
            end
        | _ -> None
            
let translateDefineMacro translateF (bindings :bindings) = function
  | { id = id; args =
        { id = name; args = [] }
        :: paramImpl
    } when id = macroMacro ->
      begin match List.rev paramImpl with
        | [] -> None
        | impl :: args ->
            begin
              let args = List.rev args in
              let argNames = List.map
                (function
                   | { id = name; args = [] } -> name
                   | _ as arg -> raiseIllegalExpression arg "only identifiers allowed as macro parameter")
                args
              in
              Some( Bindings.addMacro bindings name (fun args -> Ast2.replaceParams argNames args impl), [] )
            end
      end
  | _ ->
      None

let translateReturn (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [expr] } when id = macroReturn ->
      begin
        let _, form, toplevelExprs = translateToForms translateF bindings expr in
        Some( bindings, toplevelExprs @ [`Return form] )
      end
  | _ -> None
      
let translateAssignVar (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [
        { id = varName; args = [] };
        rightHandExpr;
      ] } when id = macroAssign -> begin
      let _, rightHandForm, toplevelForms = translateToForms translateF bindings rightHandExpr in      
      match lookup bindings varName with
        | VarSymbol v ->
            Some (bindings, toplevelForms @ [`AssignVar (v, rightHandForm)] )
        | _ -> None
    end
  | _ -> None

let translateTypedef translateF (bindings :bindings) = function
  | { id = id; args = [
        { id = newTypeName; args = [] };
        targetTypeExpr;
      ] }
      when id = macroTypedef ->
      (** type foo typeExpr *)
      begin
        match translateType bindings targetTypeExpr with
          | None -> raiseInvalidType targetTypeExpr
          | Some t -> Some (addTypedef bindings newTypeName t, [`Typedef (newTypeName, t)] )
      end
  | { id = id; args =
        { id = typeName; args = [] }
        :: componentExprs
    } as expr
      when id = macroTypedef ->
      (** record typedef *)
      begin
        let tempBindings = Bindings.addTypedef bindings typeName (`TypeRef typeName) in
        let expr2component =
          let translate name typeExpr = 
            match translateType tempBindings typeExpr with
              | Some typ -> name, typ
              | None -> raise (CouldNotParseType typeName)
          in
          function
            | { id = typeName; args = [{ id = componentName; args = []}] } ->
                translate componentName { id = typeName; args = [] }
            | { id = seq; args = [typeExpr; { id = componentName; args = [] }] } when seq = macroSequence ->
                translate componentName typeExpr
            | _ -> raiseIllegalExpression expr "(type typeName (typeExpression componentName)* ) expected"
        in
        let components = List.map expr2component componentExprs in
        let recordType = `Record components in
        Some (addTypedef bindings typeName recordType, [`Typedef (typeName, recordType)] )
      end
  | _ -> None

let translateRecord (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args =
        { id = name; args = []; }
        :: componentExprs
    } as expr
      when id = macroRecord ->
      begin
        match lookup bindings name with
          | TypedefSymbol `Record components ->
              begin
                let recordInitFunc name components =
                  { fname = name ^ "_init";
                    rettype = `Void;
                    fargs = components;
                    impl = None;
                  }
                in
                let initFunc = recordInitFunc name components in
                let translate param compExpr = match (param, compExpr) with
                  | ((argName, argType), ({ id = compName; args = [argExpr] }) ) ->
                      begin
                        if argName = compName then begin
                          let _, form, toplevelForms = translateToForms translateF bindings argExpr in
                          if List.length toplevelForms > 0 then
                            raiseIllegalExpression expr "No nested toplevel forms allowed inside record";
                          form
                        end else
                          raiseIllegalExpression compExpr (sprintf "expected %s as id" argName)
                      end
                  | _, invalidCompExpr ->
                      raiseIllegalExpression invalidCompExpr "expected (componentName expr)"
                in
                let call = {
                  fcname = initFunc.fname;
                  fcrettype = initFunc.rettype;
                  fcparams = List.map snd initFunc.fargs;
                  fcargs = List.map2 translate initFunc.fargs componentExprs;
                } in
                Some( bindings, [`FuncCall call] )
              end
          | UndefinedSymbol -> raiseIllegalExpression expr (sprintf "%s is undefined" name)
          | _ -> raiseIllegalExpression expr (sprintf "%s is not a record" name)
      end
  | _ -> None

let translateGenericIntrinsic (translateF :exprTranslateF) (bindings :bindings) expr =
  let convertSimple typeExpr constructF =
    match translateType bindings typeExpr with
      | Some typ -> Some (bindings, [constructF typ] )
      | None -> None
  in
  let buildStoreInstruction ptrExpr rightHandExpr =
    let _, ptrForm, toplevelForms = translateToForms translateF bindings ptrExpr in
    let _, rightHandForm, toplevelForms2 = translateToForms translateF bindings rightHandExpr in
    let storeInstruction = `StoreIntrinsic (ptrForm, rightHandForm) in
    match typeCheck bindings storeInstruction with
      | TypeOf _ ->
          Some( bindings, toplevelForms @ toplevelForms2 @ [storeInstruction] )
      | TypeError (m,f,e) ->
          raiseIllegalExpressionFromTypeError rightHandExpr (m,f,e)
  in
  let buildMallocInstruction typeExpr countExpr =
    let typ =
      match translateType bindings typeExpr with
        | Some t -> t
        | None -> raiseIllegalExpression typeExpr "Could not translate type"
    in
    let _, rightHandForm, toplevelForms = translateToForms translateF bindings countExpr in
    let mallocForm = `MallocIntrinsic (typ, rightHandForm) in
    match typeCheck bindings mallocForm with
      | TypeOf _ -> Some( bindings, toplevelForms @ [mallocForm] )
      | TypeError (m,f,e) ->
          raiseIllegalExpressionFromTypeError expr (m,f,e)
  in
  let buildLoadInstruction ptrExpr =
    let _, ptrForm, toplevelForms = translateToForms translateF bindings ptrExpr in
    let loadForm = `LoadIntrinsic ptrForm in
    match typeCheck bindings loadForm with
      | TypeOf _ -> Some( bindings, toplevelForms @ [loadForm] )
      | TypeError (m,f,e) ->
          raiseIllegalExpressionFromTypeError expr (m,f,e)
  in
  match expr with
    | { id = id; args = [typeExpr] } when id = macroNullptr ->
        convertSimple typeExpr (fun t -> `NullptrIntrinsic t)
    | { id = id; args = [typeExpr] } when id = macroMalloc ->
        buildMallocInstruction typeExpr { id = "1"; args = [] }
    | { id = id; args = [typeExpr; countExpr] } when id = macroMalloc ->
        buildMallocInstruction typeExpr countExpr
    | { id = id; args = [{ id = varName; args = [] }] } when id = macroGetaddr ->
        begin
          match lookup bindings varName with
            | VarSymbol var -> Some (bindings, [`GetAddrIntrinsic var] )
            | _ -> raiseIllegalExpression expr (sprintf "Could not find variable %s" varName)
        end
    | { id = id; args = [ptrExpr; rightHandExpr] } when id = macroStore ->
        begin
          buildStoreInstruction ptrExpr rightHandExpr
        end
    | { id = id; args = [ ptrExpr ] } when id = macroLoad ->
        begin
          buildLoadInstruction ptrExpr
        end
    | { id = id; args = [ptrExpr; indexExpr] } when id = macroPtradd ->
        begin
          let _, ptrForm, toplevelForms = translateToForms translateF bindings ptrExpr in
          let _, indexForm, toplevelForms2 = translateToForms translateF bindings indexExpr in
          let ptradd = `PtrAddIntrinsic (ptrForm, indexForm) in
          match typeCheck bindings ptradd with
            | TypeOf _ -> Some( bindings, toplevelForms @ toplevelForms2 @ [ptradd] )
            | TypeError (m,f,e) ->
                raiseIllegalExpressionFromTypeError expr (m,f,e)
        end
    | { id = id; args = [
          recordExpr;
          { id = fieldName; args = [ ] };
        ] } when id = macroFieldptr ->
        begin
          let _, recordForm, toplevelForms = translateToForms translateF bindings recordExpr in
          let fieldptr = `GetFieldPointerIntrinsic (recordForm, fieldName) in
          match typeCheck bindings fieldptr with
            | TypeOf _ -> Some( bindings, toplevelForms @ [fieldptr] )
            | TypeError (m,f,e) ->
                raiseIllegalExpressionFromTypeError expr (m,f,e)
        end
    | _ ->
        None

let translateLabel (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [{ id = name; args = [] }] } when id = macroLabel ->
      begin
        Some( addLabel bindings name, [`Label { lname = name; }] )
      end
  | _ -> None

let translateBranch (translateF :exprTranslateF) (bindings :bindings) = function
  | { id = id; args = [{ id = labelName; args = [] }] } when id = macroBranch ->
      begin
        Some( bindings, [`Jump { lname = labelName; }] )
      end
  | { id = id; args = [
        { id = condVarName; args = [] };
        { id = trueLabelName; args = [] };
        { id = falseLabelName; args = [] };
      ] } as expr when id = macroBranch ->
      begin
        match lookup bindings condVarName with
          | VarSymbol var when var.typ = `Bool ->
              begin
                Some( bindings, [`Branch {
                                   bcondition = { var with typ = `Bool };
                                   trueLabel = { lname = trueLabelName};
                                   falseLabel = { lname = falseLabelName}; }] )
              end
          | _ ->
              raiseIllegalExpression expr "Branch requires a bool variable as condition"
      end
  | _ -> None

      
type toplevelExprTranslateF = bindings -> sexpr -> bindings * toplevelExpr list

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
                | (#integralType as typ) | (`Pointer `Char as typ) ->
                    let value = parseValue typ valueString in
                    let var = globalVar name typ value in
                    let newBindings = addVar bindings var in
                    Some( newBindings, [ `GlobalVar var ] )
                | `Pointer targetType ->
                    if valueString = "null" then
                      let var = globalVar name (`Pointer targetType) (PointerVal (targetType, None)) in
                      let newBindings = addVar bindings var in
                      Some( newBindings, [`GlobalVar var] )
                    else
                      raiseIllegalExpression expr "only null values supported for pointers currently"
                | _ -> raiseIllegalExpression expr
                    "only integral types legal for variables at this time"
            end
          | None -> raiseInvalidType typeExpr
      end
  | _ ->
      None

(* let uniqueIdCounter = ref 0 *)

(* let uniqueIdF = *)
(*   function *)
(*       (\*     | [] -> { id = sprintf "\"__id_%d\"" !uniqueIdCounter; args = [] } *\) *)
(*     | [] -> *)
(*         incr uniqueIdCounter; *)
(*         { id = string_of_int !uniqueIdCounter; args = [] } *)
(*     | _ as args -> raiseIllegalExpression { id = "uniqueIdF"; args = args } "Macro expects 0 params" *)

(* let compileTimeBindings = ref *)
(*   begin *)
(*     Bindings.addMacro Genllvm.defaultBindings "uniqueId" uniqueIdF *)
(*  end *)
(* (\* TODO: let uniqueId return a string and make it work *\) *)
(* let translateAntiquote (translateF :exprTranslateF) (bindings :bindings) = *)
(*   function *)
(*     | { id = "antiquote"; args = args } as expr -> *)
(*         begin *)
(*           let exprFromArgs = function *)
(*             | { id = id; args = [] } :: moreArgs -> Some { id = id; args = moreArgs } *)
(*             | _ -> None *)
(*           in *)
(*           let ctexpr = *)
(*             match exprFromArgs args with *)
(*               | Some ctexpr -> ctexpr *)
(*               | None -> raiseIllegalExpression expr "First argument of antiquoted expr must not have arguments" *)
(*           in *)
(*           eprintf "Translating sexpr %s\n" (Ast2.expression2string ctexpr); *)
(*           let tracingTranslateF bindings sexpr = *)
(*             eprintf "Translating %s\n" (Ast2.expression2string sexpr); *)
(*             translateF bindings sexpr *)
(*           in *)
(*           let newctBindings, ctforms = Printexc.print (fun () -> tracingTranslateF !compileTimeBindings ctexpr) () in *)
(*           let lastOfList list = *)
(*             let rec worker prev = function *)
(*               | [] -> prev *)
(*               | h :: t -> *)
(*                   worker h t *)
(*             in *)
(*             worker (List.hd list) list *)
(*           in *)
(*           match lastOfList ctforms with *)
(*             | `Constant c -> *)
(*                 let sexpr = { id = valueString c; args = [] } in *)
(*                 let newBindings, simpleforms = translateF bindings sexpr in *)
(*                 Some( newBindings, simpleforms ) *)
(*             | `Variable var -> *)
(*                 let sexpr = { id = var.vname; args = [] } in *)
(*                 eprintf "Ok2\n"; *)
(*                 let newBindings, simpleforms = translateF bindings sexpr in *)
(*                 Some( newBindings, simpleforms ) *)
(*             | _ -> *)
(*                 let illformStrings = List.map formWithTLsEmbeddedToString ctforms in *)
(*                 let illformCombined = Common.combine "\n" illformStrings in *)
(*                 raiseIllegalExpression ctexpr (sprintf "Resulted in invalid form: %s" illformCombined) *)
(*         end *)
(*     | _ -> *)
(*         None *)

let translateNested = translate raiseIllegalExpression
  [
    translateSeq;
    translateDefineVar;
    translateSimpleExpr;
    translateFuncCall;
    translateMacro;
    translateDefineMacro;
    translateAssignVar;
    (*     translateTypedef; *)
    translateGenericIntrinsic;
    translateRecord;
    translateReturn;
    translateLabel;
    translateBranch;
(*     translateAntiquote; *)
  ]

let rec translateFunc (translateF : toplevelExprTranslateF) (bindings :bindings) expr =
  let buildFunction bindings typ name paramExprs implExprOption =
    let expr2param argExpr =
      let translate varName typeExpr =
        match translateType bindings typeExpr with
          | Some typ -> (varName, typ)
          | None -> raiseInvalidType typeExpr
      in
      match argExpr with
        | { id = "seq"; args = [typeExpr; { id = varName; args = []};] } ->
            translate varName typeExpr
        | { id = typeName; args = [{ id = varName; args = [] }] } ->
            translate varName { id = typeName; args = [] }
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
    let nestedTLForms, impl = match implExprOption with
      | Some implExpr ->
          let nestedForms = snd (translateNested innerBindings implExpr) in
          let nestedTLForms, implForms = extractToplevelForms nestedForms in
          let nestedTLForms = List.map (fun (`ToplevelForm f) -> f) nestedTLForms in
(*           if nestedTLForms <> [] then failwith "nested tl forms aren't empty"; *)
(*           if implForms <> nestedForms then failwith "implForms <> nestedForms"; *)
          nestedTLForms, Some (`Sequence implForms)
      | None -> [], None
    in
    let f = func name typ params impl in
    let newBindings = addFunc bindings f in
    let funcDef = `DefineFunc f in
    newBindings, nestedTLForms, funcDef
  in
  match expr with
    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
          { id = seq1; args = paramExprs };
          { id = seq2; args = _ } as implExpr;
        ] } when id = macroFunc && seq1 = macroSequence && seq2 = macroSequence ->
        begin
          match translateType bindings typeExpr with
            | Some typ -> begin
                let tempBindings, _, _ = buildFunction bindings typ name paramExprs None in
                let newBindings, toplevelForms, funcDef =
                  buildFunction tempBindings typ name paramExprs (Some implExpr)
                in
                match typeCheckTL newBindings funcDef with
                  | TypeOf _ -> Some( newBindings, toplevelForms @ [funcDef] )
                  | TypeError (msg, declaredType, returnedType) ->
                      raiseIllegalExpression
                        expr
                        (Printf.sprintf "Function has return type %s but returns %s: %s"
                           (Lang.typeName declaredType)
                           (Lang.typeName returnedType)
                           msg)
              end
            | None -> raiseInvalidType typeExpr
        end
    | { id = id; args = [
          typeExpr;
          { id = name; args = [] };
          { id = seq; args = paramExprs };
        ] } when id = macroFunc && seq = macroSequence ->
        begin
          match translateType bindings typeExpr with
            | Some typ ->
                begin
                  let newBindings, _, funcDecl = buildFunction bindings typ name paramExprs None in
                  Some (newBindings, [funcDecl] )
                end
            | None ->
                raiseInvalidType typeExpr
        end
    | _ ->
        None
          
and translateTL bindings expr = translate raiseIllegalExpression
  [
    translateGlobalVar;
    translateFunc;
    translateTypedef;
    translateDefineMacro;
    translateMacro;
  ]
  bindings expr

let makeNested (translateF :toplevelExprTranslateF) (_ :exprTranslateF) (bindings :bindings) expr =
  let _, toplevelForms = translateF bindings expr in
  let nestedForms = List.map (fun f -> `ToplevelForm f) toplevelForms in
  Some( bindings, nestedForms )


(*                     | `Sequence expressionns -> *)
(*                         let newtls, seqImpls = toplevelAndImpl expressionns in *)
(*                         newtls @ remTL, (`Sequence seqImpls)::remImpl *)
(*                     | `DefineVariable (var, rightHandForm) -> *)
(*                         begin match rightHandForm with *)
(*                           | Some rightHandForm -> *)
(*                               begin *)
(*                                 let newtls, rightHand = toplevelAndImpl rightHandForm in *)
(*                                 newtls @ remTL, (`DefineVariable(var,rightHand))::remImpl *)
(*                               end *)
(*                           | None -> remTL, head::remImpl *)
(*                         end *)
(*                     | `FuncCall funcCall -> *)
(*                         begin *)
(*                           let newtlsList, argImpls = List.map toplevelAndImpl funcCall.fcargs in *)
(*                           let newtls = List.flatten newtlsList in *)
(*                           newtls @ remTL, (`FuncCall {funcCall with args = argImpls})::remImpl *)
(*                         end *)
(*                     | `AssignVar (var, rightHandForm) -> *)
(*                         let newtls, rightHand = toplevelAndImpl rightHandForm in *)
(*                         newtls @ remTL, (`AssignVar rightHand)::remImpl *)
(*                     | `Return form -> *)
(*                         let newtls, newForm = toplevelAndImpl form in *)
(*                         newtls @ remTL, (`Return newForm)::remImpl *)
(*                     | `MallocIntrinsic (typ, countForm) -> *)
(*                         let newtls, newCountForm =  in *)
(*                     | `GetAddrIntrinsic of composedType variable *)
(*                     | `StoreIntrinsic of 'expr * 'expr *)
(*                     | `LoadIntrinsic of 'expr *)
(*                     | `PtrAddIntrinsic of 'expr * 'expr (\* pointer, int *\) *)
(*                     | `GetFieldPointerIntrinsic of 'expr * string *)
(*                 end *)
