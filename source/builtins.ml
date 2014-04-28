
open Ast2
open Lang
open Types
open Printf
open Bindings
open Common

type backendInfo = {
  sizeT :Types.typ;
}

(** TODO: raise proper exception *)
let raiseCodeGenError ~msg = failwith msg

let lastUniqueId = ref 0
let newUniqueId() =
  incr lastUniqueId;
  !lastUniqueId
let newUniqueName() =
  "__temp_builtins_" ^ string_of_int (newUniqueId())

(** will turn #foo into (astFromInt foo) if foo evaluates to `Int etc. *)
let insertAstConstructors bindings =
  fun id args ->
    let default = Ast2.expr id args in
    match args with
      | [] ->
        begin match lookup bindings id with
          | VarSymbol { typ = `Int32 } ->
            Ast2.expr "ast:fromInt" [idExpr id]
          | VarSymbol { typ = `Float } ->
            Ast2.expr "ast:fromFloat" [idExpr id]
          | VarSymbol { typ = `Pointer `Char } ->
            Ast2.expr "ast:fromString" [idExpr id]
          | _ ->
            default
        end
      | _ -> default

let sexpr2codeNoAntiquotes recursion expr =
  let astFromString id locationOpt =
    match locationOpt with
      | Some loc ->
        simpleExpr "ast:fromStringLoc"
          [id;
           "\"" ^ loc.Basics.fileName ^ "\"";
           sprintf "%d" loc.Basics.line;
           sprintf "%d" (someOrDefault loc.Basics.column 0)]
      | None ->
        simpleExpr "ast:fromString" [id]
  in
  match expr with
    | { id = id; args = [] } ->
    (* TODO: check why an ad-hoc work-around is needed here *)
      let fixedId =
        if id = "'\\0'" then "\"'!'\""
        else ("\"" ^ id ^ "\"")
      in
      astFromString fixedId expr.location
    | sexprWithArgs ->
      let tempVarName = newUniqueName() in
      let defVarExpr =
        { id = "var";
          args = [
            simpleExpr "ptr" ["ast"];
            idExpr tempVarName;
            astFromString ("\"" ^ sexprWithArgs.id ^ "\"") expr.location];
          location = expr.location }
      in
      let returnExpr = idExpr tempVarName in
      let addChildExpr childExpr =
        { id = "ast:addChild";
          args = [
            idExpr tempVarName;
            childExpr;];
          location = None }
      in
      let argExprs = List.map recursion sexprWithArgs.args in
      let argAddExprs = List.map addChildExpr argExprs in
      seqExpr( [defVarExpr] @ argAddExprs @ [returnExpr] )

let rec sexpr2codeasis expr = sexpr2codeNoAntiquotes sexpr2codeasis expr

let rec sexpr2code ?(antiquoteF = Ast2.expr) = function
  | { id = "antiquote"; args = [{ id = id; args = args}] } ->
    begin
      antiquoteF id args
    end
  | expr ->
    sexpr2codeNoAntiquotes (sexpr2code ~antiquoteF) expr

let builtinMacros =
  let builtinMacros =
    let macro name doc f =
      (name, MacroSymbol (Lang.macro name doc Basics.builtinLocation f))
    in

    let quoteMacro =
      macro "quote"
        "ast..."
        (fun bindings expr ->
          match expr.args with
            | [quotedExpr] -> sexpr2code ~antiquoteF:(insertAstConstructors bindings) quotedExpr
            | [] -> simpleExpr "ast:fromString" ["seq"]
            | args -> Ast2.expr "quote" args
        )
    in
    let quoteasisMacro =
      macro "quoteasis" "ast..."
        (fun bindings expr ->
          match expr.args with
            | [quotedExpr] -> sexpr2codeasis quotedExpr
            | [] -> simpleExpr "ast:fromString" ["seq"]
            | args -> Ast2.expr "quote" args
        )
    in
    let bindingsIsNameUsed =
      macro "std:bindings:isNameUsed" "name"
        (fun bindings expr ->
          match expr.args with
            | [{ id = name; args = []}] ->
              begin match Bindings.lookup bindings name with
                | Bindings.UndefinedSymbol -> idExpr "false"
                | _ -> idExpr "true"
              end
            | _ ->
              raiseCodeGenError ~msg:("std:bindings:isNameUsed expects exactly one argument")
        )
    in
    let bindingsLookupVar =
      let syntax = "name ('hasType' typeVar code ...) ('notFound' code ...)" in
      macro "std:bindings:matchVar" syntax
        (fun bindings expr ->
          match expr.args with
            | [ {id = name; args = []};
                {id = "hasType"; args = {id = typeVar; args = []} :: onFound};
                {id = "notFound"; args = onNotFound}] ->
              begin match lookup bindings name with
                | VarSymbol var ->
                  replaceParams [typeVar] [idExpr (typeName var.typ)]
                    (seqExpr onFound)
                | _ ->
                  seqExpr onNotFound
              end
            | _ ->
              raiseCodeGenError ~msg:(
                sprintf "std:bindings:matchVar expects syntax %s" syntax)
        )
    in
    let testMacro =
      let calls1i functionName arg =
        Machine.zompResetArgs();
        Machine.zompAddPointerArg arg;
        Machine.zompRunFunctionStringWithArgs functionName
      in
      let calli1i functionName arg =
        Machine.zompResetArgs();
        Machine.zompAddPointerArg arg;
        Machine.zompRunFunctionIntWithArgs functionName
      in
      macro "std:test" "()"
        (fun bindings args ->
          let sexprAddress = Machine.zompSimpleAst "foobar" in
          if Machine.zompAstIsNull sexprAddress then begin
            Machine.zompAddChild sexprAddress (Machine.zompSimpleAst "child");
            let name = calls1i "macroAstId" sexprAddress in
            let childCount = calli1i "macroAstChildCount" sexprAddress in
            Ast2.simpleExpr "was" [name; string_of_int childCount]
          end else begin
            Ast2.simpleExpr "returned NULL pointer" []
          end)
    in

    let opjuxMacro =
      macro "opjux" "opjux id args..."
        (fun bindings expr -> { expr with id = Lang.macroApply })
    in
    let opcallMacro =
      macro "opcall" "opcall id args..."
        (fun bindings expr -> { expr with id = Lang.macroApply })
    in
    let opseqMacro =
      macro "opseq" "opseq args..."
        (fun bindings expr -> Ast2.seqExpr expr.args)
    in

    let isInteractiveMacro =
      macro "std:vm:isInteractive" "bool()"
        (fun bindings expr ->
          Ast2.idExpr (if Zompvm.isInteractive() then "true" else "false"))
    in

    let setTraceMacroExpansionMacro =
      macro "std:compiler:setTraceMacroExpansion" "bool"
        (fun bindings expr ->
          match expr.args with
            | [{ Ast2.id = "true" | "false" as newState; args = [] }] ->
              Zompvm.traceMacroExpansionOn := (newState = "true");
              printf "%s\n" (Basics.formatInfo (Ast2.locationOr expr Basics.fakeLocation)
                               (sprintf "set trace macro expansion to %s" newState));
              flush stdout;
              Ast2.emptyExpr
            | _ ->
              Ast2.idExpr "error")
    in
    let getTraceMacroExpansionMacro =
      macro "std:compiler:getTraceMacroExpansion" "bool"
        (fun bindings expr ->
          match expr.args with
            | [] ->
              Ast2.idExpr (if !(Zompvm.traceMacroExpansionOn) then "true" else "false")
            | _ ->
              Ast2.idExpr "error")
    in

    [
      testMacro;
      quoteMacro;
      quoteasisMacro;
      bindingsIsNameUsed;
      bindingsLookupVar;

      isInteractiveMacro;

      setTraceMacroExpansionMacro;
      getTraceMacroExpansionMacro;

      (** macros to support indent expressions *)
      opjuxMacro;
      opcallMacro;
      opseqMacro;
    ]
  in

  builtinMacros

let builtinIntrinsics backend : Lang.func list =
  let func name typ args =
    funcDecl name typ args Basics.builtinLocation
  in

  let comparison typ name =
    func name `Bool ["l", typ; "r", typ]
  in

  let binaryOp name instruction (typ :typ) =
    func name typ ["l", typ; "r", typ]
  in

  let binaryOps typ namespace names =
    List.map (fun name -> binaryOp (sprintf "%s:%s" namespace name) name typ) names
  in

  let intCompareIntrinsics typ typeName =
    let comparisons =
      [
        "equal";
        "notEqual";
        "ugreater";
        "ugreaterEqual";
        "uless";
        "ulessEqual";
        "sgreater";
        "sgreaterEqual";
        "sless";
        "slessEqual";

        "greater";
        "greaterEqual";
        "less";
        "lessEqual";
      ]
    in
    List.map (fun zompName ->
      comparison typ (typeName ^ ":" ^ zompName))
      comparisons
  in

  let intIntrinsics ?name typ =
    let intBinaryOps =
      ["add"; "sub"; "mul"; "sdiv"; "udiv"; "urem"; "srem"; "and"; "or"; "xor"]
    in
    let name = someOrDefault name $ typeName typ in

    binaryOps typ name intBinaryOps
    @ intCompareIntrinsics typ name
  in

  let floatIntrinsics typ =
    let typeName = typeName typ in
    let comparisons = [
      "equal";
      "notEqual";
      "greater";
      "greaterEqual";
      "less";
      "lessEqual";
    ] in

    let ops = ["add"; "sub"; "mul"; "fdiv"; "frem"] in

    List.map (fun op -> comparison typ (sprintf "%s:o%s" typeName op)) comparisons
    @ List.map (fun op -> binaryOp (sprintf "%s:%s" typeName op) "" typ) ops
  (* TODO: rename fdiv, fmul, frem *)
  in

  let intrinsicFuncs =
    [
      binaryOp "u32:shl" "shl" `Int32;
      binaryOp "u32:lshr" "lshr" `Int32;
      binaryOp "u32:ashr" "ashr" `Int32;

      (** deprecated *)
      func "float:toInt" `Int32 ["v", `Float];
      func "int:toFloat" `Float ["v", `Int32];
      func "int:toDouble" `Double ["v", `Int32];
      func "double:toInt" `Int32 ["v", `Double];
      func "float:toDouble" `Double ["v", `Float];
      func "double:toFloat" `Float ["v", `Double];

      (** deprecated *)
      func "u32:toChar" `Char ["v", `Int32];
      func "char:zextToU32" `Int32 ["v", `Char];

      (** deprecated *)
      func "u64:toU32" `Int32 ["v", `Int64];
      func "u32:zextToU64" `Int64 ["v", `Int32];
    ]

    @ intIntrinsics `Int8
    @ intIntrinsics `Int16
    @ intIntrinsics `Int32
    @ intIntrinsics `Int64
    @ intIntrinsics backend.sizeT ~name:Types.sizeTName

    @ binaryOps `Bool "bool" ["and"; "or"; "xor"]

    @ floatIntrinsics `Float
    @ floatIntrinsics `Double

    @ intCompareIntrinsics `Char (typeName `Char)
  in
  intrinsicFuncs

let defaultBindings backend =
  Bindings.fromSymbolList
    ([Types.sizeTName, TypedefSymbol backend.sizeT]
     @ List.map (fun func -> func.fname, FuncSymbol func) (builtinIntrinsics backend)
     @ builtinMacros)


