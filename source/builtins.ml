
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
  fun loc id args ->
    let default = Ast2.exprLoc loc id args in
    match args with
      | [] ->
        begin match lookup bindings id with
          | VarSymbol { typ = `Int32 } ->
            Ast2.exprLoc loc "ast:fromInt" [idExprLoc loc id]
          | VarSymbol { typ = `Float } ->
            Ast2.exprLoc loc "ast:fromFloat" [idExprLoc loc id]
          | VarSymbol { typ = `Pointer `Char } ->
            Ast2.exprLoc loc "ast:fromString" [idExprLoc loc id]
          | _ ->
            default
        end
      | _ -> default

let sexpr2codeNoAntiquotes recursion expr =
  let astFromString id locationOpt =
    match locationOpt with
      | Some loc ->
        simpleExprLoc loc "ast:fromStringLoc"
          [id;
           "\"" ^ loc.Basics.fileName ^ "\"";
           sprintf "%d" loc.Basics.line;
           sprintf "%d" (someOrDefault loc.Basics.column 0)]
      | None ->
        simpleExprLoc Basics.fakeLocation "ast:fromString" [id]
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
      let loc = Ast2.location expr in
      let defVarExpr =
        Ast2.exprLoc
          loc
          "var"
          [simpleExprLoc loc "ptr" ["ast"];
            idExprLoc loc tempVarName;
            astFromString ("\"" ^ sexprWithArgs.id ^ "\"") expr.location]
      in
      let returnExpr = idExprLoc loc tempVarName in
      let addChildExpr = function
        | `Insert childExpr ->
           Ast2.exprLoc
             Basics.fakeLocation
             "ast:addChild"
             [idExprLoc loc tempVarName;
              childExpr;]
        | `Splice childExpr ->
           Ast2.exprLoc
             Basics.fakeLocation
             "ast:addAllChilds"
             [idExprLoc loc tempVarName;
              childExpr]
      in
      let argExprs = List.map recursion sexprWithArgs.args in
      let argAddExprs = List.map addChildExpr argExprs in
      seqExprLoc loc ([defVarExpr] @ argAddExprs @ [returnExpr])

let rec sexpr2codeasis expr =
  sexpr2codeNoAntiquotes (fun e -> `Insert (sexpr2codeasis e)) expr

let rec sexpr2code ?(antiquoteF = Ast2.exprLoc) expr =
  let antiQuote expr = antiquoteF (Ast2.location expr) expr.id expr.args in
  let recurse expr =
    match expr with
      | { id = "splicingantiquote"; args = [arg] } ->
         (`Splice (antiQuote arg))
      | { id = "splicingantiquote"; } ->
         failwith "splicingantiquote expects only one argument"
      | _ ->
         `Insert (sexpr2code ~antiquoteF expr)
  in
  match expr with
    | { id = "antiquote"; args = [arg] } ->
       antiQuote arg
    | { id = "splicingantiquote"; args = [arg] } ->
       failwith "splicingantiquote not allowed as single argument to quote"
    | _ ->
       sexpr2codeNoAntiquotes recurse expr

let builtinMacros =
  let builtinMacros =
    let macro name doc f =
      (name, MacroSymbol (Lang.macro name doc Basics.builtinLocation f))
    in

    let quoteMacro =
      macro "quote"
        "ast..."
        (fun bindings expr ->
          let loc = Ast2.location expr in
          match expr.args with
            | [quotedExpr] -> sexpr2code ~antiquoteF:(insertAstConstructors bindings) quotedExpr
            | [] -> simpleExprLoc loc "ast:fromString" ["seq"]
            | args -> Ast2.exprLoc loc "quote" args)
    in
    let quoteasisMacro =
      macro "quoteasis" "ast..."
        (fun bindings expr ->
          let loc = Ast2.location expr in
          match expr.args with
            | [quotedExpr] -> sexpr2codeasis quotedExpr
            | [] -> simpleExprLoc loc "ast:fromString" ["seq"]
            | args -> Ast2.exprLoc loc "quote" args
        )
    in
    let bindingsIsNameUsed =
      macro "std:bindings:isNameUsed" "name"
        (fun bindings expr ->
          let loc = Ast2.location expr in
          match expr.args with
            | [{ id = name; args = []}] ->
              begin match Bindings.lookup bindings name with
                | Bindings.UndefinedSymbol -> idExprLoc loc "false"
                | _ -> idExprLoc loc "true"
              end
            | _ ->
              raiseCodeGenError ~msg:("std:bindings:isNameUsed expects exactly one argument")
        )
    in
    let bindingsLookupVar =
      let syntax = "name ('hasType' typeVar code ...) ('notFound' code ...)" in
      macro "std:bindings:matchVar" syntax
        (fun bindings expr ->
          let loc = Ast2.location expr in
          match expr.args with
            | [ {id = name; args = []};
                {id = "hasType"; args = {id = typeVar; args = []} :: onFound};
                {id = "notFound"; args = onNotFound}] ->
              begin match lookup bindings name with
                | VarSymbol var ->
                  replaceParams [typeVar] [idExprLoc loc (typeName var.typ)]
                    (seqExprLoc loc onFound)
                | _ ->
                  seqExprLoc loc onNotFound
              end
            | _ ->
              raiseCodeGenError ~msg:(
                sprintf "std:bindings:matchVar expects syntax %s" syntax))
    in
    let testMacro =
      let module NAst = Zompvm.NativeAst in
      let calls1i functionName ast =
        Zompvm.Call.reset();
        Zompvm.Call.addPointerArg (NAst.addr ast);
        Zompvm.Call.string functionName
      in
      let calli1i functionName ast =
        Zompvm.Call.reset();
        Zompvm.Call.addPointerArg (NAst.addr ast);
        Zompvm.Call.int functionName
      in
      macro "std:test" "()"
        (fun bindings args ->
          let loc = Ast2.location args in
          let sexprAddress = NAst.simple "foobar" in
          if NAst.isNull sexprAddress then begin
            NAst.addChild sexprAddress (NAst.simple "child");
            let name = calls1i "macroAstId" sexprAddress in
            let childCount = calli1i "macroAstChildCount" sexprAddress in
            Ast2.simpleExprLoc loc "was" [name; string_of_int childCount]
          end else begin
            Ast2.simpleExprLoc loc "returned NULL pointer" []
          end)
    in

    let opjuxMacro =
      macro "opjux" "opjux id args..."
        (fun bindings expr -> expr >>= Ast2.withId Lang.macroApply)
    in
    let opcallMacro =
      macro "opcall" "opcall id args..."
        (fun bindings expr -> expr >>= Ast2.withId Lang.macroApply)
    in
    let opseqMacro =
      macro "opseq" "opseq args..."
        (fun bindings expr -> Ast2.seqExprLoc (Ast2.location expr) expr.args)
    in

    let isInteractiveMacro =
      macro "std:vm:isInteractive" "bool()"
        (fun bindings expr ->
          Ast2.idExprLoc (Ast2.location expr) (if Zompvm.isInteractive() then "true" else "false"))
    in

    let setTraceMacroExpansionMacro =
      macro "std:compiler:setTraceMacroExpansion" "bool"
        (fun bindings expr ->
          let loc = Ast2.location expr in
          match expr.args with
            | [{ Ast2.id = "true" | "false" as newState; args = [] }] ->
              Zompvm.traceMacroExpansionOn := (newState = "true");
              printf "%s\n" (Basics.formatInfo (Ast2.locationOr expr Basics.fakeLocation)
                               (sprintf "set trace macro expansion to %s" newState));
              flush stdout;
              Ast2.emptyExprLoc loc
            | _ ->
              Ast2.idExprLoc loc "error")
    in
    let getTraceMacroExpansionMacro =
      macro "std:compiler:getTraceMacroExpansion" "bool"
        (fun bindings expr ->
          let loc = Ast2.location expr in
          match expr.args with
            | [] ->
              Ast2.idExprLoc loc (if !(Zompvm.traceMacroExpansionOn) then "true" else "false")
            | _ ->
              Ast2.idExprLoc loc "error")
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
    func name `Bool [Lang.funcParam "l" typ; Lang.funcParam "r" typ]
  in

  let binaryOp name instruction (typ :typ) =
    func name typ [Lang.funcParam "l" typ; Lang.funcParam "r" typ]
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
      func "float:toInt" `Int32 [Lang.funcParam "v" `Float];
      func "int:toFloat" `Float [Lang.funcParam "v" `Int32];
      func "int:toDouble" `Double [Lang.funcParam "v" `Int32];
      func "double:toInt" `Int32 [Lang.funcParam "v" `Double];
      func "float:toDouble" `Double [Lang.funcParam "v" `Float];
      func "double:toFloat" `Float [Lang.funcParam "v" `Double];

      (** deprecated *)
      func "u32:toChar" `Char [Lang.funcParam "v" `Int32];
      func "char:zextToU32" `Int32 [Lang.funcParam "v" `Char];

      (** deprecated *)
      func "u64:toU32" `Int32 [Lang.funcParam "v" `Int64];
      func "u32:zextToU64" `Int64 [Lang.funcParam "v" `Int32];
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


