open Printf
open Basics

type sexpr = {
  id :string;
  args :sexpr list;
  location :location option;
}

type t = sexpr

let withLoc ast loc = { ast with location = Some loc }

(** construction functions **)

let idExpr name = { id = name; args = []; location = None }
let idExprLoc location name = { id = name; args = []; location = Some location }
let simpleExpr name args = { id = name; args = List.map idExpr args; location = None }
let simpleExprLoc location name args =
  { id = name;
    args = List.map (idExprLoc location) args;
    location = Some location }
let emptyExpr = { id = "seq"; args = []; location = None }
let emptyExprLoc location = { id = "seq"; args = []; location = Some location }
let seqExpr args = { id = "seq"; args = args; location = None }
let seqExprLoc location args = { id = "seq"; args = args; location = Some location }
let opseqExpr args = { id = "opseq"; args = args; location = None }
let opseqExprLoc location args = { id = "opseq"; args = args; location = Some location }
let expr name args = { id = name; args = args; location = None }
let exprLoc location name args = { id = name; args = args; location = Some location }
let juxExpr args = { id = "opjux"; args = args; location = None }
let juxExprLoc location args = { id = "opjux"; args = args; location = Some location }
let callExpr args = { id = "opcall"; args = args; location = None }
let callExprLoc location args = { id = "opcall"; args = args; location = Some location }

(** construction w/ explicit loc handling (for parser) *)

let combineLocations exprs =
  match exprs with
    | [] -> None
    | first :: _ -> first.location

let juxExprInferLoc exprs =
  let jux = juxExpr exprs in
  { jux with location = combineLocations exprs }
let callExprInferLoc exprs =
  let e = callExpr exprs in
  { e with location = combineLocations exprs }
let seqExprInferLoc exprs =
  let e = opseqExpr exprs in
  { e with location = combineLocations exprs }

let exprInferLoc name exprs =
  let e = expr name exprs in
  { e with location = combineLocations exprs }

(** query functions **)

let fileName ast =
  match ast.location with
    | Some { fileName = fileName } -> fileName
    | None -> "?.zomp"

let lineNumber ast =
  match ast.location with
    | Some { line = line } -> line
    | None -> 0
  
(** transformations **)

let toSingleExpr = function
  | [single] -> single
  | multiOrNone -> seqExpr multiOrNone

type stringTree =
  | STLeaf of string
  | STBranch of stringTree list

let lastOutput = ref ""
let secondLastOutput = ref ""

let () =
  let printLocations = true in
  let lastLoc = ref fakeLocation in
  let makeLocationIndicator alwaysPrintLoc = function
    | Some { fileName = "" } -> "~"
    | None -> "!"
    | Some loc ->
      if printLocations && (alwaysPrintLoc || not (Basics.locationEqual loc !lastLoc)) then begin
        lastLoc := loc;
        sprintf " @%s" (Basics.locationToString loc);
      end else
        ""
  in

  let rec toStringTree expr =
    let leafString expr = sprintf "%s%s" expr.id (makeLocationIndicator false expr.location) in
    match expr with
      | { args = [] } ->
        STLeaf (leafString expr)
      (* | { id = "op>" | "op<" | "op=" | "op+" | "op-" | "op*" | "op/" ; args = [lhs; rhs] } -> *)
      (*   STBranch [toStringTree lhs; *)
      (*             STLeaf (leafString { expr with id = Str.string_after expr.id 2 }); *)
      (*             toStringTree rhs] *)
      | _ ->
        STBranch (STLeaf (leafString expr) :: List.map toStringTree expr.args)
  in

  let rec stToString ~maxLength ?(indent = 0) tree =
    let recurse = stToString ~maxLength ~indent in
    match tree with
      | STLeaf str -> str
      | STBranch childs ->
        let childStrings = List.map recurse childs in
        let lengths = List.map String.length childStrings in
        let sum = List.fold_left (+) 0 in
        let totalLength = sum lengths in
        if totalLength + List.length childStrings <= maxLength - 2 then
          String.concat ""
            ["("; String.concat " " childStrings; ")"]
        else
          let head = List.hd childStrings in
          let tail = List.tl childStrings in
          let indented = List.map Common.indent tail in
          String.concat ""
            ["("; head; "\n"; String.concat "\n" indented; ")"]
  in

  let testWithMaxLength maxLength =
    let seperatorString = "\n" ^ String.make maxLength '-' ^ "\n" in
    let loc line = { Basics.fileName = "testfile.zomp"; line } in
    let exprs =
      [idExprLoc (loc 0) "foo";
       idExpr "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
       simpleExpr "short" ["a"; "b"];
       simpleExpr "simple" ["arg0"; "arg1"; "arg2"; "arg3"];
       opseqExpr
         [simpleExpr "foo" ["bar"; "baz"; "buzz"; "long_argument_soup"];
          simpleExpr "abcd" ["0123"];
          idExpr "lalal"];
       simpleExpr "x0123456789" ["a"; "b"; "c"];
       expr "foobar" [
         callExpr [idExpr "f"; idExpr "x"];
         opseqExpr [simpleExpr "f2" ["a"; "b"; "c"]]];
       expr "if" [
         expr "op>" [idExpr "x"; simpleExpr "op*" ["count"; "10"]];
         idExpr "then";
         opseqExpr [
           simpleExpr "println" ["'hello'"];
           simpleExpr "op=" ["x"; "0"]];
         idExpr "else";
         opseqExpr [
           simpleExpr "opcall" ["abort"]]];
      ]
    in
    let strings = List.map (fun e -> stToString maxLength (toStringTree e)) exprs in
    Common.combine seperatorString strings
  in
  let runOutputs = List.map testWithMaxLength [10; 20] in
  let output = Common.combine "\n\n" runOutputs in
  printf "\ntesting\n%s\n" output;
  if output = !lastOutput then
    printf "Same result as last run\n"
  else begin
    printf "Output is different than last run\n";
    let fileOld = "/tmp/fileOld.txt"
    and fileNew = "/tmp/fileNew.txt"
    and fileDiffOutput = "/tmp/diffOutput.txt"
    in
    let writeToFile ~file string =
      Common.withFileForWriting file (fun out -> output_string out string)
    in
    writeToFile ~file:fileOld !lastOutput;
    writeToFile ~file:fileNew output;
    let runCommand cmd =
      match Sys.command cmd with
        | 0 -> ()
        | errorCode ->
          printf "returned %d\n" errorCode;
    in
    runCommand (sprintf "diff %s %s > %s" fileOld fileNew fileDiffOutput);
    printf "%s\n" (Common.readFile fileDiffOutput)
  end;
  secondLastOutput := !lastOutput;
  lastOutput := output

let rec makeString printLocations sexpr =
  let lastLoc = ref Basics.fakeLocation in
  let rec expression2string sexpr =
    let idString =
      if sexpr.id = "seq" && List.length sexpr.args = 0 then "()"
      else if String.length sexpr.id > 0 then sexpr.id
      else "/0/"
    in
    let (++) f g x = f (g x) in
    let rec classify sexprs =
      let longExpr params = `LongExpr params
      and noSeq params =
        let summedLengths = List.fold_left (fun len str -> len + String.length str) 0 in
        if summedLengths params > 60 then
          `LongExpr params
        else
          `NoSeq params
      and seqAtEnd seq params = `SeqAtEnd(params, seq)
      in
      let rec worker hadSequence acc = function
        | [] ->
          (if hadSequence then longExpr else noSeq), acc
        | [{ id = "seq"; args = seqArgs } as seq] when seqArgs <> [] ->
          (if hadSequence then longExpr, (seq::acc) else seqAtEnd seqArgs, acc)
        | { id = "seq" } as expr :: tail when expr.args <> [] ->
          worker true (expr :: acc) tail
        | hd :: tail ->
          let subExprHasSequence =
            match classify hd.args with
              | `NoSeq _ -> false
              | `SeqAtEnd(_,_) | `LongExpr _ -> true
          in
          worker (hadSequence or subExprHasSequence) (hd :: acc) tail
      in
      let f, params = worker false [] sexprs in
      let paramStrings = List.map expression2string params in
      f (List.rev paramStrings)
    in
    let inParens str = "(" ^ str ^ ")" in
    let simple2string idString paramStrings =
      Common.combine " " (idString :: paramStrings)
    in
    let makeLocationIndicator alwaysPrintLoc = function
      | Some { fileName = "" } -> "~"
      | None -> "!"
      | Some loc ->
        if printLocations && (alwaysPrintLoc || not (Basics.locationEqual loc !lastLoc)) then begin
          lastLoc := loc;
          sprintf " @%s" (Basics.locationToString loc);
        end else
          ""
    in
    let str, locationIndicator =
      match classify sexpr.args with
        | `NoSeq params ->
          begin match params with
            | [] -> idString
            | _ -> inParens (simple2string idString params)
          end,
            makeLocationIndicator false sexpr.location
        | `SeqAtEnd (params, seqArgs) ->
          inParens (
            simple2string idString params ^ " (\n"
            ^ Common.combine "\n" (List.map (Common.indent ++ expression2string) seqArgs) ^ " )"),
          makeLocationIndicator false sexpr.location
        | `LongExpr childs ->
          "(" ^ Common.combine "\n" ((idString ^ makeLocationIndicator true sexpr.location) :: List.map Common.indent childs) ^ " )",
          ""
    in
    str ^ locationIndicator
  in
  expression2string sexpr

let expression2string = makeString false
let toString = expression2string

let rec equals l r =
  l.id = r.id
  && List.length l.args = List.length r.args
  && List.for_all2 equals l.args r.args

let rec replaceParams params args expr =
  let argCount = List.length args
  and paramCount = List.length params
  in
  if argCount <> paramCount then
    failwith (sprintf "Macro called with %d parameters, expected %d" paramCount argCount);
  let replacementList = List.combine params args in
  let replace name =
    try List.assoc name replacementList
    with Not_found -> { id = name; args = []; location = None }
  in
  match replace expr.id, expr.args with
    | replacement, [] -> replacement
    | { id = name; args = [] }, _ ->
        { id = name;
          args = List.map (replaceParams params args) expr.args;
          location = None }
    | head, (_::_) ->
        { id = "seq";
          args = head :: List.map (replaceParams params args) expr.args;
          location = None }

let shiftId = function
  | {id = firstArgId; args = []} as expr :: remArgs ->
      { id = firstArgId; args = remArgs; location = expr.location }
  | _ ->
      failwith "shiftId"

let shiftLeft = function
  | { id = id; args = [] } as first :: args -> { first with args = args }
  | args -> {
    id = "seq";
    args = args;
    location = match args with first :: _ -> first.location | [] -> None }

