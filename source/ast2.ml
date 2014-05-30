open Printf
open Basics

type sexpr = {
  id :string;
  args :sexpr list;
  location :location option;
}

type t = sexpr

let withLoc loc ast = { ast with location = Some loc }
let withId id ast = { ast with id }
let withArgs args ast = { ast with args }

let rec removeSourceLocations ast =
  { id = ast.id; location = None; args = List.map removeSourceLocations ast.args }

(** construction functions **)

let idExprLoc location name = { id = name; args = []; location = Some location }
let simpleExprLoc location name args =
  { id = name;
    args = List.map (idExprLoc location) args;
    location = Some location }
let emptyExprLoc location = { id = "seq"; args = []; location = Some location }
let seqExprLoc location args = { id = "seq"; args = args; location = Some location }
let opseqExprLoc location args = { id = "opseq"; args = args; location = Some location }
let exprLoc location name args = { id = name; args = args; location = Some location }
let exprNoLoc name args = { id = name; args = args; location = None }
let juxExprLoc location args = { id = "opjux"; args = args; location = Some location }
let callExprLoc location args = { id = "opcall"; args = args; location = Some location }

let juxExpr args = { id = "opjux"; args = args; location = None }
let callExpr args = { id = "opcall"; args = args; location = None }
let opseqExpr args = { id = "opseq"; args = args; location = None }
let expr = exprNoLoc

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
    | None -> Basics.fakeLocation.fileName

let lineNumber ast =
  match ast.location with
    | Some { line = line } -> line
    | None -> Basics.fakeLocation.line

let column ast =
  match ast.location with
    | Some { column = Some column } -> column
    | _ -> 0

let locationOr ast defaultLocation =
  match ast.location with
    | Some loc -> loc
    | None -> defaultLocation

let location ast = locationOr ast Basics.fakeLocation

let assertHasLocation expr =
  assert (expr.location != None)

(** transformations **)

type stringTree =
  | STLeaf of string
  | STBranch of stringTree list

let toStringTree expr =
  let printLocations = true in
  let lastFileName = ref "" in
  let lastLine = ref 0 in
  let makeLocationIndicator alwaysPrintLoc = function
    | Some { fileName = "" } -> "~"
    | None -> "!"
    | Some loc ->
      if printLocations then begin
        let str =
          match loc.fileName = !lastFileName, loc.line = !lastLine with
            | true, true -> ""
            | true, false -> sprintf " @:%d" loc.line
            | false, false | false, true -> sprintf " @%s" (Basics.locationToString loc)
        in
        lastFileName := loc.fileName;
        lastLine := loc.line;
        str
      end else
        ""
  in

  let rec recurse expr =
    let leafString expr = sprintf "%s%s" expr.id (makeLocationIndicator false expr.location) in
    match expr with
      | { args = [] } ->
        STLeaf (leafString expr)
        (* | { id = "op>" | "op<" | "op=" | "op+" | "op-" | "op*" | "op/" ; args = [lhs; rhs] } -> *)
        (*   STBranch [toStringTree lhs; *)
        (*             STLeaf (leafString { expr with id = Str.string_after expr.id 2 }); *)
        (*             toStringTree rhs] *)
      | _ ->
        let head = STLeaf (leafString expr) in
        let childs = List.map recurse expr.args in
        STBranch (head :: childs)
  in
  recurse expr

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

let expression2string expr = stToString ~maxLength:60 (toStringTree expr)
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
    failwith (sprintf "macro called with %d parameters, expected %d" paramCount argCount);
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

