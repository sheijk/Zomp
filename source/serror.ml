(** Single error, to avoid name clash with Error constructor in mayfail type *)

open Printf
open Common
open Ast2

type t = {
  emsg: string;
  eloc: Basics.location option;
  eexpr: Ast2.t option;
}

(** TODO: add 'in $expr' if location is missing *)
let diagnosticsToString kind error =
  let loc =
    match error.eloc, error.eexpr with
      | Some location, _ ->
        location
      | None, Some { location = Some location } ->
        location
      | None, Some { location = None }
      | None, None ->
        Basics.fakeLocation
  in
  Basics.formatDiagnostics kind loc error.emsg

let toString error =
  diagnosticsToString Basics.DiagnosticKind.Error error

let check funcName error =
  let warn msg =
    let shortMsg = restrictLength 80 (toString error) in
    printf "alert, %s (in %s, msg = %s)!\n" msg funcName shortMsg
  in
  if error.emsg =~ ".*\\.zomp:[0-9]+: error:" then begin
    warn "error location has been added twice";
    if error.eloc = None then
      warn "error location missing";
    Printexc.print_backtrace stdout;
    flush stdout;
  end;
  if String.length error.emsg = 0 then
    warn "empty error message"
  else begin
    let firstChar = error.emsg.[0] in
    if firstChar >= 'A' && firstChar <= 'Z' then
      warn "error message begins with upper case letter";
  end;
  error

let checkAll funcName errors =
  ignore (List.map (check funcName) errors);
  ()

let fromMsg eloc emsg = { emsg; eloc; eexpr = None }

let fromExpr expr msg =
  let eexpr, emsg =
    match expr.location with
      | Some location ->
        None, msg
      | None ->
        Some expr, sprintf "%s in %s" msg (Ast2.toString expr)
  in
  { emsg; eloc = expr.location; eexpr }

let fromMsg eloc emsg = check "fromMsg" (fromMsg eloc emsg)
let fromExpr expr msg = check "fromExpr" (fromExpr expr msg)
let fromException eloc exn = fromMsg eloc (sprintf "unknown exception: %s" (Printexc.to_string exn))

