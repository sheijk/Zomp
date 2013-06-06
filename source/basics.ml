(**
 * Some very basic types which are used by most modules.
 *)

open Printf
open Common

type location = {
  fileName :string;
  line :int;
  column :int option;
}

let location fileName line column = { fileName; line; column }

(** A fake location used in places where location info is not available due to
legacy code. Always use this value so it get's easier to search for places that
need to be fixed. This should go away once all code in the compiler handles
locations properly. *)
let fakeLocation = { fileName = "???.zomp"; line = 1; column = None }

(** The location used for compiler provided types, macros, etc. *)
let builtinLocation = location "builtin" 0 None

let locationToString loc =
  match loc.column with
    | Some column ->
      sprintf "%s:%d:%d" loc.fileName loc.line column
    | None ->
      sprintf "%s:%d" loc.fileName loc.line

let locationOptToString locOpt =
  locationToString (someOrDefault locOpt fakeLocation)

let locationEqual lhs rhs =
  (lhs.line = rhs.line) &&
    ((String.compare lhs.fileName rhs.fileName) = 0)

(** Catch some common kinds of invalid locations. *)
let locationValid = function
  | { fileName = ""; line = 0; column = (Some 0 | None) } ->
    false
  | _ ->
    true

module DiagnosticKind =
struct
  type t =
  | Error
  | Warning
  | Info
  | Other of string

  let toString = function
    | Error -> "error"
    | Warning -> "warning"
    | Info -> "info"
    | Other str -> sprintf "other('%s')" str

  let parse = function
    | "error" -> Error
    | "warning" -> Warning
    | "info" -> Info
    | otherString -> Other otherString
end

let formatDiagnostics kind location message =
  sprintf "%s: %s: %s" (locationToString location) (DiagnosticKind.toString kind) message

let formatError = formatDiagnostics DiagnosticKind.Error
let formatWarning = formatDiagnostics DiagnosticKind.Warning
let formatInfo = formatDiagnostics DiagnosticKind.Info

let diagnosticRe =
  let fileRe = "\\([a-zA-Z_0-9/\\.-]+\\)" in
  let lineRe = "\\([0-9]+\\)" in
  let columnColonRe = "\\([0-9]+:\\)" in
  let re =
    sprintf "^%s:%s:%s? *\\(error\\|warning\\|info\\): \\(.*\\)"
      fileRe lineRe columnColonRe
  in
  Str.regexp re
  
let parseDiagnostics line =
  if (Str.string_match diagnosticRe line 0) then
    let fileName = Str.matched_group 1 line in
    let lineNum = int_of_string (Str.matched_group 2 line) in
    let column =
      try
        let columnStr, _ = splitLastChar (Str.matched_group 3 line) in
        Some (int_of_string columnStr)
      with Not_found ->
        None
    in
    let kind = DiagnosticKind.parse (Str.matched_group 4 line) in
    let message = Str.matched_group 5 line in
    Some (location fileName lineNum column, kind, message)
  else
    None

exception ParseError of location * string

