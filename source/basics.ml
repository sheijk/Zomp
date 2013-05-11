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

let fakeLocation = { fileName = "???.zomp"; line = 1; column = None }

let locationToString loc =
  match loc.column with
    | Some column ->
      sprintf "%s:%d:%d" loc.fileName loc.line column
    | None ->
      sprintf "%s:%d" loc.fileName loc.line

let locationEqual lhs rhs =
  (lhs.line = rhs.line) &&
    ((String.compare lhs.fileName rhs.fileName) = 0)


let formatDiagnostics kind location message =
  sprintf "%s: %s: %s" (locationToString location) kind message

let formatError = formatDiagnostics "error"
let formatWarning = formatDiagnostics "warning"
let formatInfo = formatDiagnostics "info"

let makeDiagnosticChecker fileName =
  let diagnosticRe =
    let re = (sprintf "^\\(%s\\):\\([0-9]+\\)+:\\([0-9]+:\\)? .*\\(error\\|warning\\|info\\): \\(.*\\)" (Str.quote fileName)) in
    Str.regexp re
  in
  let parseDiagnostics line =
    let isDiagnostics = Str.string_match diagnosticRe line 0 in
    if isDiagnostics then begin
      let fileName = Str.matched_group 1 line in
      let lineNum = int_of_string (Str.matched_group 2 line) in
      let column =
        try
          let columnStr, _ = splitLastChar (Str.matched_group 3 line) in
          Some (int_of_string columnStr)
        with Not_found ->
            None
      in
      let kindStr = Str.matched_group 4 line in
      ignore kindStr;
      let message = Str.matched_group 5 line in
      Some (location fileName lineNum column, message)
    end else
      None
  in
  parseDiagnostics

exception ParseError of location * string

