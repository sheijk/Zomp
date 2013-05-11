(**
 * Some very basic types which are used by most modules.
 *)

open Printf
open Common

type location = {
  fileName :string;
  line :int;
}

let location fileName line = { fileName; line }

let fakeLocation = { fileName = "???.zomp"; line = 1 }

let locationToString loc = sprintf "%s:%d" loc.fileName loc.line

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
    let re = (sprintf "^\\(%s\\):\\([0-9]+\\)+: .*\\(error\\|warning\\|info\\): \\(.*\\)" (Str.quote fileName)) in
    Str.regexp re
  in
  let parseDiagnostics line =
    let isDiagnostics = Str.string_match diagnosticRe line 0 in
    if isDiagnostics then begin
      let fileName = Str.matched_group 1 line in
      let lineNum = safeParseInt (Str.matched_group 2 line) in
      let message = Str.matched_group 4 line in
      Some (location fileName lineNum, message)
    end else
      None
  in
  parseDiagnostics

exception ParseError of location * string

