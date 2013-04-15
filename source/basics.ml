(**
 * Some very basic types which are used by most modules.
 *)

open Printf

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


exception ParseError of location * string

