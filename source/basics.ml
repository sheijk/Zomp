(**
 * Some very basic types which are used by most modules.
 *)

open Printf

type location = {
  line :int;
  fileName :string;
}

let locationToString loc = sprintf "%s:%d" loc.fileName loc.line

let locationEqual lhs rhs =
  (lhs.line = rhs.line) &&
    ((String.compare lhs.fileName rhs.fileName) = 0)

let formatError location message =
  sprintf "%s: error: %s" (locationToString location) message

let fakeLocation = { fileName = "???.zomp"; line = 1 }

