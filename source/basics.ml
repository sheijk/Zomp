(**
 * Some very basic types which are used by most modules.
 *)

open Printf

type location = {
  line :int;
  fileName :string;
}

let locationToString loc = sprintf "%s:%d" loc.fileName loc.line

let formatError location message =
  sprintf "%s: error: %s" (locationToString location) message

let fakeLocation = { fileName = "???.zomp"; line = 1 }

