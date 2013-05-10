
type t = private {
  emsg: string;
  eloc: Basics.location option;
  eexpr: Ast2.t option;
}

val toString : t -> string
val diagnosticsToString : string -> t -> string

val fromMsg : Basics.location option -> string -> t
val fromExpr : Ast2.t -> string -> t
val fromException : Basics.location option -> exn -> t

