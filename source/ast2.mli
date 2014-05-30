(** Basic AST type **)

type sexpr = private {
  id : string;
  args : sexpr list;
  location : Basics.location option;
}

type t = sexpr

(** Constructors. Variants w/o source location are deprecated. *)

val idExpr : string -> sexpr
val idExprLoc : Basics.location -> string -> sexpr
val simpleExpr : string -> string list -> sexpr
val simpleExprLoc : Basics.location -> string -> string list -> sexpr
val emptyExpr : sexpr
val emptyExprLoc : Basics.location -> sexpr
val seqExpr : sexpr list -> sexpr
val seqExprLoc : Basics.location -> sexpr list -> sexpr
val opseqExpr : sexpr list -> sexpr
val opseqExprLoc : Basics.location -> sexpr list -> sexpr
val expr : string -> sexpr list -> sexpr
val exprLoc : Basics.location -> string -> sexpr list -> sexpr
val juxExpr : sexpr list -> sexpr
val juxExprLoc : Basics.location -> sexpr list -> sexpr
val callExpr : sexpr list -> sexpr
val callExprLoc : Basics.location -> sexpr list -> sexpr

val juxExprInferLoc : sexpr list -> sexpr
val callExprInferLoc : sexpr list -> sexpr
val seqExprInferLoc : sexpr list -> sexpr
val exprInferLoc : string -> sexpr list -> sexpr

val withLoc : Basics.location -> t -> t
val withId : string -> t -> t
val withArgs : t list -> t -> t

(** Source location handling. *)

val fileName : sexpr -> string
val lineNumber : sexpr -> int
val column : sexpr -> int

val locationOr : sexpr -> Basics.location -> Basics.location
val location : t -> Basics.location
val assertHasLocation : sexpr -> unit

val removeSourceLocations : t -> t

(** Utility functions. *)

type stringTree = STLeaf of string | STBranch of stringTree list
val toStringTree : sexpr -> stringTree
val stToString : maxLength:int -> ?indent:int -> stringTree -> string
val expression2string : sexpr -> string

val toString : sexpr -> string
val equals : sexpr -> sexpr -> bool
val replaceParams : string list -> sexpr list -> sexpr -> sexpr
val shiftId : sexpr list -> sexpr
val shiftLeft : sexpr list -> sexpr
