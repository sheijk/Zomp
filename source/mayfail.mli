
type 'a t = Result of 'a | Error of Serror.t list
type 'a mayfail = 'a t

val errorFromString : Basics.location -> string -> 'a mayfail
val errorFromExpr : Ast2.sexpr -> string -> 'a mayfail
val singleError : Serror.t -> 'a mayfail
val multipleErrors : Serror.t list -> 'a mayfail

val result : 'a -> 'a mayfail

val combineResults : 'a mayfail list -> 'a list mayfail
val mapResult : ('m -> 'n) -> 'm mayfail -> 'n mayfail
val extractErrors : 'o mayfail -> Serror.t list
val flattenResult : 'p mayfail mayfail -> 'p mayfail

