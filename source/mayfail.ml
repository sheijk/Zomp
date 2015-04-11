open Common

type 'a t =
  | Result of 'a
  | Error of Serror.t list
type 'a mayfail = 'a t

let errorFromString location msg = Error [Serror.fromMsg (Some location) msg]
let errorFromExpr expr msg = Error [Serror.fromExpr expr msg]
let singleError error = Error [error]
let multipleErrors errors = Error errors
let result r = Result r

let combineResults mayfails =
  let errors = ref [] in
  let addTo listRef e = listRef := e :: !listRef in
  let results = mapFilter
                  (function Error msgs -> List.iter (addTo errors) msgs; None | Result r -> Some r)
                  mayfails
  in
  match !errors with
    | [] -> Result results
    | messages -> Error messages

let mapResult f = function
  | Result r -> Result (f r)
  | Error errors -> Error errors

let extractErrors = function
  | Result _ -> []
  | Error errors -> errors

let flattenResult = function
  | Result (Result r) -> Result r
  | Result (Error errors)
  | Error errors -> Error errors

