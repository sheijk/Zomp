

type typeStructure = [
| `Int
| `Bool
| `Pointer of typ
| `Record of components
| `Abstract
]
and components = (string * typ) list
and typ = {
  tname :string;
  tstructure :typeStructure;
}

type integral = [
| `Int
| `Bool
]

type composed = [
| integral
| `Pointer of composed
]
    

let canBeDivided num div = (num / div) * div = num
  
let isPrime num =
  let rec worker = function
    | div when div <= 1 -> true
    | div when canBeDivided num div -> false
    | div -> worker (div - 1)
  in
  if num <= 1 then false
  else worker (num / 2)

let rec fromto lower upper =
  if lower <= upper then lower :: fromto (lower+1) upper
  else []

let (>>>) = fromto

  
