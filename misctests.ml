

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



    
