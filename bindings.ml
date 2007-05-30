open Lang

type symbol =
  | VarSymbol of variable
  | FuncSymbol of func
  | MacroSymbol of macro
  | UndefinedSymbol

type bindings = (string * symbol) list

let defaultBindings : bindings = []

let addVar bindings var : bindings = (var.vname, VarSymbol var) :: bindings

let addFunc bindings func : bindings = (func.fname, FuncSymbol func) :: bindings
  
let rec lookup (bindings :bindings) name =
  match bindings with
    | [] -> UndefinedSymbol
    | (symName, sym) :: tail when symName = name -> sym
    | _ :: tail -> lookup tail name

let isFunction bindings name =
  match lookup bindings name with
    | FuncSymbol _ -> true
    | _ -> false
        
