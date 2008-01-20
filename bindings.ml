open Lang

type symbol =
  | VarSymbol of composedType variable
  | FuncSymbol of func
  | MacroSymbol of bindings macro
  | TypedefSymbol of composedType
  | LabelSymbol of label
  | UndefinedSymbol
and bindings = (string * symbol) list

type t = bindings
    
let defaultBindings : bindings = []

let addVar bindings var : bindings = (var.vname, VarSymbol var) :: bindings

let addFunc bindings func : bindings = (func.fname, FuncSymbol func) :: bindings

let addTypedef bindings name typ = (name, TypedefSymbol typ) :: bindings

let addLabel bindings name = (name, LabelSymbol { lname = name }) :: bindings

let addMacro bindings name doc implF = (name, MacroSymbol { mname = name; mdocstring = doc; mtransformFunc = implF }) :: bindings

let lookup (bindings :bindings) name =
  let rec worker bindings = 
    match bindings with
      | [] -> UndefinedSymbol
      | (symName, sym) :: tail when symName = name -> sym
      | _ :: tail -> worker tail
  in
  worker bindings

let isFunction bindings name =
  match lookup bindings name with
    | FuncSymbol _ -> true
    | _ -> false
        
