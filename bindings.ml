open Lang

module StringMap = Map.Make(String)

type symbol =
  | VarSymbol of composedType variable
  | FuncSymbol of func
  | MacroSymbol of bindings macro
  | TypedefSymbol of composedType
  | LabelSymbol of label
  | UndefinedSymbol
and bindings = symbol StringMap.t

type t = bindings

let defaultBindings : bindings = StringMap.empty

let fromSymbolList (list : (string * symbol) list) : bindings =
  List.fold_left
    (fun bindings (name, symbol) -> StringMap.add name symbol bindings)
    defaultBindings
    list

let addVar bindings var =
  StringMap.add var.vname (VarSymbol var) bindings

let addFunc bindings func =
  StringMap.add func.fname (FuncSymbol func) bindings

let addTypedef bindings name typ =
  StringMap.add name (TypedefSymbol typ) bindings

let addLabel bindings name =
  StringMap.add name (LabelSymbol { lname = name }) bindings

let addMacro bindings name doc implF =
  let macro = { mname = name; mdocstring = doc; mtransformFunc = implF } in
  StringMap.add name (MacroSymbol macro) bindings

let lookup bindings name =
  try
    StringMap.find name bindings
  with
      Not_found -> UndefinedSymbol

let isFunction bindings name =
  match lookup bindings name with
    | FuncSymbol _ -> true
    | _ -> false

let iter f bindings =
  let f' key sym = f (key, sym) in
  StringMap.iter f' bindings

