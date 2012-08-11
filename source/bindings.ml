open Lang

module StringMap = Map.Make(String)

type symbol =
  | VarSymbol of composedType variable
  | FuncSymbol of func
  | MacroSymbol of bindings macro
  | TypedefSymbol of composedType
  | LabelSymbol of label
  | UndefinedSymbol
and symbolInfo = {
  symbol : symbol;
  (** unique id of this symbol. redefinitions of the same symbol in the toplevel
      get new sequence numbers *)
  sequenceNum : int;
}
and bindings = {
  symbols : symbolInfo StringMap.t;
  (** unique index of the last added symbol *)
  prevSequenceNum : int;
}

type t = bindings

let defaultBindings : bindings = { symbols = StringMap.empty; prevSequenceNum = 0 }

let addSymbol name sym bindings =
  let sequenceNum = bindings.prevSequenceNum + 1 in
  let info =  {symbol = sym; sequenceNum = sequenceNum} in
  { symbols = StringMap.add name info bindings.symbols;
    prevSequenceNum = sequenceNum }

let fromSymbolList (list : (string * symbol) list) : bindings =
  List.fold_left
    (fun bindings (name, symbol) -> addSymbol name symbol bindings)
    defaultBindings
    list

let addVar bindings var =
  addSymbol var.vname (VarSymbol var) bindings

let addFunc bindings func =
  addSymbol func.fname (FuncSymbol func) bindings

let addTypedef bindings name typ =
  addSymbol name (TypedefSymbol typ) bindings

let addLabel bindings name =
  addSymbol name (LabelSymbol { lname = name }) bindings

let addMacro bindings name doc implF =
  let macro = { mname = name; mdocstring = doc; mtransformFunc = implF } in
  addSymbol name (MacroSymbol macro) bindings

let lookup bindings name =
  try
    (StringMap.find name bindings.symbols).symbol
  with
      Not_found -> UndefinedSymbol

let lookupInfo bindings name =
  try
    StringMap.find name bindings.symbols
  with
      Not_found -> { symbol = UndefinedSymbol; sequenceNum = 0 }

let isFunction bindings name =
  match lookup bindings name with
    | FuncSymbol _ -> true
    | _ -> false

let iter f bindings =
  let f' key info = f (key, info.symbol) in
  StringMap.iter f' bindings.symbols

let symbolTypeToString = function
  | VarSymbol _ -> "VarSymbol"
  | FuncSymbol _ -> "FuncSymbol"
  | MacroSymbol _ -> "MacroSymbol"
  | TypedefSymbol _ -> "TypedefSymbol"
  | LabelSymbol _ -> "LabelSymbol"
  | UndefinedSymbol -> "UndefinedSymbol"

