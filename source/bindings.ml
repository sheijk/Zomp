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
  location : Basics.location option;
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

let symbolLocation = function
  | VarSymbol { Lang.vlocation = loc }
  | FuncSymbol { Lang.flocation = loc }
  | MacroSymbol { Lang.mlocation = loc }
    -> loc
  | TypedefSymbol _
  | LabelSymbol _
  | UndefinedSymbol
    -> None

let addSymbol name symbol location bindings =
  let sequenceNum = bindings.prevSequenceNum + 1 in
  let info =  { symbol; sequenceNum = sequenceNum; location } in
  { symbols = StringMap.add name info bindings.symbols;
    prevSequenceNum = sequenceNum }

let fromSymbolList (list : (string * symbol) list) : bindings =
  List.fold_left
    (fun bindings (name, symbol) -> addSymbol name symbol (symbolLocation symbol) bindings)
    defaultBindings
    list

let addVar bindings var =
  addSymbol var.vname (VarSymbol var) var.vlocation bindings

let addFunc bindings func =
  addSymbol func.fname (FuncSymbol func) func.flocation bindings

let addTypedef bindings name typ location =
  addSymbol name (TypedefSymbol typ) (Some location) bindings

let addLabel bindings name =
  addSymbol name (LabelSymbol { lname = name }) None bindings

let addMacro bindings name doc location implF =
  let macro = { mname = name; mdocstring = doc; mtransformFunc = implF; mlocation = Some location } in
  addSymbol name (MacroSymbol macro) macro.mlocation bindings

let lookup bindings name =
  try
    (StringMap.find name bindings.symbols).symbol
  with
      Not_found -> UndefinedSymbol

let lookupInfo bindings name =
  try
    StringMap.find name bindings.symbols
  with
      Not_found -> { symbol = UndefinedSymbol; sequenceNum = 0; location = None }

let location info = info.location
let symbol info = info.symbol

let isFunction bindings name =
  match lookup bindings name with
    | FuncSymbol _ -> true
    | _ -> false

let iterInfo f bindings =
  let f' key info = f (key, info) in
  StringMap.iter f' bindings.symbols

let iter f bindings =
  let f' (key, info) = f (key, info.symbol) in
  iterInfo f' bindings

let kindToString = function
  | VarSymbol _ -> "var"
  | FuncSymbol _ -> "func"
  | MacroSymbol _ -> "macro"
  | TypedefSymbol _ -> "typedef"
  | LabelSymbol _ -> "label"
  | UndefinedSymbol -> "undefined"

