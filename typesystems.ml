open Printf
  
module type TYPE_SYSTEM =
sig
  type typ
  type value

  exception CouldNotParseType of string

  val typeOf : value -> typ
  val typeName : typ -> string
  val valueString : value -> string
  val parseType : string -> typ
  val parseValue : typ -> string -> value
  val defaultValue : typ -> value
end

module Llvm =
struct
  exception CouldNotParseType of string
  type typ = Void | Int8 | Int32
  type value = VoidValue | Int8Value of int | Int32Value of Int32.t

  let typeInfo = [
    Void, "void";
    Int8, "i8";
    Int32, "i32";
  ]

  let typeOf = function
    | VoidValue -> Void
    | Int8Value _ -> Int8
    | Int32Value _ -> Int32

  let typeName searchedTyp =
    let _, name = List.find (fun (typ, _) -> typ = searchedTyp) typeInfo in
    name

  let valueString = function
    | VoidValue -> "void"
    | Int8Value i -> string_of_int i
    | Int32Value i -> Int32.to_string i

  let parseType typeName =
    try
      let typ, _ = List.find (fun (_, name) -> name = typeName) typeInfo in
      typ
    with
        Not_found -> raise (CouldNotParseType typeName)

  let parseValue typ valueString =
    try
      match typ with
      | Void -> if valueString = "void" then VoidValue else raise (Failure valueString)
      | Int8 -> Int8Value (int_of_string valueString)
      | Int32 -> Int32Value (Int32.of_string valueString)
    with
        _ -> raise (Failure valueString)

  let defaultValue = function
    | Void -> VoidValue
    | Int8 -> Int8Value 0
    | Int32 -> Int32Value 0l
end

module Zomp =
struct
  type integralType = [
  | `Void
  | `Int
  | `Float
  | `Bool
  | `Char
  ]

  type typ = [
  | integralType
  | `Pointer of typ
  | `Record of recordType
  ]
  and recordType = (string * typ) list

  type value =
    | VoidVal
    | IntVal of int
    | FloatVal of float
    | StringLiteral of string
    | BoolVal of bool
    | CharVal of char
    | PointerVal of typ * int option
    | RecordVal of (string * value) list

  exception CouldNotParseType of string

  (* val typeOf : value -> typ *)
  let rec typeOf = function
    | VoidVal -> `Void
    | IntVal _ -> `Int
    | FloatVal _ -> `Float
    | StringLiteral _ -> (`Pointer `Char)
    | BoolVal _ -> `Bool
    | CharVal _ -> `Char
    | PointerVal (t, _) -> t
    | RecordVal components ->
        let convert (name, value) = name, typeOf value in
        `Record (List.map convert components)
          
  (* val typeName : typ -> string *)
  let rec typeName = function
    | `Void -> "void"
    | `Int -> "int"
    | `Float -> "float"
    | `Bool -> "bool"
    | `Char -> "char"
    | `Pointer t -> (typeName t) ^ "*"
(*     | `NamedType name -> name *)
    | `Record components ->
        let rec convert components str =
          match components with
            | [] -> ""
            | (name, typ) :: tail ->
                let s = Printf.sprintf "(%s : %s)" name (typeName typ) in
                convert tail (str ^ s)
        in
        "(" ^ convert components "" ^ ")"

  (* val valueString : value -> string *)
  let rec valueString = function
    | VoidVal -> raise (Failure "no values of void allowed")
    | IntVal i -> string_of_int i
    | FloatVal f -> string_of_float f
    | StringLiteral s -> "\"" ^ s ^ "\""
    | BoolVal b -> string_of_bool b
    | CharVal c -> string_of_int (int_of_char c)
    | PointerVal (typ, target) ->
        begin
          match target with
            | Some addr -> "0x" ^ string_of_int addr
            | None -> "null"
        end
    | RecordVal components ->
        let rec convert = function
          | [] -> ""
          | (name, value) :: tail ->
              (Printf.sprintf "(%s = %s)" name (valueString value)) ^ (convert tail)
        in
        "(" ^ convert components ^ ")"

  (* val parseType : string -> typ *)
  let rec parseType str = 
    let len = String.length str in
    if len >= 1 && str.[len-1] = '*' then
      `Pointer (parseType (Str.string_before str (len - 1)))
    else
      match str with
        | "int" -> `Int
        | "float" -> `Float
        | "bool" -> `Bool
        | "char" -> `Char
        | "void" -> `Void
        | _ as name -> raise (CouldNotParseType name)
        
  (* val parseValue : typ -> string -> value *)
  let parseValue typ str =
    let unquoted quoteChar str =
      let error() =
        raise (Failure (sprintf "Expected format %ctext%c" quoteChar quoteChar));
      in
      let length = String.length str in
      if length < 2 then
        error();        
      let value = String.sub str 1 (length-2) in
      if str.[0] <> quoteChar || str.[length-1] <> quoteChar then
        error();
      value
    in
    match typ with
      | `Void -> raise (Failure "no values of void allowed")
      | `Int -> IntVal (int_of_string str)
      | `Float -> FloatVal (float_of_string str)
      | `Bool -> BoolVal (bool_of_string str)
      | `Char -> CharVal (unquoted '\'' str).[0]
      | `Pointer `Char -> StringLiteral (unquoted '"' str)
      | `Pointer t -> if str == "null"
        then PointerVal (t, None)
        else raise (Failure (sprintf "%s is not a valid pointer value" str))
      | `Record _ -> raise (Failure (sprintf "cannot parse records (value was %s)" str))
(*       | `NamedType name -> failwith (sprintf "Cannot parse value of type %s only known by name" name) *)
          
  (* val defaultValue : typ -> value *)
  let rec defaultValue = function
    | `Void -> VoidVal
    | `Int -> IntVal 0
    | `Float -> FloatVal 0.0
    | `Bool -> BoolVal false
    | `Char -> CharVal (char_of_int 0)
    | `Pointer t -> PointerVal (t, None)
    | `Record components ->
        let convert (name, typ) = name, defaultValue typ in
        RecordVal (List.map convert components)
(*     | `NamedType name -> failwith (sprintf "No default value for type %s only known by name" name) *)
end

module Tests =
struct
  (* ensure that the type systems conform to the TYPE_SYSTEM type *)
  module TypesysTest = functor(Typesys : TYPE_SYSTEM) ->
  struct
  end

  module TestZomp = TypesysTest(Zomp)
  module TestLlvm = TypesysTest(Llvm)
end  
  
