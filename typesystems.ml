
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

module Zomp =
struct
  type intType = [
  | `Int8
  | `Int16
  | `Int32
  | `Int64
  ]

  type integralType = [
  | `Void
  | intType
  | `Float
  | `Double
  | `Bool
  | `Char
  ]

  type typ = [
  | integralType
  | `Pointer of typ
  | `Array of typ * int
  | `Record of recordType
  | `TypeRef of string
  | `Function of functionType
  ]
  and recordType = {
    rname :string;
    fields :(string * typ) list;
  }
  and functionType = {
    returnType :typ;
    argTypes :typ list;
  }

  type value =
    | VoidVal
    | Int8Val of Int32.t
    | Int16Val of Int32.t
    | Int32Val of Int32.t
    | Int64Val of Int64.t
    | FloatVal of float
    | DoubleVal of float
    | StringLiteral of string
    | BoolVal of bool
    | CharVal of char
    | PointerVal of typ * int option
    | ArrayVal of typ * value list
    | RecordVal of string * (string * value) list
    | FunctionVal of functionType * int option

  exception CouldNotParseType of string

  let rec canonicType lookupType = function
    | `TypeRef typeName ->
        begin match lookupType typeName with
          | `Found `TypeRef sameName when sameName = typeName -> `TypeRef typeName
          | `Found typ -> canonicType lookupType typ
          | `NotFound -> (failwith "")
        end
    | `Pointer targetType -> `Pointer (canonicType lookupType targetType)
    | `Record record ->
        (* TODO: apply2nd *)
        let canonicField (name, fieldType) = (name, canonicType lookupType fieldType) in
        let canonicFields = List.map canonicField record.fields in
        `Record { record with fields = canonicFields }
    | t -> t

  (* val typeOf : value -> typ *)
  let rec typeOf = function
    | VoidVal -> `Void
    | Int8Val _ -> `Int8
    | Int16Val _ -> `Int16
    | Int32Val _ -> `Int32
    | Int64Val _ -> `Int64
    | FloatVal _ -> `Float
    | DoubleVal _ -> `Double
    | StringLiteral _ -> (`Pointer `Char)
    | BoolVal _ -> `Bool
    | CharVal _ -> `Char
    | PointerVal (t, _) -> t
    | ArrayVal (typ, values) ->
        assert (List.for_all (fun value -> typ = typeOf value) values);
        typ
    | RecordVal (typeName, components) ->
        let convert (name, value) = name, typeOf value in
        `Record { rname = typeName; fields = List.map convert components }
    | FunctionVal (t, _) -> `Function t

  (* val typeName : typ -> string *)
  let rec typeName : typ -> string = function
    | `Void -> "void"
    | `Int8 -> "u8"
    | `Int16 -> "u16"
    | `Int32 -> "u32"
    | `Int64 -> "u64"
    | `Float -> "float"
    | `Double -> "double"
    | `Bool -> "bool"
    | `Char -> "char"
    | `TypeRef name -> name
    | `Pointer t -> (typeName t) ^ "*"
    | `Array (baseType, size) ->
        sprintf "%s[%d]" (typeName baseType) size
    | `Record record ->
        record.rname
        (* let component2String (name, typ) = sprintf "%s :%s" name (typeName typ) in *)
        (* let componentStrings = List.map component2String record.fields in *)
        (* "(" ^ Common.combine ", " componentStrings ^ ")" *)
    | `Function ft ->
        let retName = typeName ft.returnType in
        let argNames = List.map typeName ft.argTypes in
        sprintf "%s -> %s" (Common.combine ", " argNames) retName

  (* val valueString : value -> string *)
  let rec valueString : value -> string =
    let pointerValueName = function
      | Some addr -> "0x" ^ string_of_int addr
      | None -> "null"
    in
    function
      | VoidVal -> raise (Failure "no values of void allowed")
      | Int8Val i -> Int32.to_string i
      | Int16Val i -> Int32.to_string i
      | Int32Val i -> Int32.to_string i
      | Int64Val i -> Int64.to_string i
      | FloatVal f -> string_of_float f
      | DoubleVal f -> string_of_float f
      | StringLiteral s -> "\"" ^ s ^ "\""
      | BoolVal b -> string_of_bool b
      | CharVal c -> string_of_int (int_of_char c)
      | PointerVal (_, target) -> pointerValueName target
      | ArrayVal (_, values) ->
          sprintf "[%s]" (Common.combine ", " (List.map valueString values))
      | RecordVal (_, components) ->
          let rec convert = function
            | [] -> ""
            | (name, value) :: tail ->
                (Printf.sprintf "(%s = %s)" name (valueString value)) ^ (convert tail)
          in
          "(" ^ convert components ^ ")"
      | FunctionVal (_, target) -> pointerValueName target

  (* val parseType : string -> typ *)
  let rec parseType (str :string) :typ =
    let len = String.length str in
    if len >= 1 && str.[len-1] = '*' then
      `Pointer (parseType (Str.string_before str (len - 1)))
    else
      match str with
        | "u8" -> `Int8
        | "u16" -> `Int16
        | "u32" -> `Int32
        | "u64" -> `Int64
        | "float" -> `Float
        | "double" -> `Double
        | "bool" -> `Bool
        | "char" -> `Char
        | "void" -> `Void
        | _ as name -> raise (CouldNotParseType name)

  (* val parseValue : typ -> string -> value *)
  let parseValue (typ :typ) str :value =
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
      | `TypeRef name -> failwith (sprintf "Cannot parse value of type %s referred by name" name)
      | `Void -> failwith "no values of void allowed"
      | `Int8 -> Int8Val (Int32.of_string str)
      | `Int16 -> Int16Val (Int32.of_string str)
      | `Int32 -> Int32Val (Int32.of_string str)
      | `Int64 -> Int64Val (Int64.of_string str)
      | `Float -> FloatVal (Common.restrictToSingleprecision (float_of_string str))
      | `Double -> DoubleVal (float_of_string str)
      | `Bool -> BoolVal (bool_of_string str)
      | `Char -> CharVal (unquoted '\'' str).[0]
      | `Pointer `Char -> StringLiteral (unquoted '"' str)
      | `Pointer t -> if str == "null"
        then PointerVal (t, None)
        else raise (Failure (sprintf "%s is not a valid pointer value" str))
      | `Array _ -> raise (Failure (sprintf "Cannot parse arrays (value was %s)" str))
      | `Record _ -> raise (Failure (sprintf "Cannot parse records (value was %s)" str))
      | `Function _ -> raise (Failure (sprintf "Cannot parse function ptr values (value was %s)" str))

  let rec defaultValue : typ -> value = function
    | `Void -> VoidVal
    | `TypeRef name -> failwith (sprintf "No default value for type %s referred by name" name)
    | `Int8 -> Int8Val 0l
    | `Int16 -> Int16Val 0l
    | `Int32 -> Int32Val 0l
    | `Int64 -> Int64Val 0L
    | `Float -> FloatVal 0.0
    | `Double -> DoubleVal 0.0
    | `Bool -> BoolVal false
    | `Char -> CharVal (char_of_int 0)
    | `Pointer t -> PointerVal (t, None)
    | `Record record ->
        let convert (name, typ) = name, defaultValue typ in
        RecordVal (record.rname, List.map convert record.fields)
    | `Array (memberType, size) ->
        ArrayVal (memberType, Common.listCreate size (defaultValue memberType))
    | `Function t -> FunctionVal (t, None)
end

module Tests =
struct
  (* ensure that the type systems conform to the TYPE_SYSTEM type *)
  module TypesysTest = functor(Typesys : TYPE_SYSTEM) ->
  struct
  end

  module TestZomp = TypesysTest(Zomp)
(*   module TestLlvm = TypesysTest(Llvm) *)
end

