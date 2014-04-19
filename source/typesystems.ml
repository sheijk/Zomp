
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

module New_system =
struct
  type intTypeSize = Bits8 | Bits16 | Bits32 | Bits64 | BitsArbitrary of int

  type typ = [
  | `Void
  | `Bool
  | `UnsignedInt of intTypeSize
  | `SignedInt of intTypeSize
  | `Floating of Int64.t
  | `Record of (string * typ) list
  | `Array of typ * int
  | `Pointer of typ
  | `FunctionType of functionType
  | `NamedType of namedType
  | `TypeParamType of typ
  | `TypeParamInt of int
  | `ParametricType of typ * typeParam list
  ]
  and namedType = {
    tname :string;
    tparams :typeParam list;
    trepr :typ
  }
  and functionType = {
    returnType :typ;
    argTypes :typ list;
  }
  and typeParam =
    (* | ParamTypeParam of kind *)
    | TypeParam
    | IntParam
    (* | BoolParam of bool *)
    (* | FloatParam of float *)
    (* ... *)

  (** Char = `NamedType ("char", `UnsignedInt of Bits8) *)
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

  type 'typ parameterizableType = [
  | `Pointer of 'typ
  | `Record of 'typ recordType
  ]
  and 'typ recordType = {
    rname :string;
    fields :(string * 'typ) list;
  }

  type typ = [
  | integralType
  | `Array of typ * int
  | `TypeRef of string
  | `Function of functionType
  | typ parameterizableType

  | `ParametricType of typ parameterizableType
  | `TypeParam

  (** string is only an indicator for debugging, not a user-visible error message *)
  | `ErrorType of string
  ]
  and functionType = {
    returnType :typ;
    argTypes :typ list;
  }
  (* and typeParam = *)
  (*   | TypeParam *)
  (*   | IntParam *)
        (* | BoolParam of bool *)
        (* | FloatParam of float *)
        (* ... *)

  let bitcount : intType -> int = function
    | `Int8 -> 8
    | `Int16 -> 16
    | `Int32 -> 32
    | `Int64 -> 64

  let rec isTypeParametric : typ -> bool = function
    | `TypeParam
    | `ParametricType _ ->
        true
    | `Array (t, _)
    | `Pointer t ->
        isTypeParametric t
    | `Record rv ->
        List.exists (fun (_, t) -> isTypeParametric t) rv.fields
    | `TypeRef _
    | `Function _
    | #integralType ->
        false
    | `ErrorType _ ->
      false

  type value =
    | VoidVal
    | Int8Val of Int32.t
    | Int16Val of Int32.t
    | Int32Val of Int32.t
    | Int64Val of Int64.t
    | FloatVal of float
    | DoubleVal of float (** OCaml float = IEE-754 double *)
    | StringLiteral of string
    | BoolVal of bool
    | CharVal of char
    | NullpointerVal of typ
    | ArrayVal of typ * value list
    | RecordVal of string * (string * value) list
    (** string is only an indicator for debugging *)
    | ErrorVal of string

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
    | NullpointerVal t -> `Pointer t
    | ArrayVal (typ, values) ->
        assert (List.for_all (fun value -> typ = typeOf value) values);
        typ
    | RecordVal (typeName, components) ->
        let convert (name, value) = name, typeOf value in
        `Record { rname = typeName; fields = List.map convert components }
    | ErrorVal msg -> `ErrorType msg


  let recordDescr typeNameRec record =
    let component2String (name, typ) = sprintf "%s :%s" name (typeNameRec typ) in
    let componentStrings = List.map component2String record.fields in
    record.rname ^ " {" ^ Common.combine ", " componentStrings ^ "}"

  let typeNameRec typeNameRec : typ -> string = function
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
    | `Pointer t -> (typeNameRec t) ^ "*"
    | `Array (`Function _ as baseType, size) ->
        sprintf "(%s)[%d]" (typeNameRec baseType) size
    | `Array (baseType, size) ->
        sprintf "%s[%d]" (typeNameRec baseType) size
    | `Record record ->
        record.rname
    | `Function ft ->
        let retName = typeNameRec ft.returnType in
        let argNames = List.map typeNameRec ft.argTypes in
        sprintf "%s(%s)" retName (Common.combine ", " argNames)
    | `ParametricType t ->
        typeNameRec (t :> typ) ^ "!T"
    | `TypeParam -> "'T"
    | `ErrorType msg -> "error_t(\"" ^ msg ^ "\")"

  (* val typeName : typ -> string *)
  let rec typeName (typ :typ) : string =
    typeNameRec typeName typ

  let rec typeDescr = function
    | `Record record ->
      recordDescr typeDescr record
    | `ParametricType t ->
        "<T> " ^ typeDescr (t :> typ)
    | other ->
        typeName other

  let rec typeNameExplicit = function
    | `TypeRef name -> sprintf "typedef %s" name
    | `Pointer t -> typeNameExplicit t ^ "*"
    | `Record record -> recordDescr typeNameExplicit record
    | other -> typeNameRec typeNameExplicit other

  let sizeTName = "size_t"

  let rec valueString : value -> string =
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
      | NullpointerVal t -> sprintf "nullptr(%s)" (typeName t)
      | ArrayVal (_, values) ->
          sprintf "[%s]" (Common.combine ", " (List.map valueString values))
      | RecordVal (_, components) ->
          let rec convert = function
            | [] -> ""
            | (name, value) :: tail ->
                (Printf.sprintf "(%s = %s)" name (valueString value)) ^ (convert tail)
          in
          "(" ^ convert components ^ ")"
      | ErrorVal msg ->
        "error(\"" ^ msg ^ "\")"

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
        | "error_t" -> `ErrorType ""
        | _ as name ->
          if Str.string_match (Str.regexp "error_t(\"\\([^\"]*\\)\")") name 0 then
            `ErrorType (Str.matched_group 1 name)
          else
            raise (CouldNotParseType name)

  (* val parseValue : typ -> string -> value *)
  let parseValue (typ :typ) str :value =
    let unquoted quoteChar str =
      let error() =
        raise (Failure (sprintf "expected format %ctext%c" quoteChar quoteChar));
      in
      let length = String.length str in
      if length < 2 then
        error();
      let value = String.sub str 1 (length-2) in
      if str.[0] <> quoteChar || str.[length-1] <> quoteChar then
        error();
      value
    in
    try
      begin match typ with
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
          then NullpointerVal t
          else failwith "only null is a valid pointer value"
        | `Array _ -> failwith "cannot parse array values"
        | `Record _ -> failwith "cannot parse record values"
        | `Function _ -> failwith "cannot parse function ptr values"
        | `ParametricType _ -> failwith "cannot parse values of parametric type"
        | `TypeParam -> failwith "cannot parse a type parameter"
        | `TypeRef name ->
          failwith (sprintf "cannot parse value of type %s referred by name" name)
        | `ErrorType _ as t ->
          failwith (sprintf "cannot parse value of type %s" (typeName t))
      end with
        | Failure s -> failwith (sprintf "%s (when parsing %s)" s str)

  let rec defaultValue : typ -> value = function
    | `Void -> VoidVal
    | `TypeRef name -> failwith (sprintf "no default value for type %s referred by name" name)
    | `Int8 -> Int8Val 0l
    | `Int16 -> Int16Val 0l
    | `Int32 -> Int32Val 0l
    | `Int64 -> Int64Val 0L
    | `Float -> FloatVal 0.0
    | `Double -> DoubleVal 0.0
    | `Bool -> BoolVal false
    | `Char -> CharVal (char_of_int 0)
    | `Pointer t -> NullpointerVal t
    | `Record record ->
        let convert (name, typ) = name, defaultValue typ in
        RecordVal (record.rname, List.map convert record.fields)
    | `Array (memberType, size) ->
        ArrayVal (memberType, Common.listCreate size (defaultValue memberType))
    | `Function t -> NullpointerVal (`Function t)
    | `ParametricType t ->
        failwith (sprintf "no default value for parametric types (here: %s)"
                    (typeName (t :> typ)))
    | `TypeParam ->
        failwith "no default value for type parameter"

    | `ErrorType msg -> ErrorVal msg
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

