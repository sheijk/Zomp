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
module New_system :
  sig
    type intTypeSize =
        Bits8
      | Bits16
      | Bits32
      | Bits64
      | BitsArbitrary of int
    type typ =
        [ `Array of typ * int
        | `Bool
        | `Floating of Int64.t
        | `FunctionType of functionType
        | `NamedType of namedType
        | `ParametricType of typ * typeParam list
        | `Pointer of typ
        | `Record of (string * typ) list
        | `SignedInt of intTypeSize
        | `TypeParamInt of int
        | `TypeParamType of typ
        | `UnsignedInt of intTypeSize
        | `Void ]
    and namedType = {
      tname : string;
      tparams : typeParam list;
      trepr : typ;
    }
    and functionType = { returnType : typ; argTypes : typ list; }
    and typeParam = TypeParam | IntParam
  end
module Zomp :
  sig
    type intType = [ `Int16 | `Int32 | `Int64 | `Int8 ]
    type integralType =
        [ `Bool
        | `Char
        | `Double
        | `Float
        | `Int16
        | `Int32
        | `Int64
        | `Int8
        | `Void ]
    type 'a parameterizableType =
        [ `Pointer of 'a | `Record of 'a recordType ]
    and 'a recordType = { rname : string; fields : (string * 'a) list; }
    type typ =
        [ `Array of typ * int
        | `Bool
        | `Char
        | `Double
        | `ErrorType of string
        | `Float
        | `Function of functionType
        | `Int16
        | `Int32
        | `Int64
        | `Int8
        | `ParametricType of typ parameterizableType
        | `Pointer of typ
        | `Record of typ recordType
        | `TypeParam
        | `TypeRef of string
        | `Void ]
    and functionType = { returnType : typ; argTypes : typ list; }
    val bitcount : intType -> int
    val isTypeParametric : typ -> bool
    type value =
        VoidVal
      | Int8Val of Int32.t
      | Int16Val of Int32.t
      | Int32Val of Int32.t
      | Int64Val of Int64.t
      | FloatVal of float
      | DoubleVal of float
      | StringLiteral of string
      | BoolVal of bool
      | CharVal of char
      | NullpointerVal of typ
      | ArrayVal of typ * value list
      | RecordVal of string * (string * value) list
      | ErrorVal of string
    exception CouldNotParseType of string
    val canonicType :
      ('a ->
       [< `Found of
            [> `Pointer of 'b | `Record of 'b recordType | `TypeRef of 'a ]
            as 'b
        | `NotFound ]) ->
      'b -> 'b
    val typeOf : value -> typ
    val recordDescr : ('a -> string) -> 'a recordType -> string
    val typeNameRec : (typ -> string) -> typ -> string
    val typeName : typ -> string
    val typeDescr : typ -> string
    val typeNameExplicit : typ -> string
    val valueString : value -> string
    val parseType : string -> typ
    val parseValue : typ -> string -> value
    val defaultValue : typ -> value
  end
module Tests :
  sig
    module TypesysTest : functor (Typesys : TYPE_SYSTEM) -> sig  end
    module TestZomp : sig  end
  end