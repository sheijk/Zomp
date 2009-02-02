open Str
open Printf

exception Type_not_found of string
exception Compile_error of string
  
module Utils =
struct
  (* the ocaml toplevel ignores prints newlines as \n, fix this *)
  let print_nice (f : Format.formatter) str =
    let lines = Str.split (Str.regexp "\n") str in
    match lines with
        [] -> ()
      | [single] -> Format.fprintf f "\"%s\"" single
      | _ ->
          Format.fprintf f "\"";
          List.iter (fun s -> Format.fprintf f "%s\n" s) lines;
          Format.fprintf f "\""

  (* #install_printer print_nice *)

  let test_re ~str ~re =
    let r = Str.regexp re in
    if Str.string_match r str 0 then (
      let rec printmatch n =
        if n >= 0 then (
          print_string (
            (string_of_int n) ^ ": <" ^
              (Str.matched_group n str) ^
              ">\n");
          printmatch (n + 1)
        ) else ()
      in (
        try
          printmatch 0
        with
            Not_found | Invalid_argument _ -> ()
      );
      true
    )
    else false

  let forEachFile ?(path = ".") ?(matchre = ".*") func =
    let re = Str.regexp matchre in
    Array.iter
      (fun dirname -> if (Str.string_match re dirname 0) then func dirname)
      (Sys.readdir path);;

  let isLastChar str chr =
    if String.length str <= 0 then false
    else String.get str ((String.length str) - 1) = chr

  let substring str ~first ~len =
    let newstr = String.create len in
    String.blit str first newstr 0 len;
    newstr

  let lastChar string =
    string.[String.length string - 1]
      
  let withoutLastChar string =
    substring string ~first:0 ~len:(String.length string - 1)

  let concat stringList delimeter =
    let rec conc = function
        [] -> ""
      | [last] -> last
      | head :: tail -> head ^ delimeter ^ (conc tail)
    in
    conc stringList

  let replaceFileNameExtension fileName newExt =
    let re = Str.regexp "\\([^.]*\\.\\).*" in
    if Str.string_match re fileName 0 then
      (Str.matched_group 1 fileName) ^ newExt
    else
      fileName

  let intToDecimal hexNumber constants =
    try
      let i64 = Int64.of_string hexNumber in
      Int64.to_string i64
    with _ ->
      (* some constant's values are names of other constants and
         thus cannot be converted to decimals *)
      let valueConstantName = String.uppercase hexNumber in
      begin try
        List.assoc valueConstantName !constants
      with Not_found ->
        raise (Compile_error (sprintf "Could not find constant '%s'" valueConstantName))
      end

end

open Utils

module BindingExpressions =
struct
  type constant = {
    name :string;
    value: string
  }

  type param = {
    pname : string option;
    ptype : string;
    mltype : string option
  }

  let untransformedParam name typ = {
    pname = name;
    ptype = typ;
    mltype = None
  }

  type func = {
    fname : string;
    retval : string;
    params : param list
  }

  type expr =
    | Include of string
    | Constant of constant
    | Function of func
    | Unknown of string

end

module type PARSER =
sig
  val parse : in_channel -> BindingExpressions.expr list
end

(**
 * Parses files in the simplistic format used by the libraries glew (http://glew.sourceforge.net)
 * with an extension to specify include files
 *)
module GlewParser : PARSER =
struct
  include BindingExpressions

  let splitparam str =
    let items = Str.split (Str.regexp " ") str in
    match items with
        [t] -> untransformedParam None t
      | ["const"; t] -> untransformedParam None ("const " ^ t)
      | [t; n] -> untransformedParam (Some n) t
      | ["const"; t; n] -> untransformedParam (Some n) ("const " ^ t)
      | _ -> untransformedParam None ""

  let extractParams str =
    let typeAndNames = Str.split (Str.regexp ", *") str in
    List.map splitparam typeAndNames

  let parseLine line =
    let constantRE = Str.regexp "[\t ]*\\([A-Za-z0-9_]+\\) +\\([A-Za-z0-9_]+\\) *$"
    and functionRE = Str.regexp "[\t ]*\\([a-zA-Z0-9_ ]+\\*?\\) \\([a-zA-Z0-9_]+\\) (\\([^)]*\\))"
    and includeRE = Str.regexp "[\t ]*include \\([\"<][a-zA-Z0-9_\\./]*[\">]\\)"
    in
    if Str.string_match includeRE line 0 then
      let filename = Str.matched_group 1 line in
      Include filename
    else if Str.string_match constantRE line 0 then
      Constant {
        name = (Str.matched_group 1 line);
        value = (String.lowercase (Str.matched_group 2 line))
      }
    else if Str.string_match functionRE line 0 then
      let name = Str.matched_group 2 line
      and retval = Str.matched_group 1 line
      and params = Str.matched_group 3 line
      in
      Function {
        fname = name;
        retval = retval;
        params = extractParams params
      }
    else
      Unknown line

  let parse channel =
    let rec readline () =
      let line = input_line channel in
      let expr = parseLine line in
      begin match expr with
          (* extra exit for interactive usage *)
          Unknown str when str = "!" -> []
        | _ -> begin
            try
              expr :: readline ()
            with
                End_of_file -> []
              | Type_not_found typeName ->
                  let message = Printf.sprintf
                    "could not find type %s while processing line %s"
                    typeName
                    line
                  in
                  raise (Compile_error message)
          end
      end (* match *)
    in
    readline()
end

module Printer =
struct
  open BindingExpressions

  let constant2string c = Printf.sprintf
    "Contant name='%s' value='%s'"
    c.name
    c.value

  let param2string p =
    let optval = function
        None -> "none"
      | Some str -> str
    in
    Printf.sprintf
      "Param pname='%s' ptype='%s' mltype='%s'"
      (optval p.pname)
      p.ptype
      (optval p.mltype)

  let func2string f =
    Printf.sprintf
      "Function fname='%s' retval='%s' %d params"
      f.fname
      f.retval
      (List.length f.params)

  let expr2string = function
    | Include filename -> "#include \"" ^ filename ^ "\""
    | Constant c -> constant2string c
    | Function f -> func2string f
    | Unknown str -> "Unknown: " ^ str
end

module CamlTypemapper =
struct

  class type paramType = object
    method name : string
    method mltype : string
    method ctype : string
    method value2ctype : string
    method ctype2val : string
    method isArray : bool
  end

  class paramTypeGen
    ~(name :string)
    ~(mltype :string)
    ~(ctype :string)
    ?(value2ctype = (String.capitalize mltype) ^ "_val")
    ?(ctype2val = "Val_" ^ mltype)
    () =
  object
    method name = name
    method mltype = mltype
    method ctype = ctype
    method value2ctype = value2ctype
    method ctype2val = ctype2val
    method isArray = false
  end

  class paramTypeUntransformed ~(name : string) = object
    inherit paramTypeGen name name name ()
  end

  class paramArrayType ~(baseType : paramType) = object
    method name = baseType#name ^ "*"
    method mltype = "(" ^ baseType#mltype ^ " array)"
    method ctype = baseType#ctype ^ "*"
    method value2ctype = baseType#value2ctype
    method ctype2val = baseType#ctype2val
    method isArray = true
  end

  let types =
    let tbl = Hashtbl.create 32 in
    let voidType = (new paramTypeGen
                      ~name:"void"
                      ~ctype:"void"
                      ~mltype:"unit" () :> paramType)
    in
    Hashtbl.add tbl "void" voidType;
    Hashtbl.add tbl "GLvoid" voidType;
    tbl

  let findType ~name =
    try
      Hashtbl.find types name
    with
        Not_found -> raise (Type_not_found name)

  let printTypes () =
    let print x y =
      Format.printf "%s " x;
      if y#name <> y#mltype then
        Format.printf "ml: %s " y#mltype;
      if y#name <> y#ctype then
        Format.printf "c: %s " y#ctype;
      Format.printf "\n"
    in
    Hashtbl.iter print types

  let addType ~name
      ?(mltype = name)
      ?(ctype = name)
      ?(value2ctype)
      ?(ctype2val)
      () =
    let t =
      new paramTypeGen ~name ~mltype ~ctype ?value2ctype ?ctype2val ()
    in
    let ta = new paramArrayType (t :> paramType) in
    let tc = new paramTypeGen
      ~name:("const " ^ name)
      ~mltype ~ctype
      ?value2ctype ?ctype2val ()
    in
    let tac = new paramArrayType (tc :> paramType) in
    Hashtbl.add types name (t :> paramType);
    Hashtbl.add types ta#name (ta :> paramType);
    Hashtbl.add types tac#name (tac :> paramType)

  let () =
    addType ~name:"GLenum" ~mltype:"int"
      ~ctype2val:"Val_long" ~value2ctype:"Long_val" ();
    addType ~name:"GLbitfield" ~mltype:"int"
      ~ctype2val:"Val_long" ~value2ctype:"Long_val" ();
    addType ~name:"const char*" ~mltype:"string"
      ~ctype2val:"caml_copy_string" ~value2ctype:"String_val" ();
    addType ~name:"const GLchar*" ~mltype:"string"
      ~ctype2val:"caml_copy_string" ~value2ctype:"String_val" ();
    addType ~name:"GLbyte" ~mltype:"int" ();
    addType ~name:"GLubyte" ~mltype:"int" ();
    addType ~name:"GLshort" ~mltype:"int" ();
    addType ~name:"GLushort" ~mltype:"int" ();
    addType ~name:"float" ();
    addType ~name:"double" ();
    addType ~name:"int" ();
    addType ~name:"GLfloat" ~mltype:"float" ();
    addType ~name:"GLdouble" ~mltype:"double" ();
    addType ~name:"GLclampf" ~mltype:"float" ();
    addType ~name:"GLclampd" ~mltype:"float" ();
    addType ~name:"GLint" ~mltype:"int" ();
    addType ~name:"GLuint" ~mltype:"int" ();
    addType ~name:"GLboolean" ~mltype:"int" ();
    addType ~name:"bool" ~mltype:"bool" ();
    addType ~name:"GLsizei" ~mltype:"int" ()
end

(**
 * Generates glue code from expressions. Glue code consists of two parts: C code and
 * target language code. The C code will be linked into the native library while
 * the target language code specifies the interface in the target language
 *)
module type CODEGEN =
sig
  val transform : BindingExpressions.expr list -> BindingExpressions.expr list

  val generate_c_code : BindingExpressions.expr list -> string
  val generate_lang_code : BindingExpressions.expr list -> string

  val lang_extension : string
end

module Processor(Parser : PARSER)(Codegen : CODEGEN) =
struct
  let process_file moduleName =
    let fileName = moduleName ^ ".skel" in
    let writefile ~filename ~text =
      let file = open_out filename in
      output_string file text;
      close_out file
    in
    let testfile = open_in fileName in
    let content = Codegen.transform ( Parser.parse testfile ) in
    close_in testfile;
    let langCode = Codegen.generate_lang_code content
    and ccode = Codegen.generate_c_code content in
    if String.length langCode > 0 then
      writefile ~filename:(moduleName ^ "." ^ Codegen.lang_extension) ~text:langCode;
    if String.length ccode > 0 then
      writefile ~filename:(moduleName ^ ".c") ~text:ccode
end

(**
 * Generates glue code for OCaml
 *)
module Camlcodegen : CODEGEN =
struct
  open BindingExpressions
  open CamlTypemapper

  let ml_constant_name c_name = String.lowercase c_name

  let gencaml_constant constant =
    "let " ^ (ml_constant_name constant.name) ^ " = " ^ constant.value

  let cstub_name func = "ml_" ^ func.fname

  let cstub_name_byte func = "byte_" ^ (cstub_name func)

  exception Internal_error

  let mltype_sig paramList =
    let mltyp (p : param) = match p.mltype with
        Some t -> t
      | _ -> raise Internal_error
    in
    let nametag (p : param) = match p.pname with
        None -> mltyp p
      | Some name -> name ^ ":" ^ (mltyp p)
    in
    match paramList with
        [] -> "unit -> "
      | _ -> (concat (List.map (fun p -> nametag p) paramList) " -> ") ^ " -> "

  let mltype2string retval = (findType retval)#mltype

  let gencaml_function func =
    let paramCount = List.length func.params in
    "external " ^ func.fname
    ^ " : " ^ (mltype_sig func.params) ^ (mltype2string func.retval)
    ^ " = "
    ^ (if paramCount > 5 then "\"" ^ (cstub_name_byte func) ^ "\" " else "")
    ^ "\"" ^ (cstub_name func) ^ "\""

  let gencaml_unknown str = "(* " ^ str ^ " *)"

  let gen_caml_code expressions =
    let helpers =
      "type double = float\n" ^
        "\n"
    in
    let expr2str = function
      | Include filename -> "(* included " ^ filename ^ " *)"
      | Constant c -> gencaml_constant c
      | Function f -> gencaml_function f
      | Unknown u -> gencaml_unknown u
    in
    let stringList =
      List.map (fun e -> expr2str e) expressions (*TODO: remove fun e.. *)
    in
    helpers ^
      (concat stringList "\n")

  let generate_lang_code = gen_caml_code

  let c_retval = function
      "void" -> "void"
    | _ -> "value"

  let c_params2str paramList =
    let p2str param =
      let name = function
          None -> ""
        | Some str -> str
      in
      "value " ^ (name param.pname)
    in
    let decllist = List.map p2str paramList in
    concat decllist ", "

  let named_params paramList =
    let argnum = ref 0 in
    let named_param p = match p with
        { pname = None; ptype = t; mltype = mln } ->
          argnum := !argnum + 1;
          {
            pname = Some ("arg" ^ (string_of_int !argnum));
            ptype = t;
            mltype = mln
          }
      | { pname = Some _; ptype = _; mltype = _ } -> p
    in
    List.map named_param paramList


  let param_name p = match p.pname with
      Some name -> name
    | None -> raise Internal_error

  let local_var_name paramName = paramName ^ "_c"

  let value2ctype t = (findType t)#value2ctype
  let ctype2val t = (findType t)#ctype2val

  exception FloatArraysNotSupported

  let make_local_cvar param =
    let value () =
      let create =
        param.ptype ^ " "
        ^ (local_var_name (param_name param))
        ^ " = " ^ (value2ctype param.ptype) ^ "(" ^ (param_name param) ^ ");\n"
      and cleanup = ""
      in
      (create, cleanup)
    and array () =
      let t = findType param.ptype in
      let baseType = substring t#ctype 0 ((String.length t#ctype)-1) in
      let isFloat = (baseType = "float") || (baseType = "GLfloat") in
      let paramName = param_name param in
      let localName = local_var_name paramName in
      let val2t = (findType baseType)#value2ctype
      and t2val = (findType baseType)#ctype2val in
      let create =
        Printf.sprintf "std::vector<%s> %sv;\n" baseType paramName
        ^ Printf.sprintf "int %ssize = Wosize_val(%s);\n" paramName paramName
        ^ Printf.sprintf "%sv.reserve(%ssize);\n" paramName paramName
        ^ Printf.sprintf "for(int i = 0; i < %ssize; ++i) {\n" paramName
        ^ (if isFloat then
             Printf.sprintf
               "  %sv.push_back( Double_field(%s, i) );\n"
               paramName paramName
           else
             Printf.sprintf "  value v = Field(%s, i);\n" paramName
             ^ Printf.sprintf "  %sv.push_back( %s(v) );\n" paramName val2t
          )
        ^ Printf.sprintf "}\n"
        ^ Printf.sprintf "%s* %s = &%sv[0];\n" baseType localName paramName
      and cleanup =
        Printf.sprintf "for(int i = 0; i < %ssize; ++i) {\n" paramName
        ^ (
          if isFloat then
            Printf.sprintf "\tStore_double_field(%s, i, double(%sv[i]));\n"
              paramName paramName
          else
            Printf.sprintf "\tvalue v = %s(%sv[i]);\n" t2val paramName
            ^ Printf.sprintf "\tStore_field(%s, i, v);\n" paramName
        )
        ^ Printf.sprintf "}\n"
      in
      (create, cleanup)
    in
    if (findType param.ptype)#isArray then array()
    else value()

  let isCheckedFunction funcName =
    let nonChecked = [
      "glVertex.*";
      "glColor.*";
      "glIndex.*";
      "glNormal.*";
      "glTexCoord.*";
      "glEvalCoord.*";
      "glEvalPoint.*";
      "glMaterial.*";
      "glEdgeFlag.*";
      "glCallList.*";
      "glBegin"
    ] in
    (Str.string_match (Str.regexp "gl[A-Z].*") funcName 0)
    & not
      (List.fold_left
         (fun l r -> l or (Str.string_match (Str.regexp r) funcName 0))
         false
         nonChecked)


  let genc_function func =
    let namedParams = named_params func.params in
    let funccall =
      func.fname ^ "("
      ^ (concat (List.map (fun p -> (local_var_name (param_name p))) namedParams) ", ")
      ^ ")"
    in
    let argvList count =
      let lst ?(start = 0) count =
        let argn n = "argv[" ^ (string_of_int n) ^ "]" in
        let rec helper n =
          if n >= count then []
          else (argn n) :: helper (n+1)
        in
        helper start
      in
      concat (lst count) ", "
    in
    let wrapperFunction =
      let signatureLine =
        (c_retval func.retval)
        ^ " "
        ^ (cstub_name func)
        ^ "(" ^ (c_params2str namedParams) ^ ")\n"
      and (localVarCreation, localVarCleanup) =
        let combine (createList, cleanupList) (create, cleanup) =
          (createList ^ "" ^ create, cleanupList ^ "" ^ cleanup)
        in
        let localVars = List.map make_local_cvar namedParams in
        List.fold_left combine ("", "") localVars
      and (return, functionCall) =
        if func.retval = "void" then
          ("", funccall ^ ";\n")
        else
          (
            "return retval;\n",
            "value retval = "
            ^ (ctype2val func.retval)
            ^ "((" ^ (func.retval) ^ ")"
            ^ funccall ^ ");\n"
          )
      and errorCheck =
        if isCheckedFunction func.fname then
          Printf.sprintf
            "\ncheckAndReportGLErrors(\"%s\");\n\n"
            func.fname
        else "\n/* function may be called between glBegin/glEnd, thus no error check */\n\n"
      in
      signatureLine
      ^ "{\n"
      ^ localVarCreation
      ^ "\n"
      ^ functionCall
      ^ "\n"
      ^ localVarCleanup
      ^ errorCheck
      ^ return
      ^ "}\n"
    and bytecodeWrapperFunction =
      let paramCount = List.length func.params in
      if paramCount > 5 then begin
        (c_retval func.retval) ^ " " ^ (cstub_name_byte func)
        ^ "(value* argv, int argn)\n"
        ^ "{\n"
        ^ "\t"
        ^ (if func.retval <> "void" then "return " else "")
        ^ (cstub_name func) ^ "(" ^ (argvList paramCount) ^ ");\n"
        ^ "}\n\n"
      end
      else ""
    in
    wrapperFunction ^ "\n\n" ^ bytecodeWrapperFunction

  let gen_c_code expressions =
    let expr2code = function
      | Function f -> (genc_function f) ^ "\n"
      | Include filename -> "#include " ^ filename ^ "\n"
      | _ -> ""
    in
    let snippets = List.map expr2code expressions in
    (concat snippets "")

  let generate_c_code = gen_c_code

  let transformParam (p : param) =
    {
      pname = p.pname;
      ptype = p.ptype;
      mltype = Some (findType p.ptype)#mltype
    }

  let transformExpr (expr : expr) : expr =
    match expr with
        Function f -> Function {
          fname = f.fname;
          retval = f.retval;
          params = List.map transformParam f.params
        }
      | _ -> expr

  let transform (expressions : expr list) : expr list =
    let rec transformExprList = function
        [] -> []
      | expr::rem -> begin
          try
            (transformExpr expr) :: (transformExprList rem)
          with
              Type_not_found t ->
                let msg = Printf.sprintf
                  "Type '%s' not found in expression %s"
                  t
                  (Printer.expr2string expr)
                in
                raise (Compile_error msg)
        end
    in
    transformExprList expressions

  let lang_extension = "ml"
end

module UniqueId =
struct
  let counter = ref 0

  let nextInt () = incr counter; !counter
  let nextString name = incr counter; sprintf "%s%d" name !counter
end

module type ZOMP_CODE_PRINTER =
sig
  val langExtension : string
  val printHeader : unit -> string
  val printError : decl:string -> msg:string -> string
  val printConstant : name:string -> typ:string -> default:string -> string
  val printFunction : name:string -> rettype:string ->
    params:([`Name of string] * [`Type of string]) list -> string
  val printUnknown : text:string -> string
  val printTypedef : name:string -> typ:string -> string
end
  
module ZompSExprPrinter : ZOMP_CODE_PRINTER =
struct
  let langExtension = "szomp"
    
  let printHeader () =
    "/// zomp definitions\n\n"

  let printError ~decl ~msg =
    sprintf "// %s\n// failed: %s\n" decl msg
    
  let printConstant ~name ~typ ~default =
    sprintf "(const %s %s %s)" typ name default

  let printUnknown ~text =
    if String.length text > 0 then 
      sprintf "// %s\n" text
    else
      "\n"

  let printFunction ~name ~rettype ~params =
    let paramStrings =
      List.map (fun (`Name name, `Type typ) -> sprintf "(%s %s)" typ name) params
    in
    sprintf "(func %s %s (%s))"
      rettype name (Utils.concat paramStrings " ")

  let printTypedef ~name ~typ = sprintf "(type %s %s)" name typ
   
end

module ZompIndentPrinter : ZOMP_CODE_PRINTER =
struct
  let langExtension = "zomp"
  let printHeader () = ""
  let printError ~decl ~msg = ""
  let printConstant ~name ~typ ~default =
    sprintf "const %s %s %s" typ name default

  let printFunction ~name ~rettype ~params =
    let paramStrings =
      Utils.concat
        (List.map (fun (`Name name, `Type typ) -> sprintf "%s %s" typ name) params)
        ", "
    in
    sprintf "func %s %s(%s)" rettype name paramStrings

  let printUnknown ~text = ""

  let printTypedef ~name ~typ = sprintf "type %s %s" name typ
end
    
module ZompCodegen(CodePrinter : ZOMP_CODE_PRINTER) : CODEGEN =
struct
  let transform (exprs :BindingExpressions.expr list) = exprs
  let generate_c_code (_ :BindingExpressions.expr list) = ""

  open BindingExpressions

  let supportedTypes =
    List.map (fun (name, typ) -> (`Name name, `Type typ))
      [
        "GLenum", "int";
        "GLuint", "int";
        "GLbitfield", "int";
        (* previous types should be unsigned! *)
        "GLint", "int";
        "GLboolean", "bool";
        "GLfloat", "float";
        "GLdouble", "double";
        "GLclampf", "float";
        "GLsizei", "int";
        "GLchar", "char";
        (* native c/zomp types *)
        "void", "";
        "int", "";
        "float", "";
        "double", "";
        "bool", "";
        "char", "";
      ]

  exception TypeNotSupported of string
    
  let rec isSupportedPrimitiveType name =
    try
      ignore( List.assoc (`Name name) supportedTypes );
      true
    with _ ->
      false

  open Utils
  
  let rec zompTypename cTypeName =
    let cTypeName = Str.global_replace (Str.regexp "const *") "" cTypeName in
    let trim str =
      let str = Str.global_replace (Str.regexp "^ +") "" str in
      str
    in
    let cTypeName = trim cTypeName in
    if lastChar cTypeName = '*' then
      match zompTypename (withoutLastChar cTypeName) with
        | Some zompTypeName -> Some (sprintf "(ptr %s)" zompTypeName)
        | None -> None
    else if cTypeName = "GLvoid" then
      Some "void"
    else if isSupportedPrimitiveType cTypeName then
      Some cTypeName
    else
      None
          
  let previousConstants = ref []
    
  let generate_zomp_code = function
    | Include fileName -> sprintf "/// include %s\n" fileName
    | Constant c ->
        let value = intToDecimal c.value previousConstants in
        previousConstants := (c.name, value) :: !previousConstants;
        CodePrinter.printConstant ~typ:"GLint" ~name:c.name ~default:value
    | Function f ->
        begin
          let error = ref None in
          let translateType ctype =
            match zompTypename ctype with
              | Some zompType -> zompType
              | None ->
                  let newMessage = (sprintf "type %s not supported" ctype) in
                  error := (match !error with
                              | None -> Some newMessage
                              | Some oldMessage -> Some (oldMessage ^ ", " ^ newMessage));
                  ctype
          in
          let params =
            let toString param =
              let zompParamType = translateType param.ptype in
              `Name (match param.pname with
                       | Some name -> name
                       | None -> UniqueId.nextString "arg"),
              `Type zompParamType
            in
            List.map toString f.params
          in
          let zompRetType = translateType f.retval in
          let declaration = CodePrinter.printFunction
            ~rettype:zompRetType ~name:f.fname ~params
          in
          match !error with
            | None -> declaration
            | Some errorMessage ->
                CodePrinter.printError ~decl:declaration ~msg:errorMessage
        end
    | Unknown text ->
        CodePrinter.printUnknown ~text

  let generate_lang_code (exprs :BindingExpressions.expr list) =
    let lines = List.map generate_zomp_code exprs in
    let typeDecls =
      Utils.concat (List.map
                      (fun (`Name name, `Type typ) ->
                         if String.length typ > 0 then
                           CodePrinter.printTypedef ~name ~typ
                         else "")
                      supportedTypes) "\n"
    in
    CodePrinter.printHeader() ^
      typeDecls ^
      Utils.concat lines "\n"

  let lang_extension = CodePrinter.langExtension
end


module CamlbindingsGen = Processor(GlewParser)(Camlcodegen)
module ZompbindingsSExprGen = Processor(GlewParser)(ZompCodegen(ZompSExprPrinter))
module ZompbindingsIndentGen = Processor(GlewParser)(ZompCodegen(ZompIndentPrinter))
  
let errorInvalidLanguage = 1
and errorInvalidParams = 2
and errorSystemError = 3
and errorCompilerError = 4

let _ =
  let processFileF, moduleName =
    match Sys.argv with
      | [| _; moduleName |] ->
          CamlbindingsGen.process_file, moduleName
      | [| _; "-lang"; "ml"; moduleName |] ->
          CamlbindingsGen.process_file, moduleName
      | [| _; "-lang"; "szomp"; moduleName |] ->
          ZompbindingsSExprGen.process_file, moduleName
      | [| _; "-lang"; "zomp"; moduleName |] ->
          ZompbindingsIndentGen.process_file, moduleName
      | [| _; "-lang"; invalidLanguage; _ |] ->
          eprintf "Language %s is not supported. Try ml or zomp\n" invalidLanguage;
          exit errorInvalidLanguage;
      | _ ->
          eprintf "Invalid arguments. 'gencode -lang lang foo' will generate bindings for module foo and language lang\n";
          exit errorInvalidParams
  in
  try
    processFileF moduleName
  with
    | Invalid_argument msg
    | Sys_error msg ->
        begin
          printf "System error: %s\n" msg;
          exit 1;
        end
    | Compile_error command ->
        begin
          print_string ("Generating bindings failed: " ^ command);
          exit 2;
        end

