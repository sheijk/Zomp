module Errors =
struct
  (** public abstract *)
  type 'severity t = {
    message :string;
    loc :Ast2.location option;
    severity : 'severity;
  }

  (** public *)
  let severity e = e.severity
  let message e = e.message
  let loc e = e.loc

  (** public *)
  type errorSeverities = [`Fatal | `Error]
  type warningSeverities = [`Warning | `Info]
  type allSeverities = [errorSeverities | warningSeverities]

  let makeDiagnostic severity loc message = {
    message = message;
    loc = Some loc;
    severity = severity
  }

  let makeDiagnosticNoLoc severity message = {
    message = message;
    loc = None;
    severity = severity
  }

  (** public *)
  let makeError = makeDiagnostic `Error
  let makeErrorNoLoc = makeDiagnosticNoLoc `Error

  let makeWarning = makeDiagnostic `Warning
  let makeWarningNoLoc = makeDiagnosticNoLoc `Warning

  let makeInfo = makeDiagnostic `Info
  let makeInfoNoLoc = makeDiagnosticNoLoc `Info
end

type environment = {
  bindings :Bindings.t;

  parse :string -> Ast2.t list mayfail;
  translate :formWithTLsEmbedded translatorFunction;
  translateTL :toplevelExpr translatorFunction;

  handleForm :toplevelExpr -> unit;

  includePath : string list;
}
and 'forms result = {
  env :environment;
  warnings :Errors.warningSeverities Errors.t list;
  forms :'forms mayfail
}
and 'forms translatorFunction = environment -> Ast2.sexpr -> 'forms result

val withIncludeDir : environment -> string -> environment

module Imperative =
struct
  type 'my_expr_t env {
    eval : string -> unit;

    parse :string -> env -> Ast2.t list mayfail;
    translateTL : env -> Ast2.t -> toplevelExpr list
  }
end

module Functional =
struct
  type ('my_context_t, 'my_in_expr_t, 'my_out_expr_t) env {
    produce : string -> 'my_expr_t list mayfail;

    translate_step : 'my_context_t -> 'my_in_expr_t -> 'my_context_t * 'my_out_expr_t;
  }

  type 't result = {
    result :'t list;
    errors :error list;
    warnings :warning list;
  }

  module type ExpansionStep =
  sig
    type env
    type input
    type output

    val translate : env -> input -> env * (output result)
  end

  module CodegenLlvm =
  struct
    type env = unit
    type input = toplevelForm list
    type output = LlvmModule
  end

  module MacroExpander =
  struct
    type env = { ... }
    type input = Ast2.t list
    type output = toplevelForm list
  end

  let translateFull (source :string) : LlvmModule =
    let abortOnErrors result =
      if not List.empty exprResult.errors then begin
        reportErrorsAndWarnings exprResult;
        exit 1
      end
    in

    let exprResult = parse source in
    abortOnErrors exprResult;
    let formResult = MacroExpander.translate MacroExpander.defaultEnv exprResult.result in
    abortOnErrors formResult;
    let llvmResult = CodegenLlvm.translate CodegenLlvm.defaultEnv formResult.result in
    abortOnErrors llvmResult;
    writeLlvmCode llvmResult.result fileName
end

module Ast3 =
struct
  (** immutable data, represented as index into interned ast table *)
  type t = {
    id : Int32.t
  }

  (** native:

      struct Ast {
        uint32 nameId;
        uint32 childCount;
        Ast* childs;
        uint32 propCount;
        Ast* props;
      };

      buffer containing

      uint32 nameId - index into string table
      uint32 childCount
      uint32 propCount
      uint32[childCount] childIds
      uint32[propCount] propIds
  *)

  let name ast = ...
  let childCount ast = ...
  let child ast n : t = ...
  let propCount ast = ...
  let prop ast n : t = ...

  let idExpr name = ...
  let expr name (childs:t list) = ...
  let withProps ast (props: t list) = ...

  let equals l r = ...
end

module type MacroExpander_semi_functional =
sig
  type nestedResult {
    expr :form;
  }

  type result {
    elements :toplevelForms list;
  }

  (** TODO allow to invoke toplevel translation while translating another one *)

  type env = {
    mutable bindings :Bindings.t;
    mutable warnings :warning list;
    mutable errors :error list;
    mutable statements :form list;
    mutable includePaths :string list;

    (** pull through previous pipelines *)
    parse :string -> Ast2.t list mayfail;

    (** send to next stage in pipeline *)
    evalExpr :toplevelExpr -> unit;

    (** call self recursively *)
    translate : env -> string -> result;
    translateNested : env -> Ast2.id -> nestedResult;
  }

  val make_env : (toplevelExpr -> unit) -> env
  val add_bindings : env -> (string * Bindings.symbol) list -> unit

  val translate : env -> string -> unit

  (** moves errors, warnings etc. *)
  val mergeToFirst : env -> env -> unit
end

(** Support translation as a set of dynamically configurable steps. Can be used
to combine various intermediate languages and backends *)
module DynamicTranslatorQueue =
struct
  (** how should "forking" the current compilation to insert something before the
      current toplevel expression work?
      - create new context, use it? means telling data attached to context to clone
      - add definition, but delay it. might prevent macro expansion being done at toplevel

      should all translation steps be semi-functional? e.g. produce data
      structure for current toplevel expression without side-effects and only
      "commit" after toplevel expression is complete?

      or should they just be required to be "re-entrant" (e.g. support
      translating multiple expressions concurrently?)

      maybe just let each translation step produce one or more complete toplevel
      expressions and then pass it to the next step instead of interleaving the
      invocation of multiple steps during translation of an expression
  *)
  type zompContext = {
    native :ZompVM.contextPtr;
    proto :zompContext option;
  }

  let clone ctx : ctx = {
    native = ZompVM.clone_context ctx.native;
    proto = Some ctx
  }

  let get_macro_env ctx : MacroExpander.env = ...
  let get_llvm_env ctx : CodegenLlvm.env = ...

  type result = {
    exprs :Ast2.t list;
    errors :error list;
    warnings :warning list;
  }

  let translate translators ctx exprs =
    match translators with
      | [] -> [], []
      | translator :: remTranslators ->
          begin
            let exprResult = translator.apply ctx exprs in
            let errors, warnings =
              if not empty exprResult.exprs then begin
                let nextStepResult = translate remTranslators exprResult.exprs in
                combineDiagnostics exprResult nextStepResult
              else
                exprResult.errors, exprResult.warnings
            in
            (if not empty errors then
              translator.cleanupFailure();
            else
              translator.cleanupSuccess());
            errors, warnings
          end
end


  (* type 'translateF env = { *)
(*   bindings :Bindings.t; *)
(*   translateF :'translateF; *)
(*   parseF :string -> Ast2.t list option; *)
(* } *)

(* type toplevelEnv = toplevelExprTranslateF env *)

(* let translateNewTL (env : toplevelEnv) (asts : Ast2.sexpr) : toplevelTranslationResult = *)
(*   Error [] *)
