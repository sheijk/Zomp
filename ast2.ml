open Printf

type sexpr = {
  id :string;
  args :sexpr list;
}

let idExpr name = { id = name; args = [] }
let simpleExpr name args = { id = name; args = List.map (fun str -> { id = str; args = [] }) args }
let emptyExpr = { id = "seq"; args = [] }
let seqExpr args = { id = "seq"; args = args }

let testAst = {
  id = "main"; args = [
    { id = "foreach";
      args = [ idExpr "x"; idExpr "in"; idExpr "objects";
               { id = "seq"; args = [
                   simpleExpr "print" ["x"];
                   simpleExpr "print" ["hello"; "x"]] }
             ] };
    simpleExpr "echo" ["this"; "be"; "echo"];
    { id = "while";
      args = [simpleExpr "greater" ["x"; "100"];
              seqExpr [
                simpleExpr "print" ["x still greater"];
                simpleExpr "+=" ["x"; "1"];
                simpleExpr "register" ["manager"; "x"];
              ]]}
  ]}

let runtests printerF =
  let tests = [
    simpleExpr "print" ["x"; "y"];
    { id = "foreach"; args = [idExpr "x"; idExpr "in"; idExpr "lst"; seqExpr [] ]};
    { id = "func"; args = [idExpr "int"; idExpr "main"; seqExpr [];
                           seqExpr [
                             simpleExpr "print" ["x"];
                             simpleExpr "+=" ["x"; "1"];
                             simpleExpr "register" ["manager"; "x"];
                           ] ] };
    testAst;
  ]
  in
  List.iter (fun e -> printf "---\n%s.\n" (printerF e)) tests

let rec expression2string sexpr =
  let idString =
    if sexpr.id = "seq" && List.length sexpr.args = 0 then "()"
    else if String.length sexpr.id > 0 then sexpr.id
    else "/0/"
  in
  let (++) f g x = f (g x) in
  let rec classify sexprs =
    let longExpr params = `LongExpr params
    and noSeq params =
      let summedLengths = List.fold_left (fun len str -> len + String.length str) 0 in
      if summedLengths params > 60 then
        `LongExpr params
      else
        `NoSeq params
    and seqAtEnd seq params = `SeqAtEnd(params, seq)
    in
    let rec worker hadSequence acc = function
      | [] ->
          (if hadSequence then longExpr else noSeq), acc
      | [{ id = "seq"; args = seqArgs } as seq] when seqArgs <> [] ->
          (if hadSequence then longExpr, (seq::acc) else seqAtEnd seqArgs, acc)
      | { id = "seq" } as expr :: tail when expr.args <> [] ->
          worker true (expr :: acc) tail
      | hd :: tail ->
          let subExprHasSequence =
            match classify hd.args with
              | `NoSeq _ -> false
              | `SeqAtEnd(_,_) | `LongExpr _ -> true
          in
          worker (hadSequence or subExprHasSequence) (hd :: acc) tail
    in
    let f, params = worker false [] sexprs in
    let paramStrings = List.map expression2string params in
    f (List.rev paramStrings)
  in
  let inParens str = "(" ^ str ^ ")" in
  let simple2string idString paramStrings =
    Common.combine " " (idString :: paramStrings)
  in
  match classify sexpr.args with
    | `NoSeq params ->
        begin match params with
          | [] -> idString
          | _ -> inParens (simple2string idString params)
        end
    | `SeqAtEnd (params, seqArgs) ->
        inParens (
          simple2string idString params ^ " (\n"
          ^ Common.combine "\n" (List.map (Common.indent ++ expression2string) seqArgs) ^ " )")
    | `LongExpr childs ->
        "(" ^ Common.combine "\n" (idString :: List.map Common.indent childs) ^ " )"

let toString = expression2string

let rec equals l r =
  l.id = r.id
  && List.length l.args = List.length r.args
  && List.for_all2 equals l.args r.args
  
let rec replaceParams params args expr =
  let argCount = List.length args
  and paramCount = List.length params
  in
  if argCount <> paramCount then
    failwith (sprintf "Macro called with %d parameters, expected %d" paramCount argCount);
  let replacementList = List.combine params args in
  let replace name =
    try List.assoc name replacementList
    with Not_found -> { id = name; args = [] }
  in
  match replace expr.id with
    | { id = name; args = [] } -> { id = name; args = List.map (replaceParams params args) expr.args; }
    | _ as head -> { id = "seq"; args = head :: List.map (replaceParams params args) expr.args; }


let shiftId = function
  | {id = firstArgId; args = []} :: remArgs ->
      { id = firstArgId; args = remArgs }
  | _ ->
      failwith "shiftId"
      
