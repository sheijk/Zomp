open Printf

let deemphasizeCommonPrefix str1 str2 =
  let maxLength = min (String.length str1) (String.length str2) in
  let rec findFirstDifferentChar lastPathSepIndex index =
    if index >= maxLength then
      lastPathSepIndex
    else
      if str1.[index] = str2.[index] then
        findFirstDifferentChar
          (if str1.[index] = '/' then index + 1 else lastPathSepIndex)
          (index+1)
      else
        lastPathSepIndex
  in
  let firstDifferentChar = findFirstDifferentChar 0 0 in
  if (firstDifferentChar = 0) then
    str2
  else
    let commonPrefix = Str.string_before str2 firstDifferentChar in
    let str2Postfix = Str.string_after str2 firstDifferentChar in
      (* sprintf "%s | %s" commonPrefix str2Postfix *)
    sprintf "<span class=\"common-prefix\">%s</span>%s" commonPrefix str2Postfix

let test() =
  let cases = [
    "foo/bar/a", "foo/bar/b";
    "foo/bar/buzz", "foo/bar/blah";
    "a", "b";
    "", "right";
    "left", "";
  ] in
  printf "---\n";
  List.iter
    (fun (prev, next) ->
      let deemphNext = deemphasizeCommonPrefix prev next in
      printf "%s, %s => %s\n" prev next deemphNext)
    cases

let () =

  (* printf "%d args: %s\n" (Array.length Sys.argv) (String.concat "|" (Array.to_list Sys.argv)); *)
  (* flush stdout; *)
  (* () *)

  if (Array.length Sys.argv) = 3 then
    let str1 = Sys.argv.(1)
    and str2 = Sys.argv.(2) in
    printf "%s" (deemphasizeCommonPrefix str1 str2)
  else if (Array.length Sys.argv = 2) then
    printf "%s" Sys.argv.(1)
  else
    printf "too few arguments"
      

