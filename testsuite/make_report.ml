open Printf

let linkIfExists file title =
  if Sys.file_exists file then
    sprintf "<th><a href=\"../../%s\">%s</a></th>" file title
  else
    sprintf "<th> <p class=\"missing-file\" title=\"File %s does not exist\">%s</p></th>" file title

let inHtmlTag tag args f =
  printf "<%s %s>\n" tag args;
  f();
  printf "</%s>\n" tag

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
    sprintf "<span class=\"common-prefix\">%s</span>%s" commonPrefix str2Postfix

let readFile fileName =
  let ch = open_in fileName in
  let lines = ref [] in
  try
    while true do
      lines := input_line ch :: !lines
    done;
    ""
  with End_of_file ->
    String.concat "\n" (List.rev !lines)

let () =
  match Array.to_list Sys.argv with
    | program :: title :: files ->
      begin
        let title = Sys.argv.(1) in
        printf "<h1>%s</h1>" title;
        let succeededTests = ref 0 in
        let prevFile = ref "" in
        inHtmlTag "table" "class=\"test-results\""
          (fun () ->
            List.iter
              (fun fileName ->
                inHtmlTag "tr" "" (fun () ->
                  let title = deemphasizeCommonPrefix !prevFile fileName in
                  let linkToTest = linkIfExists (sprintf "%s.zomp" fileName) title in
                  print_string linkToTest;

                  if Sys.file_exists (fileName ^ ".result") then begin
                    let cssClass =
                      let resultClass =
                        if 0 = (Sys.command (sprintf "grep failed %s.result > /dev/null" fileName)) then
                          "failed"
                        else begin
                          incr succeededTests;
                          "ok"
                        end
                      in
                      if 0 = (Sys.command (sprintf "grep '!' %s.result > /dev/null" fileName)) then
                        resultClass ^ " changed"
                      else
                        resultClass
                    in

                    let resultFileContent = readFile (fileName ^ ".result") in
                    printf "<th class=\"%s\">%s</th>" cssClass resultFileContent;
                  end else begin
                    printf "<th class=\"failed\">not run</th>"
                  end;

                  print_string (linkIfExists (fileName ^ ".testreport") "report");
                  print_string (linkIfExists (fileName ^ ".test_output") "output");

                  prevFile := fileName))
              files);
        let totalTests = List.length files in
        printf "<p>%d/%d succeeded</p>\n" !succeededTests totalTests
      end
    | _ ->
      printf "%s: error: expected 'make_report title files*'" Sys.argv.(0);

