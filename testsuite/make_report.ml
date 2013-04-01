open Printf

let linkIfExists file title =
  if Sys.file_exists file then
    sprintf "<a href=\"../../%s\">%s</a>" file title
  else
    sprintf "<span class=\"missing-file\" title=\"File %s does not exist\">%s</span>" file title

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

let produceReport title files =
  let succeededTests = ref 0 in
  let prevFile = ref "" in
  let testResultRows =
    List.map
      (fun fileName ->
        let title = deemphasizeCommonPrefix !prevFile fileName in
        let sourceLink = linkIfExists (sprintf "%s.zomp" fileName) title in
        
        let resultInfo =
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
            sprintf "<th class=\"%s\">%s</th>" cssClass resultFileContent;
          end else begin
            sprintf "<th class=\"failed\">not run</th>"
          end
        in
        
        let reportLink = linkIfExists (fileName ^ ".testreport") "report"
        and outputLink = linkIfExists (fileName ^ ".test_output") "output"
        in
        
        prevFile := fileName;
        
        [sprintf "<th>%s</th>\n" sourceLink;
         sprintf "%s\n" resultInfo;
         sprintf "<th>%s</th>\n" reportLink;
         sprintf "<th>%s</th>\n" outputLink])
      files
  in
  
  printf "<h1>%s</h1>\n" title;
  print_string "<table class=\"test-results\">\n";
  let printReportLine reportRows =
    print_string "  <tr>\n";
    List.iter (printf "    %s") reportRows;
    print_string "  </tr>\n"
  in
  List.iter printReportLine testResultRows;
  print_string "</table>\n";
  
  let totalTests = List.length files in
  printf "<p>%d/%d succeeded</p>\n" !succeededTests totalTests
    
let () =
  match Array.to_list Sys.argv with
    | _ :: title :: files ->
      produceReport title files
    | _ ->
      printf "%s: error: expected 'make_report title files*'" Sys.argv.(0);

