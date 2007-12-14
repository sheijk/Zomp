open Printf
  
(* moved here from Zompvm because exception was not caught properly (compiler bug?) *)
exception FailedToEvaluateLLVMCode of string * string

let apply2nd f (fst, snd) = (fst, f snd)
let (<<=) f g = f g
let (>>=) x f = f x
let (++) f g x = f (g x)
let (|>) x f = f x

let rec combine seperator strings =
  match strings with
    | [] -> ""
    | strings ->
        let stringsLength = ref 0 in
        let measuredStrings = List.map
          (fun str ->
             let len = String.length str in
             stringsLength := !stringsLength + len;
             str, len)
          strings
        in
        let stringsLength = !stringsLength in
        let seperatorLength = String.length seperator in
        let totalLength = stringsLength + ((List.length strings - 1) * seperatorLength) in
        let buffer = String.make totalLength ' ' in
        let rec copyStrings strings startPos =
          match strings with
            | [] -> ()
            | (string, stringLength) :: rem ->
                let stringStartPos = startPos in
                let seperatorStartPos = startPos + stringLength in
                String.blit string 0 buffer stringStartPos stringLength;
                if seperatorStartPos < totalLength then
                  String.blit seperator 0 buffer seperatorStartPos seperatorLength;
                copyStrings rem (seperatorStartPos + seperatorLength)
        in
        copyStrings measuredStrings 0;
        buffer
      
(* let rec combine seperator = function *)
(*     [] -> "" *)
(*   | [str] -> str *)
(*   | hd :: tl -> hd ^ seperator ^ (combine seperator tl) *)
        
let rec translate errorF translators bindings expr =
  let rec t = function
    | [] -> errorF expr "Expression can not be translated"
    | f :: remf -> begin
        match f (translate errorF translators) bindings expr with
          | Some (newBindings, result) -> (newBindings, result)
          | None -> t remf
      end
  in
  t translators

let commentOut startDelim ?(stopDelim = "") multiLineSource =
  let rec combine seperator = function
      [] -> ""
    | [str] -> str
    | hd :: tl -> hd ^ seperator ^ (combine seperator tl)
  in
  let newlineRE = Str.regexp "\n" in
  let lines = Str.split newlineRE multiLineSource in
  let commentedLines = List.map (fun line -> startDelim ^ line ^ stopDelim) lines in
  combine "\n" commentedLines
  
let indent string =
  let indentLine line =
    let len = String.length line in
    if len >= 1 && line.[len-1] = ':' then line
    else "  " ^ line
  in
  let lines = Str.split (Str.regexp "\n") string in
  let indentedLines = List.map indentLine lines in
  combine "\n" indentedLines


let readChannel channel =
  let rec worker lines totalLength =
    try
      let newline = input_line channel in
      let lineLength = String.length newline in
      worker ((newline, lineLength) :: lines) (totalLength + lineLength + 1)
    with End_of_file -> lines, totalLength
  in
  let lines, totalLength = worker [] 0 in
  let fileContent = String.make totalLength ' ' in
  let rec copyLines lines endPos =
    match lines with
      | [] -> ()
      | (line, lineLength) :: rem ->
          let startPos = endPos - lineLength - 1 in
          String.blit line 0 fileContent startPos lineLength;
          fileContent.[endPos-1] <- '\n';
          copyLines rem startPos
  in
  copyLines lines totalLength;
  fileContent
    
let readFile fileName =
  let channel = open_in fileName in
  readChannel channel
    
let some x = Some x
let id x = x
  
let rec tryAll ~onSuccess ~ifAllFailed = function
  | [] -> Lazy.force ifAllFailed
  | f :: rem ->
      try
        onSuccess (Lazy.force f)
      with _ ->
        tryAll ~ifAllFailed ~onSuccess rem

let tryAllCollectingErrors ~onSuccess ~ifAllFailed funcs =
  let rec worker exceptions = function
    | [] -> ifAllFailed exceptions
    | f :: rem ->
        try
          onSuccess (Lazy.force f)
        with e ->
          worker (e :: exceptions) rem
  in
  worker [] funcs
          
let rec lastElement = function
  | [] -> None
  | [last] -> Some last
  | _ :: tail -> lastElement tail
      
let splitAfter firstLength list =
  let rec worker num accum = function
    | [] -> accum, []
    | hd :: tl ->
        if num < firstLength then
          worker (num+1) (hd::accum) tl
        else
          accum, hd::tl
  in
  let first, second = worker 0 [] list in
  List.rev first, second

let removeQuotes str =
  let length = String.length str in
  if length > 0 && str.[0] = '"' && str.[length-1] = '"' then begin
    let substring str ~first ~len =
      let newstr = String.create len in
      String.blit str first newstr 0 len;
      newstr
    in
    substring str 1 (String.length str - 2)
  end else
    str
  
let restrictToSingleprecision double =
  let array = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 1 in
  Bigarray.Array1.set array 0 double;
  Bigarray.Array1.get array 0




let dotimes count func =
  for i = 1 to count do func() done

    
type timingInfo = {
  name :string;
  mutable totalTime :float;
  mutable childs : timingInfo list
}

let makeTimingInfo name = {
  name = name;
  totalTime = 0.0;
  childs = [];
}

let findOrCreate parent name =
  try List.find (fun ti -> ti.name = name) parent.childs
  with Not_found ->
    begin
      let newTimingInfo = makeTimingInfo name in
      parent.childs <- newTimingInfo :: parent.childs;
      newTimingInfo      
    end
  
let toplevelTimingInfos = makeTimingInfo "toplevel"

let rec totalTime timingInfo =
  let childTimes = List.map totalTime timingInfo.childs in
  let totalChildTimes = List.fold_left (+.) 0.0 childTimes in
  totalChildTimes +. timingInfo.totalTime
  
let printTimings () =
  let rec worker prefix timingInfo =
    printf "%s%f - %s\n" prefix timingInfo.totalTime timingInfo.name;
    let sortedChilds =
      List.sort (fun ti1 ti2 -> 1 - compare ti1.totalTime ti2.totalTime) timingInfo.childs
    in
    List.iter (worker (prefix ^ "  ")) sortedChilds
  in
  worker "" toplevelTimingInfos

let timingStack = ref []

let parentTimingInfo() =
  match !timingStack with
    | top :: _ -> top
    | [] -> toplevelTimingInfos

let guarded f ~finally =
  try
    let result = f() in
    let () = finally() in
    result      
  with error ->
    let () = finally() in
    raise error
    
let collectTimingInfo name f =
  let currentTimingInfo = findOrCreate (parentTimingInfo()) name in
  timingStack := currentTimingInfo :: !timingStack;
  guarded (fun () ->
             let startTime = Sys.time() in
             let result = f() in
             let duration = Sys.time() -. startTime in
             currentTimingInfo.totalTime <- currentTimingInfo.totalTime +. duration;
             result)
    ~finally:(fun () ->
                timingStack := match !timingStack with
                  | _ :: rem -> rem
                  | [] -> []
             )

let sampleFunc1 name f arg0 = collectTimingInfo name (fun () -> f arg0)
let sampleFunc2 name f arg0 arg1 = collectTimingInfo name (fun () -> f arg0 arg1)
let sampleFunc3 name f arg0 arg1 arg2 = collectTimingInfo name (fun () -> f arg0 arg1 arg2)
        
  
(*   let previousTime = *)
(*     try Hashtbl.find timingInfos name *)
(*     with Not_found -> 0.0 *)
(*   in *)
(*   let startTime = Sys.time() in *)
(*   let result = f() in *)
(*   let duration = Sys.time() -. startTime in *)
(*   let totalTime = previousTime +. duration in *)
(*   Hashtbl.replace timingInfos name totalTime; *)
(*   result *)
  
(* let timingInfos : (string, float) Hashtbl.t = *)
(*   let tbl = Hashtbl.create 100 in *)
(*   tbl *)

(* let addDummyTimings() = *)
(*   List.iter (fun (name, time) -> Hashtbl.add timingInfos name time) *)
(*     ["foo", 1.23; *)
(*      "bar", 2.4; *)
(*      "long", 23.2; *)
(*      "short", 0.03] *)

(* let resetTimings() = Hashtbl.clear timingInfos *)

(* let listFromHashTbl tbl = *)
(*   Hashtbl.fold (fun name time list -> (name, time) :: list) tbl [] *)

(* let printTimings() = *)
(*   printf "--- Timings ---\n"; *)
(*   let samples = listFromHashTbl timingInfos in *)
(*   let sortedSamples = List.sort (fun (_, ltime) (_, rtime) -> 1 - compare ltime rtime) samples in *)
(*   let totalTime = *)
(*     List.fold_left *)
(*       (fun prevTime (_, thisTime) -> prevTime +. thisTime) *)
(*       0.0 sortedSamples *)
(*   in *)
(*   List.iter (fun (name, time) -> *)
(*                let percentage =  time /. totalTime *. 100.0 in *)
(*                printf "%f%% - %s (%fs)\n" percentage name time) *)
(*     sortedSamples; *)
(*   printf "---------------\n" *)
  
(* let totalTime name = *)
(*   try Hashtbl.find timingInfos name *)
(*   with Not_found -> -0.0 *)
    
(* let collectTimingInfo name f = *)
(*   let previousTime = *)
(*     try Hashtbl.find timingInfos name *)
(*     with Not_found -> 0.0 *)
(*   in *)
(*   let startTime = Sys.time() in *)
(*   let result = f() in *)
(*   let duration = Sys.time() -. startTime in *)
(*   let totalTime = previousTime +. duration in *)
(*   Hashtbl.replace timingInfos name totalTime; *)
(*   result *)
    

