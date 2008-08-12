open Printf

(* moved here from Zompvm because exception was not caught properly (compiler bug?) *)
exception FailedToEvaluateLLVMCode of string * string

let apply2nd f (fst, snd) = (fst, f snd)
let (<<=) f g = f g
let (>>=) x f = f x
let (++) f g x = f (g x)
let (|>) x f = f x
let (=~) str re = Str.string_match (Str.regexp re) str 0

let rec fromTo min max =
  if min > max then []
  else min :: fromTo (min+1) max

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

let pushTimingContext name =
  let currentTimingInfo = findOrCreate (parentTimingInfo()) name in
  timingStack := currentTimingInfo :: !timingStack;
  let startTime = Sys.time() in
  currentTimingInfo, startTime

let popTimingContext (currentTimingInfo, startTime) =
  let duration = Sys.time() -. startTime in
  currentTimingInfo.totalTime <- currentTimingInfo.totalTime +. duration;
  timingStack := match !timingStack with
    | _ :: rem -> rem
    | [] -> []

let collectTimingInfo name f =
  let currentTimingInfo, startTime = pushTimingContext name in
  guarded (fun () ->
             let result = f() in
             popTimingContext (currentTimingInfo, startTime);
             result)
    ~finally:(fun () -> popTimingContext (currentTimingInfo, startTime) )

let sampleFunc1 name f arg0 = collectTimingInfo name (fun () -> f arg0)
let sampleFunc2 name f arg0 arg1 = collectTimingInfo name (fun () -> f arg0 arg1)
let sampleFunc3 name f arg0 arg1 arg2 = collectTimingInfo name (fun () -> f arg0 arg1 arg2)

let listIteri f list =
  let rec iterWithIndex index = function
    | [] -> ()
    | hd :: tl ->
        let () = f index hd in
        iterWithIndex (index+1) tl
  in
  iterWithIndex 0 list

let listMap2i f list1 list2 =
  let rec mapWithIndex index acc list1 list2 =
    match list1, list2 with
      | [], [] -> List.rev acc
      | (head1::tail1), (head2::tail2) ->
          let mapped = f index head1 head2 in
          mapWithIndex (index+1) (mapped :: acc) tail1 tail2
      | _, _ ->
          raise (Invalid_argument "listMap2i")
  in
  mapWithIndex 0 [] list1 list2

let listFold2i f initial list1 list2 =
  let rec foldWithIndex index value list1 list2 =
    match list1, list2 with
      | [], [] -> value
      | (head1::tail1), (head2::tail2) ->
          let newValue = f index value head1 head2 in
          foldWithIndex (index+1) newValue tail1 tail2
      | _, _ -> raise (Invalid_argument "listFold2i")
  in
  foldWithIndex 0 initial list1 list2

let mapFilter func list =
  let rec worker acc = function
    | [] -> List.rev acc
    | hd :: tail ->
        match func hd with
          | Some result -> worker (result :: acc) tail
          | None -> worker acc tail
  in
  worker [] list

let multiMap func list =
  let nestedResults = List.map func list in
  List.flatten nestedResults

let rec combineList seperator = function
  | [] -> []
  | [last] -> [last]
  | first :: tail ->
      first :: seperator :: (combineList seperator tail)

let dequoteString quoteChar str =
  let length = String.length str in
  if length >= 2 && str.[0] = quoteChar && str.[length-1] = quoteChar then
    `Quoted (String.sub str 1 (length - 2))
  else
    `NotQuoted str

let trim str =
  let rec findFirstNonWS str index succ =
    if index < String.length str && index >= 0 && str.[index] = ' ' then
      findFirstNonWS str (succ index) succ
    else
      index
  in
  let frontSpaces = findFirstNonWS str 0 ((+) 1) in
  let frontTrimmed = String.sub str frontSpaces (String.length str - frontSpaces)
  in
  let frontTrimmedLength = String.length frontTrimmed in
  let backSpaces = frontTrimmedLength - 1 - findFirstNonWS frontTrimmed (frontTrimmedLength-1) (fun x -> x - 1) in
  String.sub frontTrimmed 0 (frontTrimmedLength - backSpaces)

let mapString f str =
  let newString = String.copy str in
  let strLength = String.length str in
  for charNum = 0 to strLength-1 do
    newString.[charNum] <- f str.[charNum]
  done;
  newString



