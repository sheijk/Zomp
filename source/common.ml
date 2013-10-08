open Printf

(* moved here from Zompvm because exception was not caught properly (compiler bug?) *)
exception FailedToEvaluateLLVMCode of string * string

module CommonCombinators =
struct
  let map2nd f (fst, snd) = (fst, f snd)
  let map1st f (fst, snd) = (f fst, snd)
  let (<<=) f g = f g
  let (>>=) x f = f x
  let ($) f x = f x
  let (++) f g x = f (g x)
  let ($) f x = f x
  let (|>) x f = f x
  let (=~) str re = Str.string_match (Str.regexp re) str 0

  let makeGuardedFunction constructor destructor =
    fun constructorArg f ->
      let resource = constructor constructorArg in
      let result =
        try
          f resource
        with exn ->
          destructor resource;
          raise exn
      in
      destructor resource;
      result

    (**
       A simple type to mark staged computations. Use this when a function
       produces another function and does non-trivial precomputation to
       distinguish it from a partial application. This helps to avoid
       accidentally introducing repeated calls to the expensive function.

        For example:

        let makeMatcher txt =
          let re = Str.regexp_string txt in
          let f str = Str.string_match re str in
          Staged f
    *)
  type ('a, 'b) staged = Staged of ('a -> 'b)
end
include CommonCombinators

module CommonNum =
struct
  let rec fromTo min max =
    if min > max then []
    else min :: fromTo (min+1) max

  let safeParseInt str =
    try
      int_of_string str
    with Failure _ ->
      -1

  let restrictToSingleprecision double =
    let array = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 1 in
    Bigarray.Array1.set array 0 double;
    Bigarray.Array1.get array 0

end
include CommonNum

module CommonString =
struct
  (** TODO: check if String.concat is as fast, replace it *)
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

  let lineCount source =
    let lines = Str.split (Str.regexp_string "\\n") source in
    List.length lines + 1

  let indent string =
    let indentLine line =
      let len = String.length line in
      if len >= 1 && line.[len-1] = ':' then line
      else "  " ^ line
    in
    let lines = Str.split (Str.regexp "\n") string in
    let indentedLines = List.map indentLine lines in
    combine "\n" indentedLines

  let removeQuotes str =
    let length = String.length str in
    if length > 1 && str.[0] = '"' && str.[length-1] = '"' then begin
      let substring str ~first ~len =
        let newstr = String.create len in
        String.blit str first newstr 0 len;
        newstr
      in
      substring str 1 (String.length str - 2)
    end else
      str

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

  type sequenceAnchor = FromFront of int | FromBack of int

  let splitAt str pos =
    let strLength = String.length str in
    let splitPos = match pos with
      | FromFront pos -> pos
      | FromBack pos -> strLength - pos
    in
    let front = Str.string_before str splitPos
    and back = Str.string_after str splitPos
    in
    front, back

  let lastChar str = str.[String.length str - 1]

  let splitLastChar str =
    let strLength = String.length str in
    assert (strLength > 0);
    let lastChar = str.[strLength - 1] in
    let front = Str.string_before str (strLength-1) in
    front, lastChar

  let endsWith string ending =
    let stringLength = String.length string in
    let endingLength = String.length ending in
    stringLength >= endingLength &&
      Str.string_after string (stringLength-endingLength) = ending

  let splitup oplist str =
    let rec worker str accum =
      let strLength = String.length str in
      let rec findMatch = function
        | [] ->
          None
        | op :: remOps ->
          let opLength = String.length op in
          if strLength >= opLength && Str.first_chars str opLength = op then
            Some op
          else
            findMatch remOps
      in
      if strLength > 0 then
        match findMatch oplist with
          | Some nextOp ->
            let remstr = Str.last_chars str (strLength - String.length nextOp) in
            worker remstr (nextOp :: accum)
          | None ->
            None
      else
        Some (List.rev accum)
    in
    worker str []

  let foldString str f init =
    let strLength = String.length str in
    let rec worker index v =
      if index >= strLength then v
      else worker (index+1) (f v str.[index])
    in
    worker 0 init

  let stringToRevCharList str =
    foldString str (fun lst chr -> chr :: lst) []

  let stringToCharList = List.rev ++ stringToRevCharList

  let restrictLength maxChars str =
    let strLength = String.length str in
    let dots = "..." in
    if strLength <= maxChars then
      str
    else
      let shortenedStr = String.sub str 0 maxChars in
      let dotsLength = String.length dots in
      String.blit dots 0 shortenedStr (maxChars - dotsLength) dotsLength;
      shortenedStr
end
include CommonString

module CommonFile =
struct
  let readLinesRev channel =
    let rec worker lines =
      try
        let newline = input_line channel in
        worker (newline :: lines)
      with End_of_file ->
        lines
    in
    worker []

  let readLines channel =
    List.rev (readLinesRev channel)

  (** Read all lines from the given file. Adds a newline after last line even if
      not present. *)
  let readChannel channel =
    let revLines = readLinesRev channel in
    let fileContent = combine "\n" (List.rev ("" :: revLines)) in
    fileContent
  
  let withFileForReading filename f =
    (makeGuardedFunction open_in close_in) filename f

  let withFileForWriting filename f =
    (makeGuardedFunction open_out close_out) filename f

  let withFileForAppending filename f =
    (makeGuardedFunction
       (open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666)
       close_out)
      filename
      f

  let absolutePath fileName =
    let fileName =
      if fileName = "." then ""
      else fileName
    in
    if Filename.is_relative fileName then
      Filename.concat (Sys.getcwd()) fileName
    else
      fileName

  (**
     Will
     - remove ".." and "." path elements
     - remove trailing "/"
     - remove duplicate "/"
     - change "\\" to "/"

     Correctly works with both Unix and Windows syle paths.
  *)
  let canonicalFileName fileName =
    let fileNameLength = String.length fileName in
    if fileNameLength = 0 then
      ""
    else
      let elements = Str.split (Str.regexp "[/\\]") fileName in
      let isImportant = function
        | "." | "" -> false
        | _ -> true
      in
      let noNoopElements = List.filter isImportant elements in
      let removeRelPaths elements =
        let rec step elements acc =
          match elements, acc with
            | [], `ReverseList reversed ->
              `ReverseList reversed
            | (".." :: rem), `ReverseList (_ :: accRem) ->
              step rem (`ReverseList accRem)
            | element :: rem, `ReverseList acc ->
              step rem (`ReverseList (element :: acc))
        in
        step elements (`ReverseList [])
      in
      let `ReverseList reversedElements = removeRelPaths noNoopElements in
      let combinedAndCleanedElements = String.concat "/" (List.rev reversedElements) in
      let startedWithSlash = fileName.[0] = '/' in
      if startedWithSlash then
        "/" ^ combinedAndCleanedElements
      else
        combinedAndCleanedElements

  let rec findFileIn fileName paths =
    if Filename.is_implicit fileName then begin
      match paths with
        | [] ->
          None
        | path :: remPaths ->
          let absoluteFileName = Filename.concat (absolutePath path) fileName in
          if Sys.file_exists absoluteFileName then
            Some absoluteFileName
          else
            findFileIn fileName remPaths
    end else begin
      if Sys.file_exists fileName then
        Some (absolutePath fileName)
      else
        None
    end

  let readFileLines fileName =
    withFileForReading fileName readLines

  let readFile ?(paths = ["."]) fileName =
    match findFileIn fileName paths with
      | Some absFileName ->
        let channel = open_in absFileName in
        readChannel channel
      | None ->
        raise (Sys_error
                 (Printf.sprintf "file '%s' could not be found in '%s', pwd = %s"
                    fileName
                    (combine "', '" paths)
                    (Sys.getcwd())))

end
include CommonFile

module CommonOption =
struct
  let applyIfSome f = function
    | None -> None
    | Some v -> f v

  let someOrDefault optionValue default =
    match optionValue with
      | Some value -> value
      | None -> default

  let applyOptOrDefault optionalValue f default =
    match optionalValue with
      | Some value -> f value
      | None -> default

  let some x = Some x
  let id x = x
end
include CommonOption

module CommonList =
struct
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

  let rec partitionList keyF = function
    | [] -> []
    | first :: rem ->
      let key = keyF first in
      let withKey, otherKey = List.partition (fun ti -> key = keyF ti) rem in
      (first :: withKey) :: partitionList keyF otherKey

  let listIteri f list =
    let rec iterWithIndex index = function
      | [] -> ()
      | hd :: tl ->
        let () = f index hd in
        iterWithIndex (index+1) tl
    in
    iterWithIndex 0 list

  let listMapi f list =
    let rec mapWithIndex index acc = function
      | [] ->
        List.rev acc
      | head::tail ->
        mapWithIndex (index+1) (f index head :: acc) tail
    in
    mapWithIndex 0 [] list

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

  let rec listCreate size value =
    if (size <= 0) then []
    else value :: listCreate (size-1) value

  let listCreateF size f =
    let rec worker rem =
      if (rem <= 0) then []
      else f (size-rem) :: worker (rem-1)
    in
    worker size

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

  let rec mapfold f initialValue = function
    | [] -> [], initialValue
    | hd :: tl ->
      let mappedElements, newValue = f initialValue hd in
      let tlmapped, finalValue = mapfold f newValue tl in
      mappedElements @ tlmapped, finalValue

  let rec combineList seperator = function
    | [] -> []
    | [last] -> [last]
    | first :: tail ->
      first :: seperator :: (combineList seperator tail)

  let rec listContains element = function
    | [] -> false
    | e :: rem when e = element -> true
    | _ :: rem -> listContains element rem

  let indexOf element list =
    let rec worker index = function
      | [] -> failwith "indexOf"
      | hd :: _ when hd = element -> index
      | _ :: rem -> worker (index+1) rem
    in
    worker 0 list

  let addToList (listPlace :'a list ref) (newElement :'a) = function
    | `Back ->
      listPlace := !listPlace @ [newElement]
    | `Front ->
      listPlace := newElement :: !listPlace

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

end
include CommonList

module Ref =
struct
  let get r = !r
  let getter r () = !r
  let set r value = r := value
end

module Vector : sig
  type 'a t

  val make : unit -> 'a t
  val append : 'a t -> 'a -> int
  val size : 'a t -> int
  val get : 'a t -> int -> 'a
end = struct
  type 'a t = {
    mutable values : 'a array;
    (** number of used elements in values *)
    mutable size :int;
  }

  let validIndex vec index = index >= 0 && index < vec.size

  let make () = { values = [||]; size = 0 }

  let append vec value =
    if vec.size + 1 > Array.length vec.values then begin
      (** No, we do not want to grow by a factor of two. 1.5 reduces memory
       overhead while releatedly appending stays logarithmic. The factor should
       be configurable, though. *)
      let newSize = if vec.size = 0 then 2 else vec.size * 3 / 2 in
      let newValues = Array.make newSize value in
      Array.blit vec.values 0 newValues 0 vec.size;
      vec.values <- newValues
    end;
    vec.values.(vec.size) <- value;
    vec.size <- vec.size + 1;
    vec.size - 1

  let size vec = vec.size

  let get vec index =
    if validIndex vec index then
      vec.values.(index)
    else
      failwith "Vector.get"

  let set vec index value =
    if validIndex vec index then
      vec.values.(index) <- value
    else
      failwith "Vector.set"
end

module Statistics : sig
  type section

  val createSection : string -> section
  val createIntCounter : section -> string -> int -> (unit -> int) -> unit
  val createFloatCounter : section -> string -> int -> (unit -> float) -> unit

  type counterType = Float | Int
  type sectionRegisterFunc = sectionName:string -> unit
  type counterRegisterFunc = sectionName:string -> name:string -> fractionalDigits:int -> typ:counterType -> id:int -> unit

  val setImplementation : sectionRegisterFunc -> counterRegisterFunc -> unit

end = struct
  type section = string

  type counterType = Float | Int
  type counterRegisterFunc = sectionName:string -> name:string -> fractionalDigits:int -> typ:counterType -> id:int -> unit
  type sectionRegisterFunc = sectionName:string -> unit

  let delayedCounters = ref []
  let delayedSections = ref []
  let registerCounterDelayed ~sectionName ~name ~fractionalDigits ~typ ~id =
    delayedCounters := (sectionName, name, fractionalDigits, typ, id) :: !delayedCounters
  let registerSectionDelayed ~sectionName =
    delayedSections := sectionName :: !delayedSections

  let registerSection = ref registerSectionDelayed
  let registerCounter = ref registerCounterDelayed

  let setImplementation section counter =
    List.iter (fun sectionName -> section ~sectionName) !delayedSections;
    delayedSections := [];
    List.iter
      (fun (sectionName, name, fractionalDigits, typ, id) -> counter ~sectionName ~name ~fractionalDigits ~typ ~id)
      !delayedCounters;
    delayedCounters := [];
    registerSection := section;
    registerCounter := counter

  type getter = GetFloat of (unit -> float) | GetInt of (unit -> int)
  let counterGetters : getter Vector.t = Vector.make()

  let createSection name =
    !registerSection name;
    name

  let createIntCounter section name fractionalDigits getValue =
    let id = Vector.append counterGetters (GetInt getValue) in
    !registerCounter section name fractionalDigits Int id;
    ()

  let createFloatCounter section name fractionalDigits getValue =
    let id = Vector.append counterGetters (GetFloat getValue) in
    !registerCounter section name fractionalDigits Float id;
    ()

  let getCounterValueInt id : int =
    let getter = Vector.get counterGetters id in
    match getter with
      | GetInt f -> f()
      | _ -> failwith "getCounterValueInt"

  let getCounterValueFloat (id : int) : float =
    let getter = Vector.get counterGetters id in
    match getter with
      | GetFloat f -> f()
      | _ -> failwith "getCounterValueFloat"

  let () =
    Callback.register "zompCommonGetCounterValueInt" getCounterValueInt;
    Callback.register "zompCommonGetCounterValueFloat" getCounterValueFloat
end

module Profiling =
struct
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
  let toplevelStartTime = Sys.time()

  let printFlatTimings () =
    let rec toList timingInfo =
      let childLists = List.map toList timingInfo.childs in
      timingInfo :: List.flatten childLists
    in
    let allTimingInfos = toList toplevelTimingInfos in
    let byName = partitionList (fun ti -> ti.name) allTimingInfos in
    let sumAndName =
      List.fold_left
        (fun (_,sum) ti ->
           let childTimes = List.map (fun ti -> ti.totalTime) ti.childs in
           let totalChildTime = List.fold_left (+.) 0. childTimes in
           ti.name, sum +. ti.totalTime -. totalChildTime)
        ("",0.)
    in
    let nameAndTimes = List.map sumAndName byName in
    printf "Flat timings:\n";
    List.iter (fun (name, time) -> printf "  %f - %s\n" time name)
      (List.sort (fun (_, time1) (_, time2) -> 1 - compare time1 time2) nameAndTimes)

  let rec totalTime timingInfo =
    let childTimes = List.map totalTime timingInfo.childs in
    let totalChildTimes = List.fold_left (+.) 0.0 childTimes in
    totalChildTimes +. timingInfo.totalTime

  let printTimings () =
    let rec worker prefix timingInfo =
      printf "%s%f - %s\n" prefix timingInfo.totalTime timingInfo.name;
      match timingInfo.childs with
        | _ :: _ ->
            let childTime = List.fold_left (fun sum ti -> sum +. ti.totalTime) 0.0 timingInfo.childs in
            let minTime = 0.1 *. timingInfo.totalTime in
            let relevantChilds = List.filter
              (fun ti -> ti.totalTime > minTime || ti.totalTime > 0.1) timingInfo.childs in
            let sortedChilds =
              List.sort (fun ti1 ti2 -> 1 - compare ti1.totalTime ti2.totalTime)
                ({ name = "unmeasured"; totalTime = timingInfo.totalTime -. childTime; childs = [] }
                 :: relevantChilds)
            in
            List.iter (worker (prefix ^ "  ")) sortedChilds;
        | [] ->
            ()
    in
    toplevelTimingInfos.totalTime <- Sys.time() -. toplevelStartTime;
    printFlatTimings();
    let toplevelStopTime = Sys.time() in
    toplevelTimingInfos.totalTime <- (toplevelStopTime -. toplevelStartTime);
    printf "Hierarchical timings:\n";
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
    guarded f
      ~finally:(fun () -> popTimingContext (currentTimingInfo, startTime) )
end

(* let collectTimingInfo _ f = f() *)
let collectTimingInfo = Profiling.collectTimingInfo

let sampleFunc1 name f arg0 = collectTimingInfo name (fun () -> f arg0)
let sampleFunc2 name f arg0 arg1 = collectTimingInfo name (fun () -> f arg0 arg1)
let sampleFunc3 name f arg0 arg1 arg2 = collectTimingInfo name (fun () -> f arg0 arg1 arg2)
let sampleFunc4 name f arg0 arg1 arg2 arg3 = collectTimingInfo name (fun () -> f arg0 arg1 arg2 arg3)

