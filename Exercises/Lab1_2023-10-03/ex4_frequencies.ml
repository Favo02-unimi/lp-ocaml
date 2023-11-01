(* import Str module *)
#load "str.cma";;

(* create map on type String*)
module StringMap = Map.Make(String);;

(* if "elem" is in "map" increment its value by 1, otherwise set to 1 *)
let incrementValue elem map = 
  (* generate new value of elem in map: increase by 1 or 1 if not found *)
  let newValue elem map =
    try
      (StringMap.find elem map) + 1
    with
      | Not_found -> 1
  in StringMap.add elem (newValue elem map) map;;

(* sums the occurrence of every word (case insensitive) in the list to map*)
let rec countLine list map = 
  match list with
    | [] -> map
    | h :: tl -> countLine tl (incrementValue (String.lowercase_ascii h) map);;

(* recursively read each line from file, counting the occourrences of each word in the line using "countLine" *)
let rec readLines file map =
  try
    (* count words in parsed line and call next "readLines" *)
    readLines file (countLine
      (* convert "split_result" type to string *)
      (List.map
        (fun s -> match s with
          | Str.Text str -> str
          | Str.Delim str -> str)
        (* split line read using "input_line" on space and symbols *)
        (Str.full_split (Str.regexp "[ .,:?!\"\']") (input_line file)))
      map)
  with
    | End_of_file -> close_in file; map;;

(* input file *)
let file = open_in "./2023-10-03_ex4_input.txt";;
(* call "readLines" to count words in file *)
let occs = readLines file StringMap.empty;;

(* print map *)
StringMap.iter (fun k v -> Printf.printf "%d \t %s\n" v k) occs;;
