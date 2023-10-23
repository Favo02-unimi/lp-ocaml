(* dichiarare lista di elementi atomici *)
let alkaline_earth_metals = [4;12;20;38;56;88];;

(* trovare massimo elemento in lista *)
let max_in_list list =
  let rec max_in_list max list =
    match list with
      | [] -> max
      | h :: tl ->
        if (h > max)
          then max_in_list h tl
          else max_in_list max tl
  in max_in_list (-1) list;;

Printf.printf "max in alkaline metals: %d\n" (max_in_list alkaline_earth_metals);; 

(* trovare massimo elemento in lista (uguale a prima con zucchero sintattico) *)
let max_in_list list =
  let rec max_in_list max = function
      [] -> max
      | h :: tl ->
        if (h > max)
          then max_in_list h tl
          else max_in_list max tl
  in max_in_list (-1) list;;

Printf.printf "max in alkaline metals: %d\n" (max_in_list alkaline_earth_metals);; 

(* salvare acnhe nome elemento oltre a numero atomico *)
type metal = {
  name : string;
  atomic : int;
};;
let alkaline_earth_metals_record = [
  {name="beryllium";atomic=4};
  {name="test";atomic=120};
  {name="calcium";atomic=92};
  {name="strontium";atomic=2};
  {name="barium";atomic=56};
  {name="radium";atomic=88};
];;

(* trovare massimo in lista di record *)
let max_in_list_records list =
  let rec max_in_list_records max = function
      [] -> max
      | h :: tl ->
        if (h.atomic > max.atomic)
          then max_in_list_records h tl
          else max_in_list_records max tl
  in max_in_list_records (List.nth list 0) list;;

Printf.printf "max record in alkaline metals: %s %d\n" (max_in_list_records alkaline_earth_metals_record).name (max_in_list_records alkaline_earth_metals_record).atomic;; 

(* ordinare lista di record *)
let comparator a b = compare a.atomic b.atomic;;
let sort list = List.sort comparator list;;
sort alkaline_earth_metals_record;; (* funzione di libreria con custom comparator *)

let rec print_record_list list =
  match list with
    | [] -> []
    | h :: tl -> Printf.printf "%s %d\n" h.name h.atomic; print_record_list tl;;

print_string "\nlista (non ordinata) di alkaline metals:\n";;
print_record_list alkaline_earth_metals_record;;

(* algoritmo di ordinamento custom (non sui record ma solo su int) *)

(* procedura di merge del mergesort *)
let merge list1 list2 =
  let rec merge list1 list2 result =
    match list1, list2 with
      | [], [] -> result
      | h1 :: rem1, [] -> merge rem1 [] (result @ [h1])
      | [], h2 :: rem2 -> merge [] rem2 (result @ [h2])
      | h1 :: rem1, h2 :: rem2 ->
        if (h1 > h2)
          then merge list1 rem2 (result @ [h2])
          else merge rem1 list2 (result @ [h1])
  in merge list1 list2 [];;

(* merge sort (non proprio, la lista non viene divisa in 2 ma in primo elemento e il resto, quindi la complessità è diversa) *)
let merge_sort list =
  let rec merge_sort list =
    match list with
      | [] -> []
      | h :: [] -> list
      | h :: tail -> merge (merge_sort [h]) (merge_sort tail)
  in merge_sort list;;

let alkaline_earth_metals = [4;12;20;38;56;88];;
let noble_gases = [86;10;18;36;2;54];;

let sorted_elements = merge_sort (alkaline_earth_metals @ noble_gases);;

let rec print_list list =
  match list with
    | [] -> []
    | h :: tl -> Printf.printf "%d " h; print_list tl;;

print_string "\nlista di interi ordinata con mergesort:\n";;
print_list sorted_elements;;

(* mergesort su record *)
let merge_record list1 list2 =
  let rec merge_record list1 list2 result =
    match list1, list2 with
      | [], [] -> result
      | h1 :: rem1, [] -> merge_record rem1 [] (result @ [h1])
      | [], h2 :: rem2 -> merge_record [] rem2 (result @ [h2])
      | h1 :: rem1, h2 :: rem2 ->
        if (h1.atomic > h2.atomic)
          then merge_record list1 rem2 (result @ [h2])
          else merge_record rem1 list2 (result @ [h1])
  in merge_record list1 list2 [];;

(* merge sort (non proprio, la lista non viene divisa in 2 ma in primo elemento e il resto, quindi la complessità è diversa) *)
let merge_sort_record list =
  let rec merge_sort_record list =
    match list with
      | [] -> []
      | h :: [] -> list
      | h :: tail -> merge_record (merge_sort_record [h]) (merge_sort_record tail)
  in merge_sort_record list;;

let alkaline_earth_metals_record = [
  {name="magnesium";atomic=12};
  {name="beryllium";atomic=4};
  {name="radium";atomic=88};
  {name="strontium";atomic=38};
  {name="calcium";atomic=20};
  {name="barium";atomic=56};
];;
let noble_gases_record = [
  {name="argon";atomic=18};
  {name="helium";atomic=2};
  {name="krypton";atomic=36};
  {name="radon";atomic=86};
  {name="xenon";atomic=54};
  {name="neon";atomic=10};
];;

print_string "\n\nlista di record ordinata con mergesort:\n";;
print_record_list (merge_sort_record (alkaline_earth_metals_record @ noble_gases_record));;
