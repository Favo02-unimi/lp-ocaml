(* Insert an element at a given position into a list. *)

let insert_at elem index list =
  let rec insert_at count list res =
    match list with
      | [] -> List.rev (elem :: res)
      | h :: tail when count = index -> (List.rev (h::elem::res)) @ tail
      | h :: tail -> (insert_at [@tailcall]) (count+1) tail (h :: res)
  in insert_at 0 list [];;

insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
(* - : string list = ["a"; "alfa"; "b"; "c"; "d"] *)
insert_at "alfa" 3 ["a"; "b"; "c"; "d"];;
(* - : string list = ["a"; "b"; "c"; "alfa"; "d"] *)
insert_at "alfa" 4 ["a"; "b"; "c"; "d"];;
(* - : string list = ["a"; "b"; "c"; "d"; "alfa"] *)
