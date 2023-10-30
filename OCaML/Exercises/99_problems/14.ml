(* Duplicate the elements of a list. *)

let duplicate list =
  let rec duplicate list res =
    match list with
      | [] -> res
      | h :: tail -> duplicate tail (h :: h :: res)
  in List.rev(duplicate list []);;

duplicate ["a"; "b"; "c"; "c"; "d"];;
duplicate [];;
(* - : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)

