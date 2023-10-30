(* Remove the K'th element from a list. *)

let remove_at index list =
  let rec remove_at count list res =
    match list with
      | [] -> res
      | h :: tail when count = index -> (remove_at [@tailcall]) (count+1) tail res
      | h :: tail -> (remove_at [@tailcall]) (count+1) tail (h :: res)
  in List.rev (remove_at 0 list []);;

remove_at 1 ["a"; "b"; "c"; "d"];;
(* - : string list = ["a"; "c"; "d"] *)
