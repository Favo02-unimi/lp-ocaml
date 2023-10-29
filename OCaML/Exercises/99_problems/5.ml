(* Reverse a list. *)

let rev list =
  let rec rev list res =
    match list with
      | [] -> res
      | h :: tail -> rev tail (h :: res)
  in rev list [];;

rev ["a"; "b"; "c"];;
(* - : string list = ["c"; "b"; "a"] *)

let rev = List.rev;;

rev ["a"; "b"; "c"];;
(* - : string list = ["c"; "b"; "a"] *)
