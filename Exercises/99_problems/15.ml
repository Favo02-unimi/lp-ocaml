(* Replicate the elements of a list a given number of times. *)

let replicate list rem =
  let rec replicate list count res =
    match list with
      | [] -> res
      | h :: tail when count = 1 -> (replicate [@tailrec]) tail rem (h :: res)
      | h :: tail -> (replicate [@tailrec]) list (count-1) (h :: res)
  in List.rev (replicate list rem []);;

replicate ["a"; "b"; "c"] 3;;
(* - : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] *)
