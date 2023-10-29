(* Eliminate consecutive duplicates of list elements. *)

let compress list = List.rev
  (let rec compress list last res =
    match list with
      | [] -> res
      | h :: tail when (last = h) -> compress tail last res
      | h :: tail -> compress tail h (h :: res)
  in (compress [@tailcall]) (List.tl list) (List.hd list) [List.hd list]);;

compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* - : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)

let rec compress list =
  match list with
    | a :: ((b :: _) as tail) ->
      if a = b
        then compress tail
        else a :: compress tail
    | a :: [] -> list
    | [] -> list;;

(compress [@tailcall]) ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* - : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)
