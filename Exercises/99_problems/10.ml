(* Run-length encoding of a list. *)

let encode list = List.rev (
  let rec encode list cur_elem cur_count res =
    match list with
      | [] -> ((cur_count, cur_elem) :: res)
      | h :: tail when h = cur_elem -> encode tail cur_elem (cur_count+1) res
      | h :: tail -> encode tail h 1 ((cur_count, cur_elem) :: res)
  in encode (List.tl list) (List.hd list) 1 []);;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* - : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)
