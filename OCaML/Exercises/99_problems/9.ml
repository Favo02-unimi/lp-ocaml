(* Pack consecutive duplicates of list elements into sublists. *)

let pack list = List.rev (
  let rec pack list cur_elem cur_list res =
    match list with
      | [] -> (cur_list :: res)
      | h :: tail when h = cur_elem -> pack tail cur_elem (h :: cur_list) res
      | h :: tail -> pack tail h [h] (cur_list :: res)
  in pack (List.tl list) (List.hd list) [List.hd list] []);;

pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
(* - : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]] *)
