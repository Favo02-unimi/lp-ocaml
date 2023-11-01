(* Drop every N'th element from a list. *)

let drop list nth =
  let rec drop list count res =
    match list with 
      | [] -> res
      | h :: tail when count = 1 -> drop tail nth res 
      | h :: tail -> drop tail (count-1) (h :: res)
  in List.rev (drop list nth []);;

drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
(* - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"] *)

let drop list nth =
  let rec drop list count res =
    match list with 
      | [] -> res
      | h :: tail when (count mod nth) = 0 -> drop tail (count+1) res 
      | h :: tail -> drop tail (count+1) (h :: res)
  in List.rev (drop list 1 []);;
  
drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
(* - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"] *)
