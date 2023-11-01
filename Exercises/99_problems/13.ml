(* Run-length encoding of a list (direct solution). *)

type 'a rle =
    | One of 'a
    | Many of int * 'a;;


let encode list =
  let pack_rle count value =
    match count with
      | 1 -> One value
      | _ -> Many (count, value)
  in
    let rec encode list cur_val cur_count res =
      match list with
        | [] -> (pack_rle cur_count cur_val) :: res
        | h :: tail when h = cur_val -> encode tail cur_val (cur_count+1) res
        | h :: tail -> encode tail h 1 ((pack_rle cur_count cur_val) :: res)
    in List.rev (encode (List.tl list) (List.hd list) 1 []);;  

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
(* - : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")] *)
