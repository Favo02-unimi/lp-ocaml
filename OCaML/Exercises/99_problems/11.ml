(* Modified run-length encoding. *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode list =

  let rle_builder count value =
    match count with
      | 1 -> One value
      | n -> Many (n, value)

  in
    let rec encode list cur_value cur_count res =
      match list with
        | [] -> ((rle_builder cur_count cur_value) :: res)
        | h :: tail when h = cur_value -> encode tail cur_value (cur_count+1) res
        | h :: tail -> encode tail h 1 ((rle_builder cur_count cur_value) :: res)
    in List.rev (encode (List.tl list) (List.hd list) 1 []);;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* - : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")] *)
