(* Decode a run-length encoded list. *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let decode list =
  let rec unroll item res =
    match item with
      | One char -> [char]
      | Many (count, char) when count = 1 -> char :: res
      | Many (count, char) -> (unroll [@tailcall]) (Many ((count-1), char)) (char :: res)
  in
    let rec decode list res =
      match list with
        | [] -> res
        | h :: tail -> ((decode [@tailcall]) tail (res @ (unroll h [])))
    in decode list [];;

decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;

(* - : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)
