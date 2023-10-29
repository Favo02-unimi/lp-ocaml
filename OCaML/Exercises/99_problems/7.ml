(* Flatten a nested list structure. *)

type 'a node =
  | One of 'a
  | Many of 'a node list;;

let flatten list =
  let rec flatten list res =
    match list with
      | [] -> res
      | h :: tail -> flatten tail (res @
        (match h with
          | One a -> [a]
          | Many a -> flatten a []
          ))
  in flatten list [];;

flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
(* - : string list = ["a"; "b"; "c"; "d"; "e"] *)
