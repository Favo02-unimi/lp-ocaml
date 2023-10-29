(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. *)

let last list =
  match (List.rev list) with
    | h :: tail -> Some h
    | [] -> None;;

last ["a" ; "b" ; "c" ; "d"];;
(* - : string option = Some "d" *)
last [];;
(* - : 'a option = None *)
