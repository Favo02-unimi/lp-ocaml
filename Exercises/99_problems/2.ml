(* Find the last but one (last and penultimate) elements of a list. *)

let last_two list =
  match (List.rev list) with
    | h1 :: h2 :: tail -> Some (h2, h1)
    | _ -> None;;

last_two ["a"; "b"; "c"; "d"];;
(* - : (string * string) option = Some ("c", "d") *)
last_two ["a"];;
(* - : (string * string) option = None *)
