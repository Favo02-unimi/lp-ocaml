(* Find the K'th element of a list. *)

let rec at index list =
  match (index, list) with
    | (1, h :: tail) -> Some h
    | (_, []) -> None
    | (_, h :: tail) -> at (index-1) tail;;

(at [@tailcall]) 3 ["a"; "b"; "c"; "d"; "e"];;
(* - : string option = Some "c" *)
(at [@tailcall]) 3 ["a"];;
(* - : string option = None *)

let rec at index list =
  try
    Some (List.nth list (index-1))
  with Failure _ ->
    None;;

(at [@tailcall]) 3 ["a"; "b"; "c"; "d"; "e"];;
(* - : string option = Some "c" *)
(at [@tailcall]) 3 ["a"];;
(* - : string option = None *)
