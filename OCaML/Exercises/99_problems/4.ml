(* Find the number of elements of a list. *)

let length list =
  let rec length list count =
    match list with
      | [] -> count
      | h :: tail -> length tail (count+1)
  in length list 0;;

(length [@tailcall]) ["a"; "b"; "c"];;
(* - : int = 3 *)
(length [@tailcall]) [];;
(* - : int = 0 *)

let length = List.length;;

(length [@tailcall]) ["a"; "b"; "c"];;
(* - : int = 3 *)
(length [@tailcall]) [];;
(* - : int = 0 *)
