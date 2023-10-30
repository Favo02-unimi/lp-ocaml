(* Rotate a list N places to the left. *)

let rotate_positive list n =
  let rec rotate_positive list count buffer =
    match list with
      | [] -> buffer
      | (_ as l) when count = 0 -> l @ (List.rev (buffer))
      | h :: tail -> (rotate_positive [@tailcall]) tail (count-1) (h::buffer)
  in rotate_positive list n [];;

let rotate list n =
  match n > 0 with
    | true -> rotate_positive list n
    | false -> List.rev (rotate_positive (List.rev list) (-n));;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
(* - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] *)
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
(* - : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] *)
