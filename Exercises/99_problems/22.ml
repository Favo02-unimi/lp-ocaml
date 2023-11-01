(* Create a list containing all integers within a given range. *)

let pos_range s e =
  let rec pos_range cur res =
    match cur = e with
      | true -> cur::res
      | false -> pos_range (cur+1) (cur::res)
  in List.rev (pos_range s []);;

let range s e =
  match ((compare s e) >= 0) with
    | true -> List.rev (pos_range e s)
    | false -> pos_range s e;;

range 4 9;;
(* - : int list = [4; 5; 6; 7; 8; 9] *)
range 9 4;;
(* - : int list = [9; 8; 7; 6; 5; 4] *)
