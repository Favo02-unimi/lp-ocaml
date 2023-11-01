(* Split a list into two parts; the length of the first part is given. *)

exception ListTooShort;;

let split list len =
  let rec split list count res1 =
    match list with
      | [] -> (List.rev (res1), [])
      | h :: tail when count = len -> ((List.rev (h :: res1)), tail)
      | h :: tail -> (split [@tailcall]) tail (count+1) (h :: res1)
  in split list 1 [];;

split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
(* - : string list * string list = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]) *)
split ["a"; "b"; "c"; "d"] 5;;
(* - : string list * string list = (["a"; "b"; "c"; "d"], []) *)
