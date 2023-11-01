(* Find out whether a list is a palindrome. *)

let is_palindrome list = (list = List.rev list);;

is_palindrome ["x"; "a"; "m"; "a"; "x"];;
(* - : bool = true *)
not (is_palindrome ["a"; "b"]);;
(* - : bool = true *)
