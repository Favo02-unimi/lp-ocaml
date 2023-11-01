(*
  compile and run using:
    ocamlc *.ml* && ./a.out

  or:
    ocamlc -c *.mli
    ocamlc -c Interval.ml
    ocamlc -o main Interval.cmo main.ml
    ./main
*)


module Interval (Type : Comparable.Comparable) : (IntervalI.IntervalI with type endpoint = Type.t) =
  struct

    type endpoint = Type.t

    type interval =
      | Inter of (endpoint * endpoint)

    exception WrongInterval

    let check_valid inter =
      match inter with
        | Inter (a, b) when ((Type.compare a b) <= 0) -> inter
        | _ -> raise WrongInterval

    let create a b = check_valid (Inter (a, b))

    let is_empty inter =
      match inter with
        | Inter (a,b) when (Type.compare a b) < 0 -> false
        | _ -> true

    let contains inter elem =
      match inter with
        | Inter (a,b) ->
          let bigger_than_start = (Type.compare a elem) < 0
          and smaller_than_end = (Type.compare b elem) > 0
          in match (bigger_than_start && smaller_than_end) with
            | true -> true
            | false -> false

    let intersect inter1 inter2 =
      match (inter1, inter2) with
        | ((Inter (a,b)), (Inter (c,d))) ->
          let a_bigger_c = (Type.compare a c) > 0
          and b_bigger_d = (Type.compare b d) > 0
          in match (a_bigger_c, b_bigger_d) with
            | (true, true) -> check_valid (Inter (a, d))
            | (false, true) -> check_valid (Inter (c, d))
            | (true, false) -> check_valid (Inter (a, b))
            | (false, false) -> check_valid (Inter (c, b))

    let tostring inter =
      match inter with
        | Inter (a,b) -> Printf.sprintf "[%s, %s]" (Type.tostring a) (Type.tostring b)

  end;;

module IntInterval = Interval (
  struct
    type t = int
    let compare a b = compare a b
    let tostring a = string_of_int a
  end);;

module StringInterval = Interval(
  struct
    type t = string
    let compare = String.compare
    and tostring = fun x -> x
  end);;
