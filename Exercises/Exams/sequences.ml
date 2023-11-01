let seq list =
  let rec seq list cur acc =
    match list with
      | h::t when (h = 0) ->
          if (cur != 0)
            then seq t 0 (cur::acc)
            else seq t 0 acc
      | h::t when (h != 0) -> seq t (cur+h) acc
      | _ -> acc
  in seq list 0 [];;

seq [1;2;3;0;4;5;0;0;6;0];;
