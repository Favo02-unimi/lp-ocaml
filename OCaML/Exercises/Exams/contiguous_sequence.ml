let contiguous_sequence list =
  let rec contiguous_sequence list count res =
    match list with
      | [] -> res
      | h :: [] -> (count+1) :: res
      | h :: ((j :: _) as tail) when h = j -> contiguous_sequence tail (count+1) res
      | h :: ((j :: _) as tail) -> contiguous_sequence tail 0 ((count+1)::res)
  in List.rev(contiguous_sequence list 0 []);;

contiguous_sequence [];;
contiguous_sequence [10];;
contiguous_sequence [1;1];;
contiguous_sequence [4;4;4;4;4;2;2;3;3;3;3;3;3;7];;
contiguous_sequence [1;2;3;4;5;6;7];;
