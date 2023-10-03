let merge list1 list2 =
  let rec merge list1 list2 result =
    match list1, list2 with
      | [], [] -> result
      | h1 :: rem1, [] -> merge rem1 [] (result @ [h1])
      | [], h2 :: rem2 -> merge [] rem2 (result @ [h2])
      | h1 :: rem1, h2 :: rem2 ->
        if (h1 > h2)
          then merge list1 rem2 (result @ [h2])
          else merge rem1 list2 (result @ [h1])
  in merge list1 list2 [];;

let merge_sort list =
  let rec merge_sort list =
    match list with
      | [] -> []
      | h :: [] -> list
      | h :: tail -> merge (merge_sort [h]) (merge_sort tail)
  in merge_sort list;;
