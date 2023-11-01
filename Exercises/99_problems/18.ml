(* Extract a slice from a list. *)

let slice list start endd =
  let rec slice list count res =
    match list with
      | [] -> res
      | h :: tail when count < start -> (slice [@tailcall]) tail (count+1) res
      | h :: tail when count <= endd -> (slice [@tailcall]) tail (count+1) (h::res)
      | _ -> res
  in List.rev(slice list 0 []);;

slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
(* - : string list = ["c"; "d"; "e"; "f"; "g"] *)
