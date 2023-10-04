(* stampa una matrice *)
let print_matrix matrix =
  let print_array array =
    print_string "[ ";
    Array.iter (fun x -> Printf.printf "%d " x) array;
    print_string "]\n";
    ()
  in
    Array.iter (fun row -> print_array row) matrix;
    print_string "\n";
    ();;

(* matrice n * m tutta a 0 *)
let zeroes n m =
  Array.make_matrix n m 0;;

print_matrix (zeroes 4 6);;

(* matrice n * n con la diagonale a 1 (matrice identità) *)
let identity n =
  let rec make_identity matrix row =
    match row with
      | 0 -> matrix
      | _ -> matrix.(row-1).(row-1) <- 1; make_identity matrix (row-1)
  in make_identity (zeroes n n) n;;

print_matrix (identity 4);;

(* matrice n * n riempita dei primi n*n numeri interi *)
let init n =
  (* costruisce una singola riga dato numero iniziale della riga (start) e la grandezza (n) *)
  let make_row start n =
    let rec make_row result current remaining =
      match remaining with
        | 0 -> Array.of_list result
        | _ -> make_row (result @ [current+1]) (current+1) (remaining-1)
    in make_row [] start n
  in
    (* costruisce una matrice n*n utilizzando make_row *)
    let rec make_init result n row_start remaining =
      match remaining with
        | 0 -> result
        | _ -> make_init (Array.concat [result; [|make_row row_start n|]]) n (row_start+n) (remaining-1)
    in make_init [||] n 0 n;;

print_matrix (init 4);;

(* matrice trasposta di una matrice *)
let transponse matrix =
  let get_column matrix column =
    let rec get_column matrix result col_index row =
      match row with
        | 0 -> Array.of_list result
        | _ -> get_column matrix (matrix.(row-1).(col_index) :: result) col_index (row-1)
    in get_column matrix [] column (Array.length matrix)
  in
    let rec transponse matrix result cur_column =
      match cur_column with
        | 0 -> result
        | _ -> transponse matrix (Array.append [|(get_column matrix (cur_column-1))|] result) (cur_column-1)
    in transponse matrix [||] (Array.length matrix.(0));;

print_matrix (transponse (init 4));;

(* test matrice trasposta non quadrata *)
print_matrix [|[|1;2;3|];[|4;5;6|]|];;
print_matrix (transponse [|[|1;2;3|];[|4;5;6|]|]);;

(* somma tra matrici della stessa dimensione (non per forza quadrate) *)
let matrix_sum m1 m2 = 
  let arrays_sum a1 a2 = Array.map2 (fun a b -> a+b) a1 a2
in Array.map2 (fun a1 a2 -> arrays_sum a1 a2) m1 m2;;

print_matrix (matrix_sum (init 3) (init 3));;
print_matrix (matrix_sum [|[|1;2;3|];[|4;5;6|]|] [|[|6;5;4|];[|3;2;1|]|]);;

