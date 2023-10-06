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
let zeroes n m = Array.make_matrix n m 0;;

print_string "matrice 0 4x6:\n";;
print_matrix (zeroes 4 6);;

(* matrice n * n con la diagonale a 1 (matrice identità) *)
let identity size =
  let rec make_identity result cur_row =
    match cur_row with
      | 0 -> Array.of_list result
      | _ -> make_identity ((Array.init size (fun n -> if n = (cur_row-1) then 1 else 0)) :: result) (cur_row-1)
  in make_identity [] size;;

print_string "matrice identità 4x4:\n";;
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
        | _ -> make_init (Array.append result [|make_row row_start n|]) n (row_start+n) (remaining-1)
    in make_init [||] n 0 n;;

print_string "matrice 4x4 con i primi 4x4 numeri interi:\n";;
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

print_string "matrice trasposta della matrice 4x4 con i primi 4x4 numeri interi:\n";;
print_matrix (transponse (init 4));;

(* test matrice trasposta non quadrata *)
print_string "trasposta della matrice non quadrata:\n";;
print_matrix (transponse [|[|1;2;3|];[|4;5;6|]|]);;

(* somma tra matrici della stessa dimensione (non per forza quadrate) *)
let matrix_sum m1 m2 = 
  let arrays_sum a1 a2 = Array.map2 (fun a b -> a+b) a1 a2
in Array.map2 (fun a1 a2 -> arrays_sum a1 a2) m1 m2;;

print_string "somma tra due matrici 3x3 con i primi 3x3 interi:\n";;
print_matrix (matrix_sum (init 3) (init 3));;
print_string "somma tra due matrici non quadrate:\n";;
print_matrix (matrix_sum [|[|1;2;3|];[|4;5;6|]|] [|[|6;5;4|];[|3;2;1|]|]);;

(* moltiplicazione tra matrici, non per forza quadrate ma compatibili tra loro (num colonne di una = nom righe dell'altra) *)
let matrix_mult m1 m2 =
  (* moltiplicazione di tutti i valori di una riga *)
  let mul_row row matrix2 =
    (* get una colonna di una matrice: restitusce un array (la colonna) *)
    let get_column matrix column =
      let rec get_column matrix result col_index row =
        match row with
          | 0 -> Array.of_list result
          | _ -> get_column matrix (matrix.(row-1).(col_index) :: result) col_index (row-1)
      in get_column matrix [] column (Array.length matrix)
      
    (* calcola il risultato della moltiplicazione per una cella (tra riga e colonna): restituisce un intero *)
    and calculate_value row column =
      let calc row column =
          let mult row column = Array.map2 (fun a b -> a * b) row column
          and sum arr = Array.fold_left (+) 0 arr
        in sum (mult row column)
      in calc row column
    in
      (* moltiplica tutti i valori di una riga (attraverso calculate_value): restituisce un array (la riga moltiplicata) *)
      let rec mul_row row matrix2 column result = 
        match column with
          | 0 -> Array.of_list result
          | _ -> mul_row row matrix2 (column-1) ((calculate_value row (get_column matrix2 (column-1))) :: result)
      in mul_row row matrix2 (Array.length matrix2.(0)) []
    in
      (* moltiplica due matrici, attraverso la moltiplica di tante rige (attravers matrix_mult): restituisce una matrice *)
      let rec matrix_mult matrix1 matrix2 result row =
        match row with
          | 0 -> Array.of_list result
          | _ -> matrix_mult matrix1 matrix2 ((mul_row matrix1.(row-1) matrix2) :: result) (row-1)
      in matrix_mult m1 m2 [] (Array.length m1);;

print_string "moltiplicazione tra due matrici non quadrate:\n";;
print_matrix (matrix_mult [|[|1;2;3|];[|4;5;6|]|] [|[|10;11|];[|20;21|];[|30;31|]|]);;
