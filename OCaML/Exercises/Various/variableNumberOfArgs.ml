(* operazione da eseguire su ogni parametro *)
let op x y = x + y;;
(* inizializzazione dell'accumulatore *)
let acc = 0;;

(* singolo parametro *)
let arg x = (fun y rest -> rest (op x y));;
(* terminazione catena parametri *)
let stop x = x;;
(* inizializzazione funzione con numero di parametri variabile *)
let f g = g acc;;

f (arg 1) stop;; (* 1 *)
f (arg 10) (arg 20) stop;; (* 30 *)
f (arg 100) (arg 200) (arg 300) stop;; (* 600 *)
