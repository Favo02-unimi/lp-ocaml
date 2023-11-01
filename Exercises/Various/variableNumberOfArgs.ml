(* ------------------------ *)
(* implementazione semplice *)


(* operazione da eseguire su ogni parametro *)
let op x y = x + y;;
(* inizializzazione dell'accumulatore *)
let init = 0;;

(* singolo parametro *)
let arg x = (fun y rest -> rest (op x y));;
(* terminazione catena parametri *)
let stop x = x;;
(* inizializzazione funzione con numero di parametri variabile *)
let f g = g init;;

f (arg 1) stop;; (* 1 *)
f (arg 10) (arg 20) stop;; (* 30 *)
f (arg 100) (arg 200) (arg 300) stop;; (* 600 *)

(* --------------------------- *)
(* implementazione con funtore *)

(* interfaccia con operazioni e tipi necessari per il funzionamento di VarArgs *)
module type OpVarADT =
  sig
    type a and b and c
    val op: a -> b -> c
    val init : c
  end;;

(* funtore che rappresenta la struttura vera e propria *)
module VarArgs (OP : OpVarADT) =
  struct
    let arg x = fun y rest -> rest (OP.op x y)
    let stop x = x
    let f g = g OP.init
  end;;

module Sum = struct
  type a=int and b=int and c=int
  let op = fun x y -> x+y
  let init = 0
end;;

module VASum = VarArgs(Sum);;
VASum.f (VASum.arg 1) (VASum.stop);; (* 1 *)
VASum.f (VASum.arg 1) (VASum.arg 5) (VASum.stop);; (* 6 *)

module StringConcat = struct
  type a=string and b=string list and c=string list
  let op = fun (x: string) y -> y @ [x]
  let init = []
end;;

module VAStr = VarArgs(StringConcat);;
VAStr.f (VAStr.arg "Ciao") (VAStr.stop);; (* ["Ciao"] *)
VAStr.f (VAStr.arg "Hel") (VAStr.arg "lo") (VAStr.stop);; (* ["Hel"; "lo"] *)
