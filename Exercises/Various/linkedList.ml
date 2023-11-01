module LinkedList =
  struct

    type 'a node =
      | Elem of ('a * 'a node)
      | Null

    let create () = Null

    let rec append (head : 'a node) (neww : 'a) : ('a node) =
      match head with
        | Null -> Elem (neww, Null)
        | Elem (a, Null) -> Elem (a, (Elem (neww, Null)))
        | Elem (a, b) -> Elem (a, (append b neww))

    let rec iter (head : 'a node) (f : ('a -> unit)) : unit =
      match head with
        | Null -> ()
        | Elem (a, b) -> f a; iter b f

  end;;

let ll = LinkedList.create();;
let ll = LinkedList.append ll 5;;
let ll = LinkedList.append ll 3;;
let ll = LinkedList.append ll 2;;
let ll = LinkedList.append ll 1;;
let ll = LinkedList.append ll 0;;
let ll = LinkedList.append ll (-1);;
let ll = LinkedList.append ll (-3);;
LinkedList.iter ll (fun x -> print_int x; print_string "\n");;
