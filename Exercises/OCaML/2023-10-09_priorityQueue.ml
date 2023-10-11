module PrioQueue :
  sig
    type priority = int (* concrete *)
    type char_queue (* abstract *)
    
    val empty : char_queue
    val insert : char_queue -> int -> char -> char_queue
    val extract : char_queue -> int * char * char_queue
    exception QueueIsEmpty
  end =
  struct
    type priority = int
    type char_queue = Empty | Node of priority * char * char_queue * char_queue
    exception QueueIsEmpty
    
    let empty = Empty
    
    let rec insert queue newPrio newElem =
      match queue with
        | Empty -> Node(newPrio, newElem, Empty, Empty)
        | Node(headPrio, headElem, left, right) ->
          if newPrio <= headPrio
            then Node(newPrio, newElem, insert right headPrio headElem, left)
            else Node(headPrio, headElem, insert right newPrio newElem, left)
            
    let rec remove_top queue =
      match queue with
        | Empty -> raise QueueIsEmpty
        | Node(prio, elem, left, Empty) -> left
        | Node(prio, elem, Empty, right) -> right
        | Node(prio, elem, (Node(leftPrio, leftElem, _, _) as left),
                           (Node(rightPrio, rightElem, _, _) as right)) ->
            if leftPrio <= rightPrio
              then Node(leftPrio, leftElem, remove_top left, right)
              else Node(rightPrio, rightElem, left, remove_top right)
    
    let extract queue =
      match queue with
        | Empty -> raise QueueIsEmpty
        | Node(prio, elem, _, _) as queue -> (prio, elem, remove_top queue)
  end;;

let pq = PrioQueue.empty;;
let pq = PrioQueue.insert pq 10 'a';;
let pq = PrioQueue.insert pq 35 'b';;
let pq = PrioQueue.insert pq 4 'c';;
let priority, head, pq = PrioQueue.extract pq;; (* 4, 'c' *)
