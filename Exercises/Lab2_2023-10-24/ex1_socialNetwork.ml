module type GraphADT =
  sig
    type 'a graph

    val create : unit -> 'a graph

    val add_node : 'a -> 'a graph -> 'a graph

    val add_edge : 'a -> 'a -> 'a graph -> 'a graph

    val get_nodes : 'a graph -> 'a list

    val get_edges : 'a graph -> ('a * 'a) list

    val adjacents : 'a -> 'a graph -> 'a list

    val visit : 'a graph -> ('a * 'a list) list
  end;;

module Graph : GraphADT =
  struct

    (* type graph: list of nodes ('a list) and list of edges (('a * 'a) list) *)
    type 'a graph =
      | Graph of ('a list * (('a * 'a) list))

    exception NodeNotFoundError

    (* create new graph: empty nodes and edges *)
    let create () = Graph ([], [])

    (* utility function: add an element to a list only if it is not already in (like a set) *)
    let add_to_set list elem =
      match (List.exists (fun x -> x = elem) list) with
        | true -> list
        | false -> elem :: list

    (* add a note to graph: add node to list of nodes (using add_to_set to avoid duplicates) *)
    let add_node node graph =
      match graph with
        | Graph (nodes, edges) -> Graph ((add_to_set nodes node), edges)

    (* utility function: check if a node exists in the graph *)
    let exists_node nodes node = (List.exists (fun x -> x = node) nodes)

    (* add edge to graph: check if both nodes are in graph (throw NodeNotFoundError if not)
       and add to the edges list (using add_to_set to avoid duplicates) *)
    let add_edge node1 node2 graph =
      match graph with
        | Graph (nodes, edges) ->
            let valid1 = (exists_node nodes node1)
            and valid2 = (exists_node nodes node2)
            in match (valid1, valid2) with
              | (true, true) -> Graph (nodes, (add_to_set edges (node1, node2)))
              | _ -> raise NodeNotFoundError

    (* return the list of nodes *)
    let get_nodes graph =
      match graph with
        | Graph (nodes, _) -> nodes

    (* return the list of edges *)
    let get_edges graph =
      match graph with
        | Graph (_, edges) -> edges

    (* get all the adjacents to a node: filter all edges checking if the edge contains wanted node
       and map every filtered edge removing the node already known (('a * 'a) to 'a)
       example: adjacents of node 4 in edges [(4,3) (3,2) (9,4)]
       filter: get edges that have in it 4: [(4,3) (9,4)]
       map: upack tuples removing 4: [3 9] *)
    let adjacents node graph =
      match graph with
        | Graph (_, edges) ->
            List.map
              (* map tuples to only the edge that is not node *)
              (fun x -> if ((fst x) = node) then (snd x) else (fst x))
              (* filter only edges that have node in it *)
              (List.filter (fun x -> ((fst x) = node) || ((snd x) = node)) edges)

    (* visit graph: return adjacents to each node: call adjacent for each node in nodes
       and accumulate result using List.fold_left *)
    let visit graph =
      match graph with
        | Graph (nodes, edges) ->
          List.fold_left (fun acc x -> ((x, (adjacents x graph)) :: acc)) [] nodes
  end;;

(* test on integers *)
let g = Graph.create ();;
let g = Graph.add_node 5 g;;
let g = Graph.add_node 3 g;;
let g = Graph.add_node 5 g;;
let g = Graph.add_node 10 g;;
let g = Graph.add_node 114 g;;
let g = Graph.add_node 6 g;;
let g = Graph.add_node 9 g;;
let g = Graph.add_node 25 g;;
let g = Graph.add_edge 3 5 g;;
let g = Graph.add_edge 5 114 g;;
let g = Graph.add_edge 114 10 g;;
let g = Graph.add_edge 6 9 g;;
let g = Graph.add_edge 3 9 g;;
let g = Graph.add_edge 3 6 g;;
let g = Graph.add_edge 3 6 g;;
Graph.adjacents 3 g;;
Graph.adjacents 114 g;;
Graph.visit g;;

(* test on dummy social network *)
type person = {
  id : int;
  name : string;
  surname : string
}

let lf = { id=1; name="Luca"; surname="Favini" };;
let mb = { id=2; name="Michele"; surname="Bolis" };;

let g = Graph.create ();;
let g = Graph.add_node lf g;;
let g = Graph.add_node mb g;;
let g = Graph.add_edge lf mb g;;
Graph.visit g;;
