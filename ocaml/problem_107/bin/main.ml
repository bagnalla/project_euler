(** Obviously the problem is just asking for the minimum spanning tree
    of the input graph, so I figured I would take the opportunity to
    try out the ocamlgraph library. We use their implementation of
    Prim's algorithm. *)

open Core
open Graph

(** We use integers for both the vertices and edge labels (weights). *)
module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end

(** We use a persistent (immutable) graph type with concrete edges
    with integer labels (concrete so we know the labels are integers,
    otherwise the [weight] function below doesn't work because the
    type of [x] is held abstract rather than being exposed as [int]). *)
module G = Persistent.Graph.ConcreteLabeled(Int)(Int)

module W = struct
  type edge = G.E.t
  type label = G.E.label
  type t = int
  let weight (_, x, _: edge) : t = x
  let zero = 0
  let add = (+)
  let compare = compare
end

(** Instantiate Prim module functor with our graph and weight modules. *)
module P = Prim.Make(G)(W)

let () =
  (* Read in the file and build adjacency matrix. *)
  let lines = In_channel.read_lines "network.txt" in
  let adj_mat : int option list list =
    List.map lines
      ~f:(fun line -> List.map (String.split line ~on:',') ~f:(fun s ->
                          if String.(=) s "-" then
                            None
                          else
                            Some (int_of_string s))) in

  (* Compute sum of all weights (need to divide by two at the end due
     to symmetry of the matrix causing every edge to be counted twice). *)
  let total_weight = List.fold adj_mat ~init:0 ~f:(fun acc row ->
                         List.fold (List.map row ~f:(Option.value ~default:0))
                           ~init:acc ~f:(+)) / 2 in

  (* Create vertices. *)
  let vertices = List.init (List.length adj_mat) ~f:G.V.create in

  (* Create initial graph with all the vertices. *)
  let g0 = List.fold vertices ~init:G.empty ~f:(fun g v -> G.add_vertex g v) in

  (* Add edges. *)
  let g1 = List.foldi adj_mat ~init:g0 ~f:(fun i g row ->
               List.foldi row ~init:g ~f:(fun j g' o ->
                   match o with
                   | Some x ->
                      G.add_edge_e g' @@ G.E.create (List.nth_exn vertices i)
                                           x
                                           (List.nth_exn vertices j)
                   | None -> g'
                 )
             ) in

  (* Sanity check: print number of vertices and edges in the graph. *)
  (* print_endline @@ string_of_int @@ G.nb_vertex g1; *)
  (* print_endline @@ string_of_int @@ G.nb_edges g1; *)

  (* Compute the MST. *)
  let mst : G.E.t list = P.spanningtree g1 in

  (* Print edges in the MST. *)
  (* List.iter mst ~f:(fun (v1, w, v2) -> *)
  (*     print_endline @@ string_of_int v1 ^ " <-> " ^ string_of_int v2); *)

  (* Compute total weight of edges in MST *)
  let mst_total_weight = List.fold mst ~init:0 ~f:(fun acc (_, x, _) -> acc + x) in
  
  (* Print difference between graph total weight and MST total weight. *)
  print_endline @@ string_of_int @@ total_weight - mst_total_weight
