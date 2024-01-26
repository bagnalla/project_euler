(** Pretty straightforward brute force. We observe that it's only
    necessary to check sets [A] and [B] for equality when [|A| = |B|]
    and it is *not* the case that [Aᵢ < Bᵢ] or [Bᵢ < Aᵢ] for all [i]
    (assuming [A] and [B] are both sorted). We count how many disjoint
    subset pairs satisfy this property and then divide by [2] since
    every pair is counted twice (e.g., [a, b] and [b, a]). *)

open Core

(** True iff sets [a] and [b] are disjoint ([a ∩ b = ∅]). *)
let is_disjoint (a : ('a, 'comp) Set.t) (b : ('a, 'comp) Set.t) : bool =
  Set.is_empty @@ Set.inter a b

module IntSet = Set.Make(Int)

(** Power set of [s] minus the empty set ([2ˢ \ { ∅ }]). *)
let rec all_subsets (s : IntSet.t) : IntSet.t list =
  if Set.length s = 0 then
    []
  else
    let x = Set.choose_exn s in
    let xs = Set.remove s x in
    let ss = all_subsets xs in
    IntSet.singleton x :: ss @ List.map ss ~f:(fun s' -> Set.add s' x)

(** Cartesian product [a × b] of lists [a] and [b]. *)
let cartesian (a : 'a list) (b : 'a list) = 
  List.concat (List.map ~f:(fun x -> List.map ~f:(fun y -> (x, y)) b) a)

(** All pairs of non-empty disjoint subsets of [s], without symmetric
    duplicates (i.e., if (a, b) is in the list then (b, a) is not). *)
let disjoint_subsets (s : IntSet.t) : (IntSet.t * IntSet.t) list =
  let subsets = all_subsets s in
  let ss = List.concat_map subsets ~f:(fun a ->
               List.map (all_subsets @@ Set.diff s a) ~f:(fun b -> (a, b))) in
  print_endline @@ string_of_int @@ List.length ss / 2;
  ss

let () =
  let n = 12 in
  let ixs = List.init n ~f:Fn.id in
  let ix_subset_pairs = disjoint_subsets @@ IntSet.of_list ixs in
  print_endline @@ string_of_int @@
    List.count ix_subset_pairs
      ~f:(fun (a, b) ->
        let la = Set.to_list a in
        let lb = Set.to_list b in
        List.length la = List.length lb &&
          not (List.for_all (List.zip_exn la lb) ~f:(fun (i, j) -> i < j)) &&
            not (List.for_all (List.zip_exn lb la) ~f:(fun (i, j) -> i < j))) / 2
