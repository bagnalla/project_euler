(** Apparently by coincidence the approximate "rule" given in the
    problem statement happens to yield the correct answer, but we
    can't know that for sure without solving the problem properly,
    which we do here using integer linear programming with binary
    variables (using the glpk backend of the OCaml lp library).

    We are looking for a solution vector [xₙ] with [n=7]. We first add
    constraints for [x_i < x_(i+1)] for all [i < n] (symmetry breaking
    constraints). This isn't necessary for correctness but cuts down
    the running time drastically.

    Then, for each pair [(U, V)] of non-empty disjoint subsets of
    [{1..n}] we do the following:

    If [|U| < |V|], constrain [sum {xᵢ | i ∈ U} < sum {xᵢ | i ∈ V}],
    else if [|V| < |U|], constrain [sum {xᵢ | i ∈ V} < sum {xᵢ | i ∈ U}],
    else (when [|U| = |V|]), constrain [sum {xᵢ | i ∈ V} ≠ sum {xᵢ | i ∈ U}].

    Actually, as a slight optimization we only add the first two kinds
    of constraints when [|U|] and [|V|] differ by exactly [1], since
    the rest will follow by automatically by transitivity.

    The third kind of constraint is a little more difficult because
    linear solvers don't directly support disequality constraints like
    [a ≠ b], but we can do it indirectly by introducing a fresh binary
    variable [z] and a set of constraints [C] such that when [z=1],
    [C] is satisfied iff [a < b], and when [z=0], [C] is satisfied iff
    [b < a], and [C] is unsatisfiable for any [z] when [a = b]. Thus
    the solver can choose a value of [z] to make the constraints
    satisfiable only when [a ≠ b]. We define [C] as follows:

    The third kind of constraint is a little more difficult because
    linear solvers don't directly support disequality constraints like
    [a ≠ b], but we can do it indirectly by introducing a fresh binary
    variable [z] and a set of constraints [C] such that [C] is
    satisfied when [z=1] and [a < b], or when [z=0] and [b < a], but
    when [a = b] [C] is unsatisfiable for any [z]. Thus the solver can
    choose a value of [z] to make the constraints satisfiable only
    when [a ≠ b]. We define [C] as follows:

    [1 ≤ a - b + z * m ≤ m - 1], where m is an upper bound on [|a-b|].

    When [a < b], the solver can choose [z=1] to make:
    [1 ≤ a - b + m ≤ m - 1] ⇔ [1 - m ≤ a - b ≤ -1],

    and when [b < a], the solver can choose [z=0] to make:
    [1 ≤ a - b ≤ m - 1].

    However, when [a = b], the inequalities
    [1 ≤ z * m ≤ m - 1]
    are not satisfiable for either choice of [z] because
    [1 ≤ 0] is false, and
    [m ≤ m - 1] is false.

    This solution runs in about 2 seconds in my Linux VM.

    A possible other solution is based on the observation that every
    subset of an SSS is also an SSS, thus it should suffice to build
    up a set of possible SSSs inductively (wrt. some fixed upper bound
    on the element values, e.g., 50). *)

open Core
open Lp

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

(** Remove all (a, b) in [l] when (b, a) occurs further in [l]. *)
let rec remove_symmetric_duplicates (eq : 'a -> 'a -> bool) (l : ('a * 'a) list)
        : ('a * 'a) list =
  match l with
  | [] -> []
  | (x, y) :: l' -> if List.exists l' ~f:(fun (x', y') -> eq x y' && eq y x') then
                      remove_symmetric_duplicates eq l'
                    else
                      (x, y) :: remove_symmetric_duplicates eq l'

(** All pairs of non-empty disjoint subsets of [s], without symmetric
    duplicates (i.e., if (a, b) is in the list then (b, a) is not). *)
let disjoint_subsets (s : IntSet.t) : (IntSet.t * IntSet.t) list =
  remove_symmetric_duplicates IntSet.equal @@
    List.filter (cartesian (all_subsets s) (all_subsets s))
      ~f:(fun (x, y) -> is_disjoint x y)

(** Sum of polynomials in [l]. *)
let poly_sum (l : Poly.t list) : Poly.t =
  List.fold l ~init:zero ~f:(fun acc x -> acc ++ x)

(** Gensym for generating fresh binary variables. *)
let gensym_counter = ref 0
let gensym (s : string) : string =
  let x = s ^ string_of_int !gensym_counter in
  gensym_counter := !gensym_counter + 1;
  x

(** Generate constraints corresponding to given non-empty disjoint
    sets of variables [a] and [b]. *)
let constrs_of_subset_pair (a : Poly.t list) (b : Poly.t list) : Cnstr.t list =
  let sum_a = poly_sum a in
  let sum_b = poly_sum b in
  if List.length b - List.length a = 1 then
    [sum_a <~ sum_b -- one]
  else if List.length a - List.length b = 1 then
    [sum_b <~ sum_a -- one]
  else if List.length a = List.length b then
    let z = binary @@ gensym "z" in
    let m = c 1000.0 in
    let e = sum_a -- sum_b ++ m *~ z in
    [e >~ one;
     e <~ m -- one]
  else
    []

let () =
  let n = 7 in
  let ixs = List.init n ~f:Fn.id in
  let ix_subset_pairs = disjoint_subsets @@ IntSet.of_list ixs in
  let vars = List.map ixs ~f:(fun i -> var ("x" ^ string_of_int i) ~integer:true) in
  let var_subset_pairs = List.map ix_subset_pairs ~f:(fun (a, b) ->
                             (List.map (Set.to_list a) ~f:(List.nth_exn vars),
                              List.map (Set.to_list b) ~f:(List.nth_exn vars))) in
  let objective = minimize @@ poly_sum vars in

  (* Enforce strict ordering on the variables and then the constraints
     imposed by subset sum relationships. *)
  let constrs = List.init (n-1) ~f:(fun i ->
                    List.nth_exn vars i <~ List.nth_exn vars (i+1) -- one) @
                  List.concat_map var_subset_pairs ~f:(fun (a, b) ->
                      constrs_of_subset_pair a b) in
  let problem = make objective constrs in
  
  if validate problem then
    begin
      write "prob.lp" problem;
      match Lp_glpk.solve problem with
      | Ok (_, xs) ->
         print_endline @@
           String.concat (List.map vars ~f:(fun x ->
                              string_of_int @@ int_of_float @@ Lp.PMap.find x xs))
      | Error msg ->
         print_endline msg
    end
  else
    print_endline "-_-"
