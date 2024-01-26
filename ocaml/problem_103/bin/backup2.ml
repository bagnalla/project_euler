
open Core
open Lp

(** All [k]-combinations from set [s]. *)
let rec choose (k : int) (s : ('a, 'comp) Set.t) : (('a, 'comp) Set.t) list =
  if k <= 0 then [Set.empty (Set.comparator_s s)] else
    if Set.is_empty s then [] else
      Set.fold s ~init:[] ~f:(fun acc x ->
          acc @ List.map ~f:(fun s -> Set.add s x)
                  (choose (k-1) (Set.filter s ~f:(fun y -> y > x))))

let string_of_set (show : 'a -> string) (s : ('a, 'comp) Set.t) : string =
  String.concat (List.map (Set.to_list s) ~f:show) ~sep:" "

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

let sum (s : IntSet.t) : int =
  Set.fold s ~init:0 ~f:( + )

let string_of_set (s : IntSet.t) =
  Sexp.to_string @@ IntSet.sexp_of_t s

let is_special_sum_set (s : IntSet.t) : bool =
  let subsets = all_subsets s in
  List.for_all subsets ~f:(fun x ->
      List.for_all (all_subsets @@ Set.diff s x) ~f:(fun y ->
          let sx = sum x in
          let sy = sum y in
          let prop1 = sx <> sy in
          let prop2 = not (Set.length x < Set.length y) || sx < sy in
          (* if not prop1 then *)
          (*   print_endline @@ *)
          (*     "prop1 violated by " ^ string_of_set x ^ " and " ^ string_of_set y; *)
          (* if not prop2 then *)
          (*   print_endline @@ *)
          (*     "prop2 violated by " ^ string_of_set x ^ " and " ^ string_of_set y; *)
          prop1 && prop2
        )
    )

(* let constr_of_subset_pair (vars : int -> Var.t) (a : IntSet.t) (b : IntSet.t) : Cnstr.t = *)
(*   if Set.length a < Set.length b then *)
(*     c 1.0 <~ c 2.0 *)
(*   else if Set.length b < Set.length a then *)
(*     c 1.0 <~ c 2.0 *)
(*   else *)
(*     c 1.0 <~ c 2.0 *)

let poly_sum (l : Poly.t list) : Poly.t =
  List.fold l ~init:zero ~f:(fun acc x -> acc ++ x)

let gensym_counter = ref 0
let gensym (s : string) : string =
  let x = s ^ string_of_int !gensym_counter in
  gensym_counter := !gensym_counter + 1;
  x

(* let constr_of_subset_pair (a : Poly.t list) (b : Poly.t list) : Cnstr.t = *)
(*   if List.length a < List.length b then *)
(*     poly_sum a <~ poly_sum b -- c 1.0 *)
(*   else if List.length b < List.length a then *)
(*     poly_sum b <~ poly_sum a -- c 1.0 *)
(*   else *)
(*     (\* let x = var @@ gensym "_x" in *\) *)
(*     (\* let diff = poly_sum a -- poly_sum b in *\) *)
(*     (\* ~-- (diff *~ diff) <~ c 1.0  *\) *)
(*     let z = binary @@ gensym "_x" in *)
(*     let x = poly_sum a in *)
(*     let y = poly_sum b in *)

(* let constrs_of_subset_pair (a : Poly.t list) (b : Poly.t list) : Cnstr.t list = *)
(*   if List.length b - List.length a = 1 then *)
(*     [poly_sum a <~ poly_sum b -- one] *)
(*   else if List.length a - List.length b = 1 then *)
(*     [poly_sum b <~ poly_sum a -- one] *)
(*   else if List.length a = List.length b then *)
(*     let z = binary @@ gensym "z" in *)
(*     let x = poly_sum a in *)
(*     let y = poly_sum b in *)
(*     let m = c 1000.0 in *)
(*     let e = x -- y ++ m *~ z in *)
(*     [e >~ one; *)
(*      e <~ m -- one] *)
(*   else *)
(*     [] *)

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

(* let constrs_of_subset_pair (vars : int -> Poly.t) (a : int list) (b : int list) : Cnstr.t list = *)
(*   let poly_a = poly_sum @@ List.map a ~f:vars in *)
(*   let poly_b = poly_sum @@ List.map b ~f:vars in *)
(*   if List.length b - List.length a = 1 then *)
(*     [poly_a <~ poly_b -- one] *)
(*   else if List.length a - List.length b = 1 then *)
(*     [poly_b <~ poly_a -- one] *)
(*   else if List.length a = List.length b then *)
(*     let sum_a = List.fold a ~init:0 ~f:(+) in *)
(*     let sum_b = List.fold b ~init:0 ~f:(+) in *)
(*     (\* if sum_a = sum_b then *\) *)
(*     (\*   let z = binary @@ gensym "z" in *\) *)
(*     (\*   let x = poly_a in *\) *)
(*     (\*   let y = poly_b in *\) *)
(*     (\*   let m = c 1000.0 in *\) *)
(*     (\*   let e = x -- y ++ m *~ z in *\) *)
(*     (\*   [e >~ one; *\) *)
(*     (\*    e <~ m -- one] *\) *)
(*     if sum_a < sum_b then *)
(*       [poly_a <~ poly_b -- one] *)
(*     else *)
(*       [poly_b <~ poly_a -- one] *)
(*       (\* let min_a = List.fold a ~init:1000 ~f:Int.min in *\) *)
(*       (\* let min_b = List.fold b ~init:1000 ~f:Int.min in *\) *)
(*       (\* if min_a < min_b then *\) *)
(*       (\*   [poly_a <~ poly_b -- one] *\) *)
(*       (\* else *\) *)
(*       (\*   [poly_b <~ poly_a -- one] *\) *)
(*   else *)
(*     [] *)

(* let write problem = Lp.write "my_problem.lp" problem *)

(* let solve problem = *)
(*   (\* For other interfaces, use Lp_glpk_js or Lp_gurobi instead *\) *)
(*   match Lp_glpk.solve problem with *)
(*   | Ok (obj, xs) -> *)
(*       Printf.printf "Objective: %.2f\n" obj ; *)
(*       Printf.printf "x: %.2f y: %.2f\n" *)
(*         (Lp.PMap.find x xs) (Lp.PMap.find y xs) *)
(*   | Error msg -> *)
(*      print_endline msg *)

let () =

  let n = 7 in
  
  let ixs = List.init n ~f:Fn.id in
  let ix_subset_pairs = disjoint_subsets @@ IntSet.of_list ixs in

  (* List.iter ix_subset_pairs ~f:(fun (a, b) -> *)
  (*     print_endline @@ string_of_set a ^ " " ^ string_of_set b) *)

  
  let vars = List.map ixs ~f:(fun i -> var ("x" ^ string_of_int i)
                              (* ~integer:true ~lb:20.0 ~ub:50.0) in *)
                              (* ~integer:true ~ub:30.0) in *)
                                         ~integer:true) in
  
  let var_subset_pairs = List.map ix_subset_pairs ~f:(fun (a, b) ->
                             (List.map (Set.to_list a) ~f:(List.nth_exn vars),
                              List.map (Set.to_list b) ~f:(List.nth_exn vars))) in

  let objective = minimize @@ poly_sum vars in
  
  let constrs = List.init (n-1) ~f:(fun i -> List.nth_exn vars i <~ List.nth_exn vars (i+1) -- one) @
                  List.concat_map var_subset_pairs ~f:(fun (a, b) -> constrs_of_subset_pair a b) in
  
  (* let constrs = List.concat_map ix_subset_pairs ~f:(fun (a, b) -> *)
  (*                   constrs_of_subset_pair (fun i -> *)
  (*                       List.nth_exn vars i) (Set.to_list a) (Set.to_list b)) in *)
  let problem = make objective constrs in
  
  if validate problem then
    begin
      write "prob.lp" problem;
      match Lp_glpk.solve problem with
      | Ok (obj, xs) ->
         List.iter vars ~f:(fun x -> print_endline @@ string_of_float @@ Lp.PMap.find x xs);
         print_endline @@
           String.concat (List.map vars ~f:(fun x ->
                              string_of_int @@ int_of_float @@ Lp.PMap.find x xs)) ~sep:""
      | Error msg ->
         print_endline msg
    end
  else
    print_endline "sum ting wong"
