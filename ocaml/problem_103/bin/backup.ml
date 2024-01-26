open Core

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree list [@@deriving map]

let rec sexp_of_tree (show : 'a -> string) : 'a tree -> string = function
  | Leaf x -> "(" ^ show x ^ ")"
  | Node (x, xs) ->
     "(" ^ show x ^ " " ^
       String.concat (List.map xs ~f:(sexp_of_tree show)) ~sep:" " ^ ")"

(** All [k]-combinations from set [s]. *)
let rec choose (k : int) (s : ('a, 'comp) Set.t) : (('a, 'comp) Set.t) list =
  if k <= 0 then [Set.empty (Set.comparator_s s)] else
    if Set.is_empty s then [] else
      Set.fold s ~init:[] ~f:(fun acc x ->
          acc @ List.map ~f:(fun s -> Set.add s x)
                  (choose (k-1) (Set.filter s ~f:(fun y -> y > x))))

(* let rec all_subsets (l : 'a list) : 'a list list = *)
(*   match l with *)
(*   | [] -> [] *)
(*   | x :: xs -> *)
(*      let ss = all_subsets xs in *)
(*      [x] :: ss @ List.map ss ~f:(List.cons x) *)

(* let rec all_subsets (l : 'a list) : ('a, 'comp) Set.t list = *)
(*   match l with *)
(*   | [] -> [] *)
(*   | x :: xs -> *)
(*      let ss = all_subsets xs in *)
(*      (\* [x] :: ss @ List.map ss ~f:(List.cons x) *\) *)
(*      (\* Set.union ss (Set.map ss ~f:(List.cons x)) *\) *)
(*      Set.singleton (Set.comparator) x :: ss @ List.map ss ~f:(fun s' -> Set.insert s' x) *)

let rec subset_tree (s : ('a, 'comp) Set.t) : ('a, 'comp) Set.t tree =
(* Leaf (Set.empty @@ Set.comparator_s s) *)
  if Set.length s <= 1 then
    Leaf s
  else
    Node (s, (List.map (choose (Set.length s - 1) s) ~f:subset_tree))

let string_of_set (show : 'a -> string) (s : ('a, 'comp) Set.t) : string =
  String.concat (List.map (Set.to_list s) ~f:show) ~sep:" "

(* let is_disjoint (a : ('a, 'comp) Set.t) (b : ('a, 'comp) Set.t) : bool = *)
(*   Set.is_empty @@ Set.inter a b *)

module IntSet = Set.Make(Int)

(* let rec all_subsets (l : 'a list) : ('a, 'comp) Set.t list = *)
(*   match l with *)
(*   | [] -> [] *)
(*   | x :: xs -> *)
(*      let ss = all_subsets xs in *)
(*      IntSet.singleton x :: ss @ List.map ss ~f:(fun s' -> Set.add s' x) *)

let rec all_subsets (l : IntSet.t) : IntSet.t list =
  if Set.length l = 0 then
    []
  else
    let x = Set.choose_exn l in
    let xs = Set.remove l x in
    let ss = all_subsets xs in
    IntSet.singleton x :: ss @ List.map ss ~f:(fun s' -> Set.add s' x)

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
    ))

(* let next_approx (l : int list) : int list = *)
(*   let mid = List.nth_exn l @@ List.length l / 2 in *)
(*   mid :: List.map l ~f:(Int.(+) mid) *)

(* (\** All possible partitions of [n] into a sum of [k] integers all *)
(*     greater than [lb]. *\) *)
(* let rec partitions (n : int) (k : int) (lb : int) : int list list = *)
(*   if n <= lb then *)
(*     [] *)
(*   else if k <= 1 then *)
(*     [[n]] *)
(*   else *)
(*     List.concat @@ List.init (n - lb) ~f:(fun i -> *)
(*                        let j = lb + i + 1 in *)
(*                        let ps = partitions (n - j) (k - 1) j in *)
(*                        List.map ps ~f:(List.cons j)) *)

(** All possible partitions of [n] into a sum of [k] integers all
    greater than [lb]. *)
let rec partitions (n : int) (k : int) (lb : int) : int list list =
  if n <= lb then
    []
  else if k <= 1 then
    [[n]]
  else
    List.concat @@ List.init (n - lb) ~f:(fun i ->
                       let j = lb + i + 1 in
                       let ps = partitions (n - j) (k - 1) j in
                       List.map ps ~f:(List.cons j))
    

let () =
  
  (* let t = subset_tree @@ IntSet.of_list [3; 5; 6; 7] in *)
  (* print_endline @@ sexp_of_tree (string_of_set string_of_int) t; *)

  (* (\* let sum_t : int tree = map_tree (fun x -> Set.sum (module Int) x ~f:(Fn.id)) t in *\) *)
  (* let sum_t : int tree = map_tree sum t in *)
  (* print_endline @@ sexp_of_tree string_of_int sum_t; *)

  let l = IntSet.of_list [11; 18; 19; 20; 22; 25] in
  
  (* let ss = all_subsets l in *)
  (* List.iter ss ~f:(fun s -> Set.iter s ~f:(fun x -> *)
  (*                               print_string @@ string_of_int x ^ " "); *)
  (*                           print_endline ""); *)
  
  print_endline @@ string_of_bool @@ is_special_sum_set l;

  (* let s = IntSet.of_list @@ next_approx @@ Set.to_list l in *)
  (* Set.iter s ~f:(fun x -> print_string @@ string_of_int x ^ " "); *)
  (* print_endline ""; *)

  let mid = List.nth_exn (Set.to_list l) @@ Set.length l / 2 in
  let s = Set.add (IntSet.map l ~f:(Int.(+) mid)) mid in
  
  print_endline @@ string_of_bool @@ is_special_sum_set s;
  print_endline @@ string_of_int @@ sum s;

  let ps = partitions (sum s - mid - 84) 6 (mid - 1) in
  (* List.iter ps ~f:(fun p -> List.iter p ~f:(fun x -> *)
  (*                               print_string @@ string_of_int x ^ " "); *)
  (*                           print_endline "") *)

  List.iter ps ~f:(fun p ->
      let s = IntSet.of_list p in
      if is_special_sum_set s then
        print_endline @@ string_of_set s ^ " " ^ string_of_int @@ sum s;
                   
    )
