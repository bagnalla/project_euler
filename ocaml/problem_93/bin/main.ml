(** Brute force: for each 4-set of digits, enumerate all possible
    expression terms and evaluate them and check how many consecutive
    integers appear. There's some redundant computation because some
    syntactically distinct expressions are equivalent under evaluation
    (e.g., [1 + 2] and [2 + 1]). We could do some work to avoid this
    but it doesn't matter because the solution is plenty fast enough
    as is. *)

open Core

(** Type of arithmetic expressions involving integers. *)
type exp =
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Mult of exp * exp
  | Div of exp * exp [@@deriving sexp]

open Option.Applicative_infix
let (let*) = Option.Let_syntax.(>>=)

(** Evaluate an expression. Must evaluate to floats to get the right
    answer because, although we're looking for integer results in the
    end, intermediate results during evaluation can be non integers. *)
let rec eval : exp -> float option = function
  | Num   n        -> Some (float_of_int n)
  | Plus  (e1, e2) -> Option.return ( +. ) <*> eval e1 <*> eval e2
  | Minus (e1, e2) -> Option.return ( -. ) <*> eval e1 <*> eval e2
  | Mult  (e1, e2) -> Option.return ( *. ) <*> eval e1 <*> eval e2
  | Div   (e1, e2) -> let* n1 = eval e1 in
                      let* n2 = eval e2 in
                      if Float.( <> ) n2 0.0 then Some (n1 /. n2) else None

(** All k-combinations from given set s. *)
let rec choose (k : int) (s : ('a, 'comp) Set.t) : (('a, 'comp) Set.t) list =
  if k <= 0 then [Set.empty (Set.comparator_s s)] else
    if Set.is_empty s then [] else
      Set.fold s ~init:[] ~f:(fun acc x ->
          acc @ List.map ~f:(fun s -> Set.add s x)
                  ((choose (k-1) (Set.filter s ~f:(fun y -> y > x)))))

module IntSet = Core.Set.Make(Int)

(* let to_string : IntSet.t -> string = Fn.compose Sexp.to_string IntSet.sexp_of_t *)

(** Cartesian product of two sets. *)
let cartesian l l' = 
  List.concat (List.map ~f:(fun e -> List.map ~f:(fun e' -> (e,e')) l') l)

open List.Monad_infix

(** Generate a list of all possible expressions that contain each
    integer from the given set [s] exactly once. *)
let rec gen (s : IntSet.t) : exp list =
  if Set.length s = 1 then
    [Num (Set.choose_exn s)]
  else
    List.init (Set.length s - 1) ~f:(fun n -> n + 1) >>= fun n ->
    choose n s >>= fun u ->
    cartesian (gen u) (gen @@ Set.diff s u) >>= fun (a, b) ->
    [Plus (a, b); Minus (a, b); Mult (a, b); Div (a, b)]

(** Throw away [None]s.*)
let collect : 'a option list -> 'a list =
  let rec go acc = function
  | [] -> acc
  | Some x :: l -> go (x :: acc) l
  | None :: l -> go acc l
  in go []

(** Float approximate equality. *)
let eq (a : float) (b : float) : bool =
  Float.( <= ) (Float.abs (a -. b)) 0.0001

(** Count the number of consecutive integers starting from [1] in list [ns]. *)
let count (ns : float list) : int =
  let rec go (n : int) : int =
    if List.mem ns (float_of_int n) ~equal:eq then go (n+1) else n-1 in
  go 1

let () =
  (** All possible 4-sets of digits (omitting 0). *)
  let digit_sets = choose 4 @@ IntSet.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in

  (** Associate to each set of digits the number of consecutive
      integers from [1] obtained from the arithmetic expressions. *)
  let pairs = List.map digit_sets ~f:(fun ds ->
                  ds, count (List.filter
                               (collect @@ List.map (gen ds) ~f:eval)
                               ~f:(fun n -> Float.( < ) 0.0 n))) in

  (** Sort them. *)
  let sorted = List.sort pairs ~compare:(fun (_, n) (_, m) -> Int.compare n m) in

  (** Print them out. The answer is the last one. *)
  List.iter sorted ~f:(fun (ds, n) ->
      print_endline @@ Sexp.to_string (IntSet.sexp_of_t ds) ^ ": " ^ string_of_int n)
