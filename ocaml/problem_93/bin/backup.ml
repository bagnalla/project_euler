(** TODO: need to deal with different parenthesizations as well. Maybe
    just use an AST type and enumerate all possible values wrt. a
    given set of digits. *)

open Core

type exp =
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Mult of exp * exp
  | Div of exp * exp [@@deriving sexp]

let rec string_of_exp : exp -> string = function
  | Num n -> string_of_int n
  | Plus (e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
  | Minus (e1, e2) -> "(" ^ string_of_exp e1 ^ " - " ^ string_of_exp e2 ^ ")"
  | Mult (e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"
  | Div (e1, e2) -> "(" ^ string_of_exp e1 ^ " / " ^ string_of_exp e2 ^ ")"

open Option.Applicative_infix
let (let*) = Option.Let_syntax.(>>=)
     
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

let to_string : IntSet.t -> string = Fn.compose Sexp.to_string IntSet.sexp_of_t

let cartesian l l' = 
  List.concat (List.map ~f:(fun e -> List.map ~f:(fun e' -> (e,e')) l') l)

open List.Monad_infix

let rec gen (s : IntSet.t) : exp list =
  if Set.length s = 1 then
    [Num (Set.choose_exn s)]
  else
    List.init (Set.length s - 1) ~f:(fun n -> n + 1) >>= fun n ->
    choose n s >>= fun u ->
    cartesian (gen u) (gen @@ Set.diff s u) >>= fun (a, b) ->
    [Plus (a, b); Minus (a, b); Mult (a, b); Div (a, b)]
    (* [Plus (a, b)] *)

(* let rec gen (s : IntSet.t) : exp list = *)
(*   if Set.length s = 1 then *)
(*     [Num (Set.choose_exn s)] *)
(*   else *)
(*     let digit_sets = choose (Set.length s - 1) s in *)
(*     List.concat_map digit_sets ~f:(fun u -> *)
(*         let lhs = gen u in *)
(*         let rhs = gen (Set.diff s u) in *)
(*         cartesian lhs rhs >>= fun (a, b) -> [] *)
(*       (\* List.concat_map (cartesian lhs rhs) ~f:(fun (a, b) -> *\) *)
(*       (\*     [] *\) *)
(*       (\*   ) *\) *)
(*       ) *)

(* let rec collect : 'a option list -> 'a list = function *)
(*   | [] -> [] *)
(*   | Some x :: l -> x :: collect l *)
(*   | None :: l -> collect l *)

let collect : 'a option list -> 'a list =
  let rec go acc = function
  | [] -> acc
  | Some x :: l -> go (x :: acc) l
  | None :: l -> go acc l
  in go []

let dedup (ns : int list) : int list =
  Set.to_list (IntSet.of_list ns)

(* let rec vals (digits : IntSet.t) : int list = *)
(*   if Set.length digits = 1 then [Set.choose_exn digits] else *)
(*     Set.fold digits ~init:[] ~f:(fun acc d -> *)
(*         acc @ List.concat_map (vals (Set.remove digits d)) *)
(*                 ~f:(fun n -> [d + n; d - n; d * n] @ *)
(*                                if n <> 0 then [d / n] else [])) *)

let eq (a : float) (b : float) : bool =
  Float.( <= ) (Float.abs (a -. b)) 0.0001

let f (ns : float list) : float =
  let rec go (n : float) : float =
    if List.mem ns n ~equal:eq then go (n +. 1.0) else n -. 1.0 in
  go 1.0

let () =
  (* (\* List.iter digit_sets ~f:(Fn.compose print_endline to_string); *\) *)
  
  (* (\* let ns : (int * int list) list = List.mapi digit_sets ~f:(fun i ds -> i, vals ds) in *\) *)
  (* (\* print_endline @@ string_of_int @@ List.length ns *\) *)

  (* let ns = vals @@ IntSet.of_list [1; 2; 3; 4] in *)
  (* (\* List.iter ns ~f:(Fn.compose print_endline string_of_int); *\) *)
  (* print_endline @@ string_of_int @@ List.length ns; *)
  (* print_endline @@ string_of_int @@ f ns *)
  (* (\* List.iter ns ~f:(Fn.compose print_endline string_of_int); *\) *)
  
  (* (\* let ns : int list = List.concat @@ List.map digit_sets ~f:vals in *\) *)
  (* (\* print_endline @@ string_of_int @@ List.length ns *\) *)

  (* let exps = digit_sets >>= gen in *)
  (* (\* (\\* List.iter exps ~f:(fun e -> print_endline @@ Sexp.to_string @@ sexp_of_exp e); *\\) *\) *)
  (* (\* (\\* List.iter exps ~f:(Fn.compose print_endline string_of_exp); *\\) *\) *)
  (* print_endline @@ string_of_int @@ List.length exps; *)

  (* let exps = gen (IntSet.of_list [1; 2; 3; 4]) in *)
  (* (\* List.iter exps ~f:(fun e -> print_endline @@ string_of_exp e); *\) *)
  (* print_endline @@ string_of_int @@ List.length exps; *)
  (* let ns = collect @@ List.map exps ~f:eval in *)
  (* print_endline @@ string_of_int @@ List.length ns; *)
  (* print_endline @@ string_of_int @@ f ns; *)
  (* let uniq_ns = List.filter (dedup ns) ~f:(fun n -> n > 0) in *)
  (* List.iter uniq_ns ~f:(Fn.compose print_endline string_of_int); *)
  (* print_endline @@ string_of_int @@ List.length uniq_ns *)

  let digit_sets = choose 4 @@ IntSet.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  (* let max_digits, max_n = *)
  (*   Option.value_exn @@ *)
  (*     List.max_elt (List.map digit_sets ~f:(fun ds -> *)
  (*                       ds, f (List.filter *)
  (*                                (collect @@ List.map (gen ds) ~f:eval) *)
  (*                                ~f:(fun n -> 0 < n)))) *)
  (*       ~compare:(fun (_, n) (_, m) -> Int.compare n m) in *)
  (* print_endline @@ Sexp.to_string @@ IntSet.sexp_of_t max_digits; *)
  (* print_endline @@ string_of_int @@ max_n *)

  (* List.iter digit_sets *)
  (*   ~f:(fun ds -> *)
  (*     let exps = gen ds in *)
  (*     let ns = collect @@ List.map exps ~f:eval in *)
  (*     let uniq_ns = List.filter (dedup ns) ~f:(fun n -> n > 0) in *)
  (*     (\* List.iter uniq_ns ~f:(Fn.compose print_endline string_of_int); *\) *)
  (*     (\* print_endline @@ string_of_int @@ List.length exps; *\) *)
  (*     (\* print_endline @@ string_of_int @@ List.length ns; *\) *)
  (*     (\* print_endline @@ string_of_int @@ List.length uniq_ns) *\) *)
  (*     print_endline @@ Sexp.to_string (IntSet.sexp_of_t ds) ^ ": " ^ string_of_int @@ f uniq_ns); *)

  (* print_endline @@ string_of_int @@ List.length digit_sets *)

  let pairs = List.map digit_sets ~f:(fun ds ->
                  ds, f (List.filter
                           (collect @@ List.map (gen ds) ~f:eval)
                           ~f:(fun n -> Float.( < ) 0.0 n))) in
  let sorted = List.sort pairs ~compare:(fun (_, n) (_, m) -> Float.compare n m) in
  List.iter sorted ~f:(fun (ds, n) ->
      print_endline @@ Sexp.to_string (IntSet.sexp_of_t ds) ^ ": " ^ string_of_float n)
