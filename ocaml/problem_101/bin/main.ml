(** We first observe that optimum polynomial for the first [k] terms
    of the sequence is in general a polynomial of order [k-1] (e.g.,
    for two data points we need a linear polynomial, and for three
    data points a quadratic polynomial). Then we observe that the
    coefficients for [OP(k)] can be found as the solution of the
    system of linear equations obtained by setting [OP(k, n) = uₙ] for
    each [1 ≤ n ≤ k]. E.g., for the problem example to find [OP(1, n)]
    we must find coefficients [a] and [b] such that:
    
    [a * 1 + b = 1]
    [a * 2 + b = 8]

    This gives us two linear equations with two unknowns, so we can
    use a linear algebra library (lacaml) to solve for [a] and [b] in:

    [1 1]   [a]   [1]
    [2 1] x [b] = [8]

    which gives the unique solution [a = 7] and [b = -6], so we have
    [OP(1, n) = 7n - 6].

    Similarly, for [OP(2)] we have the three equations:
    
    [a * 1² + b * 1 + c = 1]
    [a * 2² + b * 2 + c = 8]
    [a * 3² + b * 3 + c = 27]

    so we solve for [a], [b], and [c] in:

    [1 1 1]   [a]   [1]
    [4 2 1]   [b]   [8]
    [9 3 1] x [c] = [27]

    yielding [a = 6], [b = -11], and [c = 6], so we have
    [OP(2, n) = 6n² - 11n + 6].

    We generalize this process and repeat it for all [1 ≤ k < n]. 
    
    Reminder: [M * x = y <-> x = M^-1 * y], so to solve for x it
    suffices to invert [M] and multiply with [y].
*)

open Core
open Lacaml.D

let sum (l : int list) : int =
  List.fold l ~init:0 ~f:( + )

(** The target function uₙ. *)
let generating_function (n : int) : int =
  sum @@ List.init 11 ~f:(fun i -> Int.pow (-1) i * Int.pow n i)

(** Build list of rows of LHS matrix for solving for the nth optimum polynomial. *)
let powers (n : int) : float list list =
  List.init n ~f:(fun i ->
      List.init n ~f:(fun j ->
          float_of_int @@ Int.pow (i+1) j))

let powers_matrix (n : int) : Mat.t =
  Mat.of_list @@ powers n

(** Build polynomial function from list of coefficients. *)
let poly (coeffs : int list) (n : int) : int =
  sum @@ List.mapi coeffs ~f:(fun i x -> x * Int.pow n i)

(** Find FIT for BOP [f] wrt. generating function [g]. Only terminates
    when [f] is a BOP (and thus a FIT exists). *)
let first_incorrect_term (f : int -> int) (g : int -> int) : int =
  let rec go n = if f n <> g n then f n else go (n + 1) in
  go 1

let () =
  let sum = ref 0 in
  
  for n = 1 to 10 do
    let m = powers_matrix n in
    let y = Mat.transpose_copy @@ Mat.of_list @@
              [List.init n ~f:(fun i -> float_of_int @@ generating_function (i+1))] in
    
    (* Solve the system of equations [mx = y]. *)
    gesv m y;
    
    let x = List.map (Vec.to_list @@ Mat.col y 1)
              ~f:(Fn.compose int_of_float round) in

    (* Build polynomial with solution coefficients. *)
    let f = poly x in
    
    sum := !sum + first_incorrect_term f generating_function
  done;

  print_endline @@ string_of_int !sum
