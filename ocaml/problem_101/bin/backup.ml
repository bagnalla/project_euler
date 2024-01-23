open Core
open Format
open Lacaml.D
open Lacaml.Io

(* https://github.com/mmottl/lacaml/blob/master/examples/lin_eq.ml *)

(* (\** Build list of columns of coefficient matrix for solving for the *)
(*     nth optimum polynomial. *\) *)
(* let coefficients (n : int) : float list list = *)
(*   List.rev @@ List.init n ~f:(fun j -> *)
(*                   List.init n ~f:(fun i -> *)
(*                       float_of_int @@ Int.pow (i+1) j)) *)

(* let coefficient_matrix (n : int) : Mat.t = *)
(*   Mat.transpose_copy @@ Mat.of_list @@ coefficients 4 *)

(* let f (n : int) : float = *)
(*   float_of_int (1 - n + Int.pow n 2 - Int.pow n 3 + Int.pow n 4 - *)
(*                   Int.pow n 5 + Int.pow n 6 - Int.pow n 7 + *)
(*                   Int.pow n 8 - Int.pow n 9 + Int.pow n 10) *)

let sum (l : int list) : int =
  List.fold l ~init:0 ~f:( + )

let generating_function (n : int) : int =
  sum @@ List.init 11 ~f:(fun i -> Int.pow (-1) i * Int.pow n i)

(* let generating_function (n : int) : int = *)
(*   Int.pow n 3 *)

(** Build list of rows of coefficient matrix for solving for the nth
    optimum polynomial. *)
let coefficients (n : int) : float list list =
  List.init n ~f:(fun i ->
      (* List.rev @@ List.init n ~f:(fun j -> *)
      (*                 float_of_int @@ Int.pow (i+1) j)) *)
      List.init n ~f:(fun j ->
          float_of_int @@ Int.pow (i+1) j))

let coefficient_matrix (n : int) : Mat.t =
  Mat.of_list @@ coefficients n

(** Build polynomial function from list of coefficients. *)
let poly (coeffs : int list) (n : int) : int =
  sum @@ List.mapi coeffs ~f:(fun i x -> x * Int.pow n i)

(** Find FIT for BOP [f] wrt. generating function [g]. Only terminates
    when [f] is a BOP (and thus a FIT exists). *)
let first_incorrect_term (f : int -> int) (g : int -> int) : int =
  let rec go n = if f n <> g n then f n else go (n + 1) in
  go 1

let () =
  (* List.iter (List.init 5 ~f:Fn.id) ~f:(Fn.compose print_endline string_of_int) *)

  (* Array.iter (coefficients 4) ~f:(fun cols -> *)
  (*     Array.iter cols ~f:(fun x -> print_string @@ string_of_float x ^ " "); *)
  (*     print_endline ""); *)

  (* let n = 3 in *)
  (* let m = coefficient_matrix n in *)
  (* let y = Mat.transpose_copy @@ Mat.of_list @@ *)
  (*           [List.init n ~f:(fun i -> float_of_int @@ generating_function (i+1))] in *)
  (* let m_copy = lacpy m in *)
  (* let y_copy = lacpy y in *)
  (* gesv m_copy y_copy;   *)
  (* let sol = List.map (Vec.to_list @@ Mat.col y_copy 1) *)
  (*             ~f:(Fn.compose int_of_float round) in *)
  
  (* printf "Sol: X = @[%a@]@\n" pp_rfvec sol; *)
  (* List.iter sol ~f:(fun x -> print_endline @@ string_of_int @@ int_of_float @@ round x); *)

  (* List.iter (List.init 5 ~f:f) ~f:(fun i -> print_endline @@ string_of_float i); *)
  (* List.iter (List.init 5 ~f:g) ~f:(fun i -> print_endline @@ string_of_float i); *)

  (* let f = poly sol in *)
  (* (\* List.iter (List.init 5 ~f:Fn.id) ~f:(fun i -> print_endline @@ string_of_int @@ f i); *\) *)
  (* print_endline @@ string_of_int @@ first_incorrect_term f generating_function; *)

  let sum = ref 0 in
  
  for n = 1 to 10 do
    let m = coefficient_matrix n in
    let y = Mat.transpose_copy @@ Mat.of_list @@
              [List.init n ~f:(fun i -> float_of_int @@ generating_function (i+1))] in
    gesv m y;
    let sol = List.map (Vec.to_list @@ Mat.col y 1)
                ~f:(Fn.compose int_of_float round) in
    let f = poly sol in
    sum := !sum + first_incorrect_term f generating_function
  done;

  print_endline @@ string_of_int !sum
