(** Solution to Project Euler problem 65 using streams and the zarith
    rational numbers library. *)

type 'a stream =
  | SCons of 'a * (unit -> 'a stream)

let first : 'a stream -> 'a = function
  | SCons (x, _) -> x

let rest : 'a stream -> 'a stream = function
  | SCons (_, s) -> s ()

let scons (x : 'a) (k : unit -> 'a stream) : 'a stream =
  SCons (x, k)

let e_digits =
  let rec go n =
    scons 1 (fun _ -> scons n (fun _ -> scons 1 (fun _ -> go (n+2))))
  in
  scons 2 (fun _ -> go 2)

(* Compute nth partial fraction from given stream of digits. *)
let rec partial_fraction (n : int) (digits : int stream) : Q.t =
  if n <= 1 then
    Q.of_int (first digits)
  else
    Q.add (Q.of_int @@ first digits)
      (Q.div Q.one @@ partial_fraction (n-1) (rest digits))

(* Convert string to list of characters. *)
let chars (s : string) : char list =
  List.init (String.length s) (fun i -> String.get s i)

let sum_of_digits (s : string) : int =
  List.fold_left (+) 0 (List.map (fun c -> Char.code c - 48) @@ chars s)

let () =
  (* List.iter (fun n -> print_endline @@ string_of_int n) @@ take 10 e_digits; *)
  
  (* List.iter (fun n -> print_endline @@ Q.to_string @@ partial_fraction n e_digits) @@ *)
  (*   List.init 10 (fun x -> x); *)

  (* print_endline @@ string_of_int @@ sum_of_digits "1235"; *)

  print_endline @@ string_of_int @@ sum_of_digits @@ Z.to_string @@ Q.num @@
    partial_fraction 100 e_digits
