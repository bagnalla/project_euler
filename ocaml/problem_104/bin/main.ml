(** Not the cleanest or fastest solution but it does the job. The key
    idea is to use addition mod 1000000000 to look for Fibonacci
    numbers whose last 9 digits are pandigital, and then compute the
    full Fibonacci number for these candidates (using zarith integers
    and memoization) to check if their first 9 digits are pandigital. \
  
    Runs in ~13s in my Linux VM.
 *)

open Core

module Cache = Map.Make(Int)

let cache : Z.t Cache.t ref =
  ref (Map.add_exn (Map.add_exn Cache.empty ~key:0 ~data:Z.zero) ~key: 1 ~data: Z.one)

let rec fib (n : int) : Z.t =
  match Map.find !cache n with
  | Some z -> z
  | None ->
     let m = Z.add (fib @@ n - 1) (fib @@ n - 2) in
     cache := Map.add_exn !cache ~key:n ~data:m;
     m

let () =
  (* Imperative algorithm. *)
  let n = ref 0 in
  let m = ref 1 in
  let finished = ref false in
  let a = ref 2 in
  while not !finished do
    let x = Int.rem (!n + !m) 1000000000 in
    let s = string_of_int x in
    if String.for_all "123456789" ~f:(fun c ->
           String.exists s ~f:(fun y -> Char.(=) y c)) then
      begin
        let x = fib !a in
        let s' = String.prefix (Z.to_string x) 9 in
        if String.for_all "123456789" ~f:(fun c ->
               String.exists s' ~f:(fun y -> Char.(=) y c)) then
          (print_endline @@ string_of_int !a;
           finished := true)
      end;
    n := !m;
    m := x;
    a := !a + 1
  done
