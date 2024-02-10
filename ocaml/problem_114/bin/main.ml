(** Simple memoized recursive solution (a pair a mutually recursive
    functions).

    [g n k] is the number of number of blocks of size k that can be
    placed in a row of size n.

    [f n] is the total number of blocks that can be placed in a row of
    size n.

    We define [g n k = 1 + \sum_{k+1}^{n}{f (n-i)}] and
              [f n = 1 + \sum_{3}^{n}{g n k}].

    [f] is memoized. Runs in about 145 ms in my Linux VM.
*)

open Core

let range (low : int) (high : int) : int list =
  if high <= low then [] else
    List.init (high - low) ~f:(fun n -> n + low)

module Cache = Map.Make(Int)

let cache : int Cache.t ref = ref Cache.empty

let rec g (n : int) (k : int) : int =
  1 + 
    List.fold (range (k+1) (n+1)) ~init:0 ~f:(fun acc i -> acc + f (n - i))

and f (n : int) : int =
  match Map.find !cache n with
  | Some m -> m
  | None ->
     let m = 1 + List.fold (range 3 (n+1)) ~init:0 ~f:(fun acc k -> acc + g n k) in
     cache := Map.add_exn !cache ~key:n ~data:m;
     m

let () =
  print_endline @@ string_of_int @@ f 50
