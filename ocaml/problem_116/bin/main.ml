(** Straightforward memoized recursion, modified from problems 114 and
    115. Runs in about 200 ms in my Linux VM. *)

open Core

(** List of integers from [low] (inclusive) to [high] (exclusive). *)
let range (low : int) (high : int) : int list =
  if high <= low then [] else
    List.init (high - low) ~f:(fun n -> n + low)

(** Maps with integer pair keys. *)
module Cache = Map.Make(struct type t = int * int [@@deriving sexp, compare] end)

let cache : int Cache.t ref = ref Cache.empty

(** [f k n] is the number of number of ways a row of length [n] can be
    filled with tiles of length [k], PLUS ONE. The "plus one" is
    useful for the induction, but it means that you must subtract one
    from the final result to get the right answer. *)
let rec f (k : int) (n : int) : int =
  match Map.find !cache (k, n) with
  | Some m -> m
  | None ->
     let x = 1 + List.fold (range k (n+1)) ~init:0 ~f:(fun acc i -> acc + f k (n - i)) in
     cache := Map.add_exn !cache ~key:(k, n) ~data:x;
     x

let red n = f 2 n - 1
let green n = f 3 n - 1
let blue n = f 4 n - 1
let total n = red n + green n + blue n

let () =
  print_endline @@ string_of_int @@ total 50
