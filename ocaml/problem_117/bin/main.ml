(**  Memoized mutual recursion. Modification of the previous solution. *)

open Core

(** List of integers from [low] (inclusive) to [high] (exclusive). *)
let range (low : int) (high : int) : int list =
  if high <= low then [] else
    List.init (high - low) ~f:(fun n -> n + low)

(** Maps with integer pair keys. *)
module Cache = Map.Make(struct type t = int * int [@@deriving sexp, compare] end)

let cache : int Cache.t ref = ref Cache.empty

(** [g k n] is the number of ways a row of length [n] can be filled
    with one tile of length [k] and other tiles of any of the allowed
    lengths, PLUS ONE. *)
let rec g (k : int) (n : int) : int =
  match Map.find !cache (k, n) with
  | Some m -> m
  | None ->
     let x = 1 + List.fold (range k (n+1)) ~init:0 ~f:(fun acc i -> acc + f (n - i)) in
     cache := Map.add_exn !cache ~key:(k, n) ~data:x;
     x

(** [f n] is the number of ways a row of length [n] can be filled with
    tiles of lengths 2, 3, and 4 (mixing allowed), including the case
    of zero tiles. We subtract [3 - 1 = 2] from their sum because each
    call to [g] produces the corresponding number of tilings PLUS ONE
    (hence subtracting [3]) but then we add one for the zero tiles case. *)
and f (n : int) : int =
  g 2 n + g 3 n + g 4 n - 2

let () =
  print_endline @@ string_of_int @@ f 50
