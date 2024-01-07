(** Solution to Project Euler problem 78 using streams and the zarith
    library for big integers. We implement an algorithm for the
    partition function 'p' based on the recurrence relation described
    here: https://en.wikipedia.org/wiki/Pentagonal_number_theorem. *)

open Z (* For Z infix operators. *)

type 'a stream =
  | SCons of 'a * (unit -> 'a stream)

let first : 'a stream -> 'a = function
  | SCons (x, _) -> x

let rest : 'a stream -> 'a stream = function
  | SCons (_, s) -> s ()

let scons (x : 'a) (k : unit -> 'a stream) : 'a stream =
  SCons (x, k)

let rec map (f : 'a -> 'b) : 'a stream -> 'b stream = function
  | SCons (x, s) -> SCons (f x, fun _ -> map f (s ()))

let rec take (n : int) (s : 'a stream) : 'a list =
  if n <= 0 then [] else match s with
                         | SCons (x, k) -> x :: take (Int.sub n 1) (k ())

let rec take_while (f : 'a -> bool) : 'a stream -> 'a list = function
  | SCons (x, k) -> if f x
                    then x :: take_while f (k ())
                    else []

(* nth pentagonal number (https://en.wikipedia.org/wiki/Pentagonal_number). *)
let penta (n : Z.t) : Z.t =
  (~$3 * n*n - n) / ~$2

(* 0, 1, -1, 2, -2, 3, -3, ... *)
let alternating_nats : Z.t stream =
  let rec go (n : Z.t) : Z.t stream =
    scons n (fun _ -> if Z.leq n Z.zero
                      then go (Z.succ (Z.neg n))
                      else go (Z.neg n))
  in go Z.zero

(* We maintain a cache to memoize the outputs of p. *)
module Cache = Map.Make(Z)
let cache : Z.t Cache.t ref = ref Cache.empty

let rec p (n : Z.t) : Z.t =
  if Z.equal n Z.zero then
    Z.one
  else if Z.lt n Z.zero then
    Z.zero
  else
    (* Technically since we are evaluating p on strictly increasing
       inputs (i.e., bottom-up) we could forego this check and do
       cache lookups directly instead of recursive calls to p
       (essentially turning this into a dynamic programming
       algorithm), but it doesn't seem to affect performance by any
       noticeable amount so we keep this more general form where the
       inputs can come in any order. *)
    match Cache.find_opt n !cache with
    | Some sum -> sum
    | None ->
    let sum = List.fold_left Z.add Z.zero @@
                List.map (fun k -> (if Z.is_odd k then Z.one else Z.neg Z.one) *
                                     p (n - penta k)) @@
                  take_while (fun k -> penta k <= n) (rest alternating_nats)
    in
    cache := Cache.add n sum !cache;
    sum

let search () : Z.t =
  let rec go (n : Z.t) : Z.t =
    if p n mod ~$1000000 = Z.zero then n else go (Z.succ n)
  in
  go Z.zero

let () =
  print_endline @@ Z.to_string @@ search ()
