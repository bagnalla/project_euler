(** Straightforward brute-force.

    Some experimentation revealed that for a given [a], the values of
    r are always cyclic as [n] increases (alternating between [2] and
    some multiple of [a]), and r_max is at most [a * (a-1)]. So, when
    searching for r_max we only look at odd values of [n] to skip over
    the [2]s and stop when we encounter [a * (a-1)] or a value that
    we've seen before.

    Runs in about 670 ms in my Linux VM.
*)

open Core

(** List of integers from [low] (inclusive) to [high] (exclusive). *)
let range (low : int) (high : int) : int list =
  if high <= low then [] else
    List.init (high - low) ~f:(fun n -> n + low)

open Z

(** The remainder when [(a - 1)ⁿ + (a + 1)ⁿ] is divided by [a²]. *)
let r (a : Z.t) (n : int) : Z.t =
  rem (pow (a - one) n + pow (succ a) n) (pow a 2)

exception Unimplemented
module ZSet = Set.Make(struct
                  type t = Z.t
                  let sexp_of_t z = Sexp.Atom (to_string z)
                  let t_of_sexp _ = raise Unimplemented
                  let compare = Z.compare
                end)

(** Find [r_max] for a given [a]. *)
let largest (a : Z.t) : Z.t =
  let largest_possible = a * (a - one) in
  let rec go (n : int) (s : ZSet.t) : Z.t =
    let x = r a n in
    if equal x largest_possible then
      x
    else if Set.exists s ~f:(fun y -> equal y x) then
      Set.fold s ~init:zero ~f:Z.max
    else
      go (Int.(+) n 2) (Set.add s x)
  in
  go 1 ZSet.empty

(** Compute the sum of [r_max] for all [a ∈ [3, 1000]]. *)
let () =
  print_endline @@ to_string @@
    List.sum (module Z) (List.map (range 3 1001)
                           ~f:(fun a -> largest ~$a))
      ~f:Fn.id
