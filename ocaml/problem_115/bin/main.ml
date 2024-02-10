(** Luckily we implemented the previous problem in a sufficiently
    general way that the extension to this one was trivial.
*)

open Core

(** List of integers from [low] (inclusive) to [high] (exclusive). *)
let range (low : int) (high : int) : int list =
  if high <= low then [] else
    List.init (high - low) ~f:(fun n -> n + low)

(** Maps with integer pair keys. *)
module Cache = Map.Make(struct type t = int * int [@@deriving sexp, compare] end)

let cache : int Cache.t ref = ref Cache.empty

let rec g (m : int) (n : int) (k : int) : int =
  1 + List.fold (range (k+1) (n+1)) ~init:0 ~f:(fun acc i -> acc + f m (n - i))

and f (m : int) (n : int) : int =
  match Map.find !cache (m, n) with
  | Some m -> m
  | None ->
     let x = 1 + List.fold (range m (n+1)) ~init:0 ~f:(fun acc k -> acc + g m n k) in
     cache := Map.add_exn !cache ~key:(m, n) ~data:x;
     x

let () =
  print_endline @@ string_of_int @@ f 50 168
