(** Brute force using Heron's formula and the zarith library. Not the
    fastest solution but fast enough (~30s in my Linux VM). *)

open Core

(** Heron's formula using floats. Useful for debugging, but we have to
    use big integers for the actual solution below. *)
(* let area (a : float) (b : float) (c : float) : float = *)
(*   let s = (a +. b +. c) /. 2.0 in *)
(*   sqrt @@ s *. (s -. a) *. (s -. b) *. (s -. c) *)

open Z

let () =
  let sum = ref zero in

  (* Try every [n, n, n-1] and [n, n, n+1] for [n ∈ [2, 333333333]]
     (so the perimeter doesn't exceed one billion). *)
  for n = 2 to Int.( / ) 1000000000 3 do
    let l = of_int n in

    (* First, we check if the semiperimeter [s] is an integer by
       checking that [3n +- 1] is even (which is true when [3n] is odd). *)
    if is_odd @@ ~$3 * l then

      (* Semiperimeter [s]. *)
      let s = pred (~$3 * l) / ~$2 in

      (* Let [a=b=l] and [c=l-1] and check if the radicand in Heron's
         formula is a square (and thus the area given by the square
         root is an integer). *)
      if perfect_square @@ s * (s - l) * (s - l) * succ (s - l) then
        sum := !sum + ~$2 * s;
      let s = succ (~$3 * l) / ~$2 in

      (* Let [a=b=l] and [c=l+1] and do the same. *)
      if perfect_square @@ s * (s - l) * (s - l) * pred (s - l) then
        sum := !sum + ~$2 * s;
  done;

  print_endline @@ to_string !sum
