(** Solution for Project Euler problem 80 based on the algorithm for
    digit-by-digit calculation of square roots described here:
    https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Decimal_(base_10). *)

let num_digits : int = 100

let rec split (s : string) (step : int) : string list =
  if String.length s <= step then [s] else
    String.sub s 0 step ::
      split (String.sub s step (String.length s - step)) step

open Z

(* (\* Adjust approximate choice of x upward if needed. *\) *)
(* let rec adjust (x : Z.t) (p : Z.t) (c : Z.t) : Z.t = *)
(*   if x = ~$9 || (Z.succ x) * (~$20*p + (Z.succ x)) > c then *)
(*     x *)
(*   else *)
(*     adjust (Z.succ x) p c *)

(* Determine greatest digit x such that [x(20p + x) ≤ c]. First choose
   an underapproximation derived from the quadratic formula and then
   adjust upward if needed. Experimentally it seems that the
   adjustment is never necessary... but I haven't proven that to be
   the case. *)
let choose_x (p : Z.t) (c : Z.t) : Z.t =
  let x = (~$(-20)*p + Z.sqrt (~$400*p*p + ~$4*c)) / ~$2 in
  (* adjust x p c *)
  x

(* Imperative style algorithm based on
   https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Decimal_(base_10). *)
let sqrt_digits (n : Z.t) : string =
  let s = Z.to_string n in
  let s' = if Int.rem (String.length s) 2 = 1 then "0" ^ s else s in
  let n_pairs = split s' 2 in
  let rem = ref Z.zero in
  let digits = ref "" in
  let i = ref 0 in
  let p = ref Z.zero in
  while (!rem <> Z.zero || !i < List.length n_pairs) && String.length !digits < num_digits do
    let c = !rem * ~$100 + (if !i < List.length n_pairs
                            then Z.of_string (List.nth n_pairs !i)
                            else Z.zero) in
    let x = choose_x !p c in
    let y = x * (~$20*(!p) + x) in
    digits := !digits ^ Z.to_string x;
    p := !p * ~$10 + x;
    rem := c - y;
    i := Int.succ !i
  done;
  !digits

let () =
  print_endline @@ Z.to_string @@
    List.fold_left Z.add ~$0
      (List.init 99 (fun n ->
           let digits = sqrt_digits (~$(Int.add n 2)) in
           if String.length digits = num_digits then
             String.fold_left (fun acc c -> acc + ~$(Int.sub (Char.code c) 48)) ~$0 digits
           else
             ~$0))
