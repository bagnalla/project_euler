(** Sort of a hacky solution, but looking at the problem thread that
    seems to pretty much be the case for everyone else.

    The basic idea is to generate squares, cubes, 4th powers, etc. (up
    to exponents of 8 but I originally found the solution by going up
    to 30 and then reduced it down to 8 afterward, though it makes
    little difference in performance) and find those that satisfy the
    desired property. Then we sort them and print out the 30th one.

    One optimization I did was based on the observation that the
    largest square satisfying the property had 2 digits, the largest
    cube had 5, the largest 4th power had 7, the largest 5th power had
    9, etc. It seemed that the delta in the number of digits was at
    most 3 from one exponent to the next, so I used that to place an
    upper bound on the base values for each exponent. E.g., for each
    exponent [p], we start with a base of [n=2] and check [n^p] for
    each increasing value of [n] until [n^p] has more than [2 + (p-2)
    * 3] digits. I don't think this approach extrapolates to infinity
    (the delta in the number of digits from one exponent to the next
    eventually exceeds 3) but it works for this problem.

    Runs in about 140 ms in my Linux VM. *)

open Core

let f (n : Z.t) (p : int) : bool =
  let sum = String.fold (Z.to_string n) ~init:Z.zero
              ~f:(fun acc c -> Z.add acc @@ Z.of_int (Char.get_digit_exn c)) in
  Z.equal (Z.pow sum p) n

let max_p = 8

let find_all () : Z.t list =
  let rec go (p : int) : Z.t list =
    if p > max_p then [] else
      let n = ref @@ Z.of_int 2 in
      let l = ref [] in
      let num_digits = 2 + (p - 2) * 3 in
      let upper_bound = Z.pow (Z.of_int 10) num_digits in
      let power = ref (Z.pow !n p) in
      while Z.lt !power upper_bound do
        if f !power p then
          l := !power :: !l;
        n := Z.succ !n;
        power := (Z.pow !n p)
      done;
      !l @ go (succ p)
  in
  go 2

let () =
  let a = List.sort (find_all ()) ~compare:Z.compare in
  print_endline @@ Z.to_string @@ List.nth_exn a 29
