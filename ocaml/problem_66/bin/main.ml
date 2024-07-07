(** Solution to Project Euler problem 66 using streams and the zarith
    library. Diophantine equations of the form [x² - Dy² = 1] are
    known as Pell's equations. Lagrange proved that when [D] is not a
    perfect square, such equations have infinitely many integer
    solutions in [x] and [y], and that they are given by the
    numerators and denominators of the continued fraction
    approximations (i.e., convergents) of the square root of [D].

    The algorithm for finding a minimal solution is simple: generate
    the continued fraction convergents [xₙ / yₙ ≈ sqrt(D)] in
    increasing order and look for the first pair [xₙ, yₙ] that
    satisfies [xₙ² - Dyₙ² = 1]. *)

open Z

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

let is_square (n : Z.t) : bool =
  snd (Z.sqrt_rem n) == Z.zero

let rec zeros () : Z.t stream = scons Z.zero zeros

(* Generate the sequence of integers in the continued fraction
   representation of [sqrt(n)].*)
let sqrt_continued_fraction (n : Z.t) : Z.t stream =
  if is_square n then scons (Z.sqrt n) zeros else
    let a0 = Z.sqrt n in
    let rec go _ r s : (Z.t * Z.t * Z.t) stream =
      let a' = (r + a0) / s in
      let r' = a' * s - r in
      let s' = (n - r' * r') / s in
      scons (a', r', s') (fun _ -> go a' r' s')
    in
    map (fun (x, _, _) -> x) (go a0 Z.zero Z.one)

(* Compute nth partial fraction from given stream of digits. *)
let rec partial_fraction (n : int) (digits : Z.t stream) : Q.t =
  if n <= 1 then
    Q.make (first digits) Z.one
  else
    Q.add (Q.make (first digits) Z.one)
      (Q.div Q.one @@ partial_fraction (Int.pred n) (rest digits))

(* Find minimal solution for x and y of equation [x^2 - d * y^2 = 1]. *)
let find_solution (d : Z.t) : Z.t * Z.t =
  let rec go n =
    let x_over_y = partial_fraction n (sqrt_continued_fraction d) in
    let x, y = Q.num x_over_y, Q.den x_over_y in
    if x * x - d * y * y == Z.one then
      x, y
    else
      go (Int.succ n)
  in go 0

let () =
  let max_d, max_x, max_y = ref Z.zero, ref Z.zero, ref Z.zero in
  
  for d = 2 to 1000 do
    let dz = Z.of_int d in
    if not (is_square dz) then
      let x, y = find_solution dz in
      if x >= !max_x then
        (max_x := x;
         max_y := y;
         max_d := dz)
      else ()
    else ()
  done;

  print_endline @@ Z.to_string !max_x ^ "^2 - " ^ Z.to_string !max_d ^
                     " * " ^ Z.to_string !max_y ^ "^2 = 1"
