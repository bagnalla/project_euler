(** Let [b] denote the number of blue discs, and [n] the total number
    of discs. We have from the problem statement:
    
    [b/n * (b-1)/(n-1) = 1/2]

    which can be rearranged into
    
    [2(b² - b) - (n² - n) = 0].

    Completing the squares:

<-> [2(b² - b + 1/4 - 1/4) - (n² - n + 1/4 - 1/4) = 0]
<-> [2((b - 1/2)² - 1/4) - ((n - 1/2)² - 1/4) = 0]
<-> [2(b - 1/2)² - (n - 1/2)² - 1/4 = 0].

    Let [x = 2(b - 1/2)] and [y = 2(n - 1/2)]
    so [x / 2 = b - 1/2] and [y / 2 = n - 1/2].
    Then by substitution the equation becomes:

    [2(x/2)² - (y/2)² - 1/4 = 0]
<-> [2x²/4 - y²/4 - 1/4 = 0]
<-> [2x² - y² - 1 = 0]
<-> [y² - 2x² = -1]

    This is a negative Pell equation in variables [x] and [y] with
    [D=2]. We can use the continued fraction method to find solutions
    for [x] and [y] and derive from them the corresponding [b =
    (x+1)/2] and [n = (y+1)/2]. We do this and take the first solution
    where [n >= 10^12]. 

    Runs in under 100ms in my Linux VM. *)

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
   representation of [n].*)
let sqrt_continued_fraction_digits (n : Z.t) : Z.t stream =
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

let find_solution (d : Z.t) : Z.t * Z.t =
  let rec go n =
    let x_over_y = partial_fraction n (sqrt_continued_fraction_digits d) in
    let x, y = Q.num x_over_y, Q.den x_over_y in
    if x * x - d * y * y == Z.neg Z.one && succ x / ~$2 > pow ~$10 12 then
      succ x / ~$2, succ y / ~$2
    else
      go (Int.succ n)
  in go 0

let () =
  let t, b = find_solution ~$2 in
  print_endline @@ "total = " ^ to_string t ^ ", blue = " ^ to_string b
