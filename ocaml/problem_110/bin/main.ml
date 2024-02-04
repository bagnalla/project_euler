(** I went about this in a somewhat unprincipled way. The gist of the
    algorithm is the same as my solution for problem 108 (see
    ../cpp/problem_108), although the last part had to be slightly
    refined since I didn't fully understand it before and happened to
    get lucky: When replacing the largest prime factor, you search for
    the smallest factor (not necessarily prime) to replace it with
    that makes the number of solutions still exceed the target
    number. And you must repeat this process to replace as many of the
    largest prime factors as possible until there is no suitable
    smaller factor.

    For problem 108, this replacement factor happened to be [2*3=6],
    and after that no more replacements were possible. For this
    problem, we replace the two largest prime factors both with [6],
    and then the third largest with [35]. I performed these
    replacements by hand via a bit of trial and error, out of laziness
    since it seemed easier than writing code to do it. 

    We also optimize the calculation of the number of solutions for a
    given [n] by exploiting the formula given at
    https://oeis.org/A018892 in terms of the prime factorization of
    [n]. This is convenient for us because we're generating [n] from
    its prime factors to begin with. *)

open Core
open Z

let primes =
  List.map [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59] ~f:of_int

(** Compute the number represented by a list of prime powers. *)
let num_of_prime_powers (prime_powers : int list) : Z.t =
  List.fold (List.mapi prime_powers ~f:(fun i n -> pow (List.nth_exn primes i) n))
    ~init:one ~f:( * )

let a018892 (prime_powers : int list) : int =
  Int.(/) (Int.succ @@ List.fold (List.map prime_powers
                                    ~f:(fun n -> Int.succ (Int.( * ) 2 n)))
                         ~init:1 ~f:Int.( * )) 2

let () =
  (* We use lists to represent prime powers. For example:
     [1; 1; 1] denotes 2 * 3 * 5,
     [2; 1; 3] denotes 2² * 3 * 5³, and
     [2; 1; 1; 2] denotes 2² * 3 * 5 * 7² *)

  (* First we find the smallest product of first-order prime factors
     for which the number of solutions exceeds four million. *)
  print_endline @@ string_of_int @@
    a018892 [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1];
  print_endline @@ to_string @@
    num_of_prime_powers [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1];

  (* Replace largest factor with [2*3 = 6]. *)
  print_endline @@ string_of_int @@ a018892       [2; 2; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1];
  print_endline @@ to_string @@ num_of_prime_powers [2; 2; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1];

  (* Replace next largest factor with [2*3 = 6]. *)
  print_endline @@ string_of_int @@ a018892       [3; 3; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1];
  print_endline @@ to_string @@ num_of_prime_powers [3; 3; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1];

  (* Replace next largest factor with [5*7 = 35]. *)
  print_endline @@ string_of_int @@ a018892       [3; 3; 2; 2; 1; 1; 1; 1; 1; 1; 1; 1];
  print_endline @@ to_string @@ num_of_prime_powers [3; 3; 2; 2; 1; 1; 1; 1; 1; 1; 1; 1];

  (* This is as low as we can go. 41 can't be replaced with by any smaller factor. *)
