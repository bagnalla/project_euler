(** Straightforward brute force. We first observed for the smaller
    example in the problem statement that there were exactly five ways
    for the player to win (with corresponding probabilities): 

    bbbr: [1/2 * 1/3 * 1/4 * 4/5 = 4/120]
    bbrb: [1/2 * 1/3 * 3/4 * 1/5 = 3/120]
    brbb: [1/2 * 2/3 * 1/4 * 1/5 = 2/120]
    rbbb: [1/2 * 1/3 * 1/4 * 1/5 = 1/120]
    bbbb: [1/2 * 1/3 * 1/4 * 1/5 = 1/120]

    with total probability [11/120]. There are [4 choose 3 = 4]
    combinations with 3 blues and 1 red, and [4 choose 4 = 1]
    combinations with 4 blues. The same approach can be applied to the
    larger problem, with [15 choose 8] ways to win with 8 blues, [15
    choose 9] ways to win with 9 blues, etc. We simply enumerate all
    of the combinations and add up their probabilities to obtain the
    total probability [p] of the player winning, and then look at
    [1/p] to get the maximum prize the banker should allot.

    Runs in about 75ms on my machine.
*)

open Core

module IntSet = Set.Make(Int)

(** List of integers from [low] (inclusive) to [high] (exclusive). *)
let range (low : int) (high : int) : int list =
  if high <= low then [] else
    List.init (high - low) ~f:(fun n -> n + low)

(** Array of integers from [low] (inclusive) to [high] (exclusive). *)
let range_array (low : int) (high : int) : int array =
  if high <= low then Array.create ~len:0 0 else
    Array.init (high - low) ~f:(fun n -> n + low)

(** Compute list of all [k]-combinations from set [s]. *)
let rec choose (k : int) (s : ('a, 'comp) Set.t) : (('a, 'comp) Set.t) list =
  if k <= 0 then [Set.empty (Set.comparator_s s)] else
    if Set.is_empty s then [] else
      Set.fold s ~init:[] ~f:(fun acc x ->
          acc @ List.map ~f:(fun s -> Set.add s x)
                  ((choose (k-1) (Set.filter s ~f:(fun y -> y > x)))))

(** Create a bool array of size [n] containing [true] at every index
    [i ∈ ixs] and [false] everywhere else. *)
let mk_array (n : int) (ixs : IntSet.t) : bool array =
  let a = Array.create ~len:n false in
  Set.iter ixs ~f:(fun i -> a.(i) <- true);
  a

let string_of_array (a : bool array) : string =
  Sexp.to_string @@
    Array.sexp_of_t (fun b -> if b then Sexp.Atom "b" else Sexp.Atom "r") a

(** Compute the probability of the disc sequence encoded by [a]
    ([true] stands for blue disc and [false] for red disc). *)
let prob (a : bool array) : float =
  Array.fold (Array.zip_exn (Array.init (Array.length a)
                               ~f:(fun x -> float_of_int (x + 2)))
                a)
    ~init:1.0
    ~f:(fun acc (d, b) -> acc *. (if b then 1.0 else d -. 1.0) /. d)

let () =
  
  (* Build all possible winning disc sequences. *)
  let ixs = IntSet.of_list @@ List.init 15 ~f:Fn.id in
  let disc_sequences = List.concat_map (range 8 16)
                         ~f:(fun n -> List.map (choose n ixs) ~f:(mk_array 15)) in

  (* Compute their probabilities. *)
  let probs = List.map disc_sequences ~f:prob in

  (* Compute the total probability.*)
  let total_prob = List.sum (module Float) probs ~f:Fn.id in

  (* The answer will be the largest integer that is less than the
     reciprocal of the total probability. *)
  print_endline @@ string_of_float @@ 1.0 /. total_prob
