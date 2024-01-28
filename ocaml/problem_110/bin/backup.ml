open Core
open Z

(* let tau (n : Z.t) : int = *)
(*   List.count (List.init (to_int n) ~f:of_int) *)
(*     ~f:(fun k -> divisible n (succ k)) *)

let tau (n : Z.t) : int =
  (* let root, remainder = sqrt_rem n in *)
  (* Int.( - ) (Int.( * ) 2 @@ List.count (List.init (to_int root) ~f:of_int) *)
  (*                             ~f:(fun k -> divisible n (succ k))) *)
  (*   (if Z.equal remainder zero then 1 else 0) *)
  let count = ref 0 in
  let i = ref one in
  let i_squared = ref one in
  let step = ref ~$3 in
  while lt !i_squared n do
    if divisible n !i then
      count := Int.succ !count;
    (* print_endline @@ to_string !i ^ " " ^ to_string !i_squared; *)
    i := succ !i;
    i_squared := !i_squared + !step;
    step := !step + ~$2
  done;
  !count


(* uint count_solutions(ulong n) { *)
(*   uint count = 0; *)
(*   for (ulong y = n + 1; y <= n*n + n; y++) { *)
(*     if ((y * n) % (y - n) == 0) { *)
(*       ulong x = y * n / (y - n); *)
(*       count++; *)
(*       if (x == y) { *)
(*         break; *)
(*       } *)
(*     } *)
(*   } *)
(*   return count; *)
(* } *)

let count_solutions (n : Z.t) : int =
  let count = ref 0 in
  let y = ref (succ n) in
  let go = ref true in
  while leq !y ((pow n 2) + n) && !go do
    let x, remainder = div_rem (!y * n) (!y - n) in
    if equal remainder zero then
      begin
        count := Int.succ !count;
        if equal x !y then
          go := false
      end;
    y := succ !y
  done;
  !count

let primes =
  List.map [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59] ~f:of_int

let num_of_prime_powers (prime_powers : int list) : Z.t =
  List.fold (List.mapi prime_powers ~f:(fun i n -> pow (List.nth_exn primes i) n))
    ~init:one ~f:( * )

let a018892_1 (prime_powers : int list) : int =
  count_solutions @@ num_of_prime_powers prime_powers

let a018892_2 (prime_powers : int list) : int =
  Int.(/) (Int.succ @@ List.fold (List.map prime_powers
                                    ~f:(fun n -> Int.succ (Int.( * ) 2 n)))
                         ~init:1 ~f:Int.( * )) 2

let () =
  (* for n = 1 to 10 do *)
  (*   print_endline @@ "tau(" ^ string_of_int n ^ ") = " ^ string_of_int @@ tau ~$n *)
  (* done; *)
  
  (* print_endline @@ "tau(18446744073709551615) = " ^ string_of_int @@ tau ~$1844674407370955161 *)
  (* print_endline @@ "tau(1000) = " ^ string_of_int @@ tau ~$1000 *)

  (* for n = 1 to 10 do *)
  (*   print_endline @@ string_of_int n ^ " " ^ string_of_int @@ count_solutions @@ of_int n *)
  (* done; *)

  (* let n = ~$2 * ~$3 * ~$5 * ~$7 * ~$11 * ~$17 * ~$23 * ~$29 * ~$31 in *)
  (* print_endline @@ to_string n ^ *)
  (*                    (\* " " ^ string_of_int (tau n) ^ *\) *)
  (*                    " " ^ string_of_int (count_solutions n) *)

  (* print_endline @@ to_string @@ ~$2 * ~$2 * ~$3 * ~$5 * ~$5; *)
  (* print_endline @@ to_string @@ num_of_prime_powers [2; 1; 2]; *)
  (* (\* print_endline @@ string_of_int @@ a018892_1 [1; 1; 1; 1; 1; 1; 1]; *\) *)
  
  print_endline @@ string_of_int @@ a018892_2       [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1];
  print_endline @@ to_string @@ num_of_prime_powers [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1];

  (* Replace largest factor with [2*3 = 6]. *)
  print_endline @@ string_of_int @@ a018892_2       [2; 2; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1];
  print_endline @@ to_string @@ num_of_prime_powers [2; 2; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1];

  (* Replace next largest factor with [2*3 = 6] again. *)
  print_endline @@ string_of_int @@ a018892_2       [3; 3; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1];
  print_endline @@ to_string @@ num_of_prime_powers [3; 3; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1];

  (* Replace next largest factor with [5*7 = 35]. *)
                                                  (* 2  3  5  7  11 13 17 23 29 31 37 41 *)
  print_endline @@ string_of_int @@ a018892_2       [3; 3; 2; 2; 1; 1; 1; 1; 1; 1; 1; 1];
  print_endline @@ to_string @@ num_of_prime_powers [3; 3; 2; 2; 1; 1; 1; 1; 1; 1; 1; 1];

  (* This is as low as we can go. 41 can't be replaced with by any smaller factor. *)
