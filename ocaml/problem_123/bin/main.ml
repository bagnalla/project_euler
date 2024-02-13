(** Straightforward modification of some of the code from problem 120.

    Runs in about 5s in my Linux VM.
*)

open Core

let is_prime (n : int) : bool =
  List.for_all (List.init (int_of_float @@ sqrt @@ float_of_int n -. 1.0) ~f:Fn.id)
    ~f:(fun p -> n % (p + 2) <> 0)

open Z

(** The remainder when [(a - 1)ⁿ + (a + 1)ⁿ] is divided by [a²]. *)
let r (a : Z.t) (n : int) : Z.t =
  rem (pow (a - one) n + pow (succ a) n) (pow a 2)

let () =
  let p = ref 5 in (* Current prime. *)
  let n = ref 2 in (* p is the (n-1)th prime. *)
  let continue = ref true in
  while !continue do
    if is_prime !p then
      begin
        n := Int.succ !n;
        (* Skip when n even because the remainder is always 2. *)
        if !n % 2 <> 0 then
          if Z.gt (r ~$(!p) !n) ~$10000000000 then
            (print_endline @@ string_of_int !n;
             continue := false)
      end;
    p := Int.(+) !p 2
  done
