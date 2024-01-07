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

let rec take (n : int) (s : 'a stream) : 'a list =
  if n <= 0 then [] else match s with
                         | SCons (x, k) -> x :: take (Int.sub n 1) (k ())

let rec take_while (f : 'a -> bool) (s : 'a stream) : 'a list =
  match s with
  | SCons (x, k) -> if f x
                    then x :: take_while f (k ())
                    else []
                    

(* The three functions below implement a fast algorithm based on the
   recurrence relation described here:
   https://en.wikipedia.org/wiki/Pentagonal_number_theorem. *)
let penta (n : Z.t) : Z.t =
  (Z.of_int 3 * n*n - n) / Z.of_int 2

(* let alternating (n : Z.t) : Z.t = *)
(*   Z.pow (Z.neg Z.one) (Z.succ n) *)
let alternating_nats : Z.t stream =
  let rec go (n : Z.t) : Z.t stream =
    scons n (fun _ -> if Z.leq n Z.zero
                      then go (Z.succ (Z.neg n))
                      else go (Z.neg n))
  in go Z.zero

let gpenta : Z.t stream =
  map penta alternating_nats

(* let alternating (zs : Z.t stream) : Z.t stream = *)
(*   let rec go (sign : bool) (s : Z.t stream) : Z.t stream = *)
(*     match s with *)
(*     | SCons (x, k) -> *)
(*        scons (if sign then x else Z.neg x) (fun _ -> go (not sign) (k ())) *)
(*   in go true zs *)

let list_alternating (zs : Z.t list) : Z.t list =
  let rec go (sign : bool) (l : Z.t list) : Z.t list =
    match l with
    | [] -> []
    | x :: xs -> (if sign then x else Z.neg x) :: go (not sign) xs
  in go true zs

module Cache = Map.Make(Z)
let cache : Z.t Cache.t ref = ref Cache.empty
(* let cache : Z.t Cache.t ref = ref (Cache.add Z.zero Z.one Cache.empty) *)

let rec p (n : Z.t) : Z.t =
  (* print_endline (Z.to_string n); *)
  if Z.equal n Z.zero then
    Z.one
  else if Z.lt n Z.zero then
    Z.zero
  else
    match Cache.find_opt n !cache with
    | Some sum -> sum
    | None ->
    let sum = List.fold_left Z.add Z.zero @@
                (* list_alternating (List.map (fun x -> p (n - x)) *)
                (*                     (take_while (fun x -> x <= n) (rest gpenta))) in *)
                List.map (fun k -> (if Z.is_odd k then Z.one else Z.neg Z.one) *
                                     p (n - penta k)) @@
                                     (* Cache.find (n - penta k) !cache) @@ *)
                  take_while (fun k -> penta k <= n) (rest alternating_nats)
    in
    cache := Cache.add n sum !cache;
    sum

let one_million : Z.t = Z.of_int 1000000

let search () : Z.t =
  let rec go (n : Z.t) : Z.t =
    if Z.rem (p n) one_million = Z.zero then n else go (Z.succ n)
  in
  go Z.zero

let () =
  (* print_endline @@ Z.to_string @@ penta @@ Z.of_int 3 *)
  
  (* List.iter (fun x -> print_endline @@ Z.to_string x) @@ take 10 @@ alternating *)
  (* List.iter (fun x -> print_endline @@ Z.to_string x) @@ take 10 @@ gpenta *)
  (* List.iter (fun x -> print_endline @@ Z.to_string x) @@ take 10 @@ (p_sequence @@ Z.of_int 5) *)
  
  (* print_endline @@ Z.to_string @@ p @@ Z.of_int 2 *)
  (* List.iter (fun x -> print_endline @@ Z.to_string x) *)
  (*   (take_while (fun x -> x <= Z.of_int 1) gpenta) *)

  (* let n = Z.of_int 3 in *)
  (* List.iter (fun m -> print_endline @@ Z.to_string m) @@ *)
  (*   (\* List.map (fun m -> Z.of_int (Z.sign m) * p (n - penta m)) @@ *\) *)
  (*   (\* List.map (fun k -> Z.pow Z.one (Z.to_int @@ Z.abs k - Z.one) * p (n - penta k)) @@ *\) *)
  (*   List.map (fun k -> (if Z.is_odd k then Z.one else Z.neg Z.one) * p (n - penta k)) @@ *)
  (*   take_while (fun k -> penta k <= n) (rest alternating_nats) *)

  (* print_endline @@ Z.to_string @@ p @@ Z.of_int 6 *)

  (* List.iter (fun n -> print_endline @@ Z.to_string n) @@ *)
  (*   List.init 10000 (fun n -> p (Z.of_int n)) *)

  print_endline @@ Z.to_string @@ List.hd @@ List.init 10000 (fun n -> p (Z.of_int n))

  (* print_endline @@ Z.to_string @@ search () *)
