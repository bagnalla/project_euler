
let rec split (s : string) (step : int) : string list =
  if String.length s <= step then [s] else
    String.sub s 0 step ::
      split (String.sub s step (String.length s - step)) step

open Z

(* Determine greatest digit x such that [x(20p + x) ≤ c]. *)
let rec adjust (x : Z.t) (p : Z.t) (c : Z.t) : Z.t =
  if x = ~$9 || (Z.succ x) * (~$20*p + (Z.succ x)) > c then
    x
  else
    adjust (Z.succ x) p c

let choose_x (p : Z.t) (c : Z.t) : Z.t =
  let x = (~$(-20)*p + Z.sqrt (~$400*p*p + ~$4*c)) / ~$2 in
  adjust x p c

let sqrt_digits (n : Z.t) : string =
  let s = Z.to_string n in
  let s' = if Int.rem (String.length s) 2 = 1 then "0" ^ s else s in
  let n_pairs = split s' 2 in
  let rem = ref Z.zero in
  let digits = ref "" in
  let i = ref 0 in
  let p = ref Z.zero in
  while (!rem <> Z.zero || !i < List.length n_pairs) && String.length !digits < 100 do
    let c = !rem * ~$100 + (if !i < List.length n_pairs
                            then Z.of_string (List.nth n_pairs !i)
                            else Z.zero) in
    (* let p = if String.length !digits > 0 then Z.of_string !digits else Z.zero in *)
    let x = choose_x !p c in
    (* print_endline @@ Z.to_string x; *)
    let y = x * (~$20*(!p) + x) in
    (* print_endline @@ Z.to_string y; *)
    digits := !digits ^ Z.to_string x;
    p := !p * ~$10 + x;
    rem := c - y;
    i := Int.succ !i
  done;
  (* String.concat "," n_pairs *)
  !digits

(*   for (size_t i = 0; rem != 0 || i < n_pairs.size(); i++) { *)
(*     uint c = rem * 100; *)
(*     if (i < n_pairs.size()) { *)
(*       c += stoi(n_pairs[i]); *)
(*     } *)

(*     uint p =  *)
(*   } *)

(*   return {}; *)
(* } *)


let () =
  (* List.iter print_endline @@ split_into_pairs "123456" 2 *)
  
  print_endline @@ sqrt_digits ~$2

  (* print_endline @@ Z.to_string @@ choose_x ~$12 ~$827 *)
  
  (* let digits = sqrt_digits ~$2 in *)
  (* let sum = String.fold_left (fun acc c -> acc + ~$(Int.sub (Char.code c) 48)) ~$0 digits in *)
  (* print_endline @@ Z.to_string sum *)

  (* let sum *)
  (* for i = 2 to 100 do *)
  (*   let digits = sqrt_digits ~$i in *)
    
  (*   let sum = String.fold_left (fun acc c -> acc + ~$(Int.sub (Char.code c) 48)) ~$0 digits in *)
    
  (* done *)

  (* print_endline @@ Z.to_string @@ *)
  (*   List.fold_left Z.add ~$0 *)
  (*     (List.init 99 (fun n -> *)
  (*          let digits = sqrt_digits (~$(Int.add n 2)) in *)
  (*          if String.length digits = 100 then *)
  (*            String.fold_left (fun acc c -> acc + ~$(Int.sub (Char.code c) 48)) ~$0 digits *)
  (*          else *)
  (*            ~$0)) *)
