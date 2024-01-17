(** I almost way overcomplicated this one since I expected it to be
    difficult (40% difficulty rating). It turned out that a
    straightforward brute force solution is more than good enough. *)

open Core

module IntSet = Core.Set.Make(Int)

let rec choose (k : int) (s : ('a, 'comp) Set.t) : (('a, 'comp) Set.t) list =
  if k <= 0 then [IntSet.empty] else
    if Set.is_empty s then [] else
      Set.fold s ~init:[] ~f:(fun acc x ->
          acc @ List.map ~f:(fun s -> Set.add s x)
                  ((choose (k-1) (Set.filter s ~f:(fun y -> y > x)))))

(* let to_string : IntSet.t -> string = Fn.compose Sexp.to_string IntSet.sexp_of_t *)

let cartesian l l' = 
  List.concat (List.map ~f:(fun e -> List.map ~f:(fun e' -> (e,e')) l') l)

let contains_squares (a : IntSet.t) (b : IntSet.t) : bool =
  List.for_all [[0],[1]; [0],[4]; [0],[6;9]; [1],[6;9];
                [2],[5]; [3],[6;9]; [6;9],[4]; [8],[1]]
    ~f:(fun (xs, ys) -> (List.exists xs ~f:(fun x -> Set.mem a x) &&
                           List.exists ys ~f:(fun y -> Set.mem b y)) ||
                          (List.exists xs ~f:(fun x -> Set.mem b x) &&
                             List.exists ys ~f:(fun y -> Set.mem a y)))

let () =
  let l = choose 6 (IntSet.of_list [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]) in

  (* Imperative version that should probably be a bit faster since it
     avoids double counting. The solution below double counts and
     then divides by two.*)
  (* let count = ref 0 in *)
  (* for i = 0 to List.length l - 1 do *)
  (*   for j = i to List.length l - 1 do *)
  (*     if contains_squares (List.nth_exn l i) (List.nth_exn l j) then *)
  (*       count := !count + 1 *)
  (*     else *)
  (*       () *)
  (*   done *)
  (* done; *)
  (* print_endline @@ string_of_int !count *)
  
  let pairs = cartesian l l in
  (* print_endline @@ string_of_int @@ List.length pairs; *)
  let solutions = List.filter pairs ~f:(fun (a, b) -> contains_squares a b) in
  (* List.iter solutions ~f:(fun (a, b) -> print_endline @@ to_string a ^ ", " ^ to_string b ); *)
  print_endline @@ string_of_int @@ List.length solutions / 2;
