(** This just uses code I wrote while trying to brute-force problem
    103. It seems easier than 45% difficulty rating...

    For each input set [A], we just check for every pair of non-empty
    disjoint subsets of [A] that the expected properties are
    satisfied. The only real trick is skipping non-disjoint pairs of
    sets by comparing each candidate set [s] with only subsets of [A \ s].

    Runs in about half a second in my Linux VM.
*)

open Core

module IntSet = Set.Make(Int)

let rec all_subsets (l : IntSet.t) : IntSet.t list =
  if Set.length l = 0 then
    []
  else
    let x = Set.choose_exn l in
    let xs = Set.remove l x in
    let ss = all_subsets xs in
    IntSet.singleton x :: ss @ List.map ss ~f:(fun s' -> Set.add s' x)

let sum (s : IntSet.t) : int =
  Set.fold s ~init:0 ~f:( + )

let is_special_sum_set (s : IntSet.t) : bool =
  let subsets = all_subsets s in
  List.for_all subsets ~f:(fun x ->
      List.for_all (all_subsets @@ Set.diff s x) ~f:(fun y ->
          let sx = sum x in
          let sy = sum y in
          let prop1 = sx <> sy in
          let prop2 = not (Set.length x < Set.length y) || sx < sy in
          prop1 && prop2
        )
    )

(* let string_of_set (show : 'a -> string) (s : ('a, 'comp) Set.t) : string = *)
(*   String.concat (List.map (Set.to_list s) ~f:show) ~sep:" " *)

let set_of_string (s : string) : IntSet.t =
  IntSet.of_list @@ List.map (String.split s ~on:',') ~f:int_of_string

let () =
  let lines = In_channel.read_lines "sets.txt" in
  let sets = List.map lines ~f:set_of_string in

  (* List.iter sets ~f:(fun s -> *)
  (*     if is_special_sum_set s then *)
  (*       print_endline @@ string_of_set string_of_int s *)
  (*   ) *)

  print_endline @@ string_of_int @@
    List.fold (List.filter sets ~f:is_special_sum_set) ~init:0
      ~f:(fun acc s -> acc + sum s)
