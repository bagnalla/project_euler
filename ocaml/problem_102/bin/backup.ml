open Core
open Float
open Lacaml.D

(* open Format *)
(* open Lacaml.Io *)

type point = { x : float; y : float } [@@deriving sexp]

type tri = { a : point; b : point; c : point } [@@deriving sexp]

exception ParseError

let tri_of_string (s : string) : tri =
  let ss = List.map (String.split s ~on:',') ~f:float_of_string in
  if Int.( = ) (List.length ss) 6 then
    { a = { x = List.nth_exn ss 0; y = List.nth_exn ss 1 }
    ; b = { x = List.nth_exn ss 2; y = List.nth_exn ss 3 }
    ; c = { x = List.nth_exn ss 4; y = List.nth_exn ss 5 } }
  else
    raise ParseError

(** Compute the barycentric coordinates of points [p] wrt. triangle [t]. *)
let bary (p : point) (t : tri) : float * float =
  let m = Mat.of_list [[t.a.x - t.c.x; t.b.x - t.c.x];
                       [t.a.y - t.c.y; t.b.y - t.c.y]] in
  let y = Mat.of_list [[p.x - t.c.x];
                       [p.y - t.c.y]] in
  (* printf "M = @[%a@]@\n" pp_fmat m; *)
  (* printf "y = @[%a@]@\n" pp_fmat y; *)
  gesv m y;
  let st = Vec.to_list @@ Mat.col y 1 in
  List.nth_exn st 0, List.nth_exn st 1

let in_tri (p : point) (t : tri) : bool =
  let s, t = bary p t in
  0.0 <= s && s <= 1.0 &&
    0.0 <= t && t <= 1.0 &&
      0.0 <= 1.0 - s - t && 1.0 - s - t <= 1.0

let () =
  let lines = In_channel.read_lines "triangles.txt" in
  let tris = List.map lines ~f:tri_of_string in

  (* let t1 = { a = { x = -340.0; y =  495.0 } *)
  (*          ; b = { x = -153.0; y = -910.0 } *)
  (*          ; c = { x =  835.0; y = -947.0 } } in *)
  (* print_endline @@ string_of_bool @@ in_tri { x = 0.0; y = 0.0 } t1; *)

  (* let t2 = { a = { x = -175.0; y =  41.0 } *)
  (*          ; b = { x = -421.0; y = -714.0 } *)
  (*          ; c = { x =  574.0; y = -645.0 } } in *)
  (* print_endline @@ string_of_bool @@ in_tri { x = 0.0; y = 0.0 } t2; *)

  print_endline @@ string_of_int @@
    List.count tris ~f:(in_tri { x = 0.0; y = 0.0})
