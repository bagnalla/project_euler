(** To check whether a given point [(a, b)] is within a triangle [t =
    <p₁, p₂, p₃>], it suffices to compute the barycentric coordinates
    [s, t, 1-s-t] of [(a, b)] wrt. [t] (expressing the point as a
    linear mixture of the triangle's points [(a, b) = s * p₁ + t * p₂
    + (1-s-t) * p₃]), and check that they are between [0] and [1]. We
    solve for the barycentric coordinates via the following equations
    derived from the equality above:

    [a = s * p₁.x + t * p₂.x + (1-s-t)*p₃.x]
<-> [(p₁.x - p₃.x) * s + (p₂.x - p₃.x) * t = a - p₃.x]

    [a = s * p₁.y + t * p₂.y + (1-s-t)*p₃.y]
<-> [(p₁.y - p₃.y) * s + (p₂.y - p₃.y) * t = a - p₃.y]

    so we have the following matrix equation:

    [p₁.x-p₃.x  p₂.x-p₃.x]   [s]   [a - p₃.x]
    [p₁.y-p₃.y  p₂.y-p₃.y] × [t] = [b - p₃.y]

    which we can easily solve using lacaml.
*)

open Core
open Float
open Lacaml.D

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
  gesv m y;
  let st = Vec.to_list @@ Mat.col y 1 in
  List.nth_exn st 0, List.nth_exn st 1

(** Point [p] is inside triangle [t] when the barycentric coordinates
    of [p] wrt. [t] are between [0] and [1]. *)
let in_tri (p : point) (t : tri) : bool =
  let s, t = bary p t in
  0.0 <= s && s <= 1.0 &&
    0.0 <= t && t <= 1.0 &&
      0.0 <= 1.0 - s - t && 1.0 - s - t <= 1.0

let () =
  let lines = In_channel.read_lines "triangles.txt" in
  let tris = List.map lines ~f:tri_of_string in
  print_endline @@ string_of_int @@
    List.count tris ~f:(in_tri { x = 0.0; y = 0.0 })
