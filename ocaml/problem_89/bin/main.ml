(** Bit of a pea-brain solution but it works and is easy to
    understand. Obviously could generalize the functions below but
    there's no need. *)

open Core

let chars (s : string) : char list =
  List.init (String.length s) ~f:(String.get s)

let is (l : char list) : int = List.length l

let rec vs (l : char list) : int =
  match l with
  | [] -> 0
  | 'V' :: l' -> 5 + vs l'
  | 'I' :: l' -> if List.exists ~f:(equal_char 'V') l' then vs l' - 1 else is l
  | _ -> is l

let rec xs (l : char list) : int =
  match l with
  | [] -> 0
  | 'X' :: l' -> 10 + xs l'
  | 'I' :: l' -> if List.exists ~f:(equal_char 'X') l' then xs l' - 1 else vs l
  | _ -> vs l

let rec ls (l : char list) : int =
  match l with
  | [] -> 0
  | 'L' :: l' -> 50 + ls l'
  | 'X' :: l' -> if List.exists ~f:(equal_char 'L') l' then ls l' - 10 else xs l
  | _ -> xs l

let rec cs (l : char list) : int =
  match l with
  | [] -> 0
  | 'C' :: l' -> 100 + cs l'
  | 'X' :: l' -> if List.exists ~f:(equal_char 'C') l' then cs l' - 10 else ls l
  | _ -> ls l

let rec ds (l : char list) : int =
  match l with
  | [] -> 0
  | 'D' :: l' -> 500 + ds l'
  | 'C' :: l' -> if List.exists ~f:(equal_char 'D') l' then ds l' - 100 else cs l
  | _ -> cs l

let rec ms (l : char list) : int =
  match l with
  | [] -> 0
  | 'M' :: l' -> 1000 + ms l'
  | 'C' :: l' -> if List.exists ~f:(equal_char 'M') l' then ms l' - 100 else ds l
  | _ -> ds l

let to_numeral (n : int) : string =
  let m = n / 1000 in
  let rem = ref (Int.rem n 1000) in
  let m_str = if !rem >= 900 then
                (rem := !rem - 900; "C" ^ String.make (m+1) 'M')
              else
                String.make m 'M' in
  
  let d = !rem / 500 in
  rem := Int.rem !rem 500;
  let d_str = if !rem >= 400 then
                (rem := !rem - 400; "C" ^ String.make (d+1) 'D')
              else
                String.make d 'D' in
  
  let c = !rem / 100 in
  rem := Int.rem !rem 100;
  let c_str = if !rem >= 90 then
                (rem := !rem - 90; "X" ^ String.make (c+1) 'C')
              else
                String.make c 'C' in
  
  let l = !rem / 50 in
  rem := Int.rem !rem 50;
  let l_str = if !rem >= 40 then
                (rem := !rem - 40; "X" ^ String.make (l+1) 'L')
              else
                String.make l 'L' in
  
  let x = !rem / 10 in
  rem := Int.rem !rem 10;
  let x_str = if !rem >= 9 then
                (rem := !rem - 9; "I" ^ String.make (x+1) 'X')
              else
                String.make x 'X' in
  
  let v = !rem / 5 in
  rem := Int.rem !rem 5;
  let v_str = if !rem >= 4 then
                (rem := !rem - 4; "I" ^ String.make (v+1) 'V')
              else
                String.make v 'V' in

  let i_str = String.make !rem 'I' in
  
  m_str ^ d_str ^ c_str ^ l_str ^ x_str ^ v_str ^ i_str

let () =
  let original_numerals = In_channel.read_lines "roman.txt" in
  let numeral_values = List.map ~f:(Fn.compose ms chars) original_numerals in
  let minimal_numerals = List.map ~f:to_numeral numeral_values in
  let diffs = List.map ~f:(fun (x, y) -> String.length x - String.length y) @@
                List.zip_exn original_numerals minimal_numerals in
  print_endline @@ string_of_int @@ List.fold diffs ~init:0 ~f:(+)
