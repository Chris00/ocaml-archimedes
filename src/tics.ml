(* File: tics.ml

   Copyright (C) 2009-2015

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     Pierre Hauweele <antegallya@gmail.com>
     Noemie Meunier <noemie_6462@hotmail.com>
     Fabian Pijcke <fabian.pijcke@gmail.com>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

type labels =
  | No_label
  | Text of (string * float) array
  | Number of int
  | Expnumber of float
  | Expnumber_named of float * string
  | Custom of (float -> string)

type tic =
  | Major of string option * float
  | Minor of float

type t =
  | Fixed of labels * float list
  | Fixed_norm of labels * float list
  | Equidistants of
      labels * float * float * int (* labels, offset, distance, minors *)
  | Auto of labels

(* ntics, min, max *)
(*  val loose_labels_numeric : float -> float -> tic list

  val loose_labels_textual : string array -> tic list*)

let nicenum x round =
  let rounded = 10. ** (floor (log10 x)) in
  let f = x /. rounded in
  let nf =
    if round then
      if f < 1.5 then 1.
      else if f < 3. then 2.
      else if f < 7. then 5.
      else 10.
    else
      if f <= 1. then 1.
      else if f <= 2. then 2.
      else if f <= 5. then 5.
      else 10.
  in
  nf *. rounded


let label_of_float label x = match label with
  | No_label -> None
  | Text _ -> raise (Failure "Not yet implemented")
  | Number n -> Some (Printf.sprintf "%.*g" n x)
  | Expnumber _ -> raise (Failure "Not yet implemented")
  | Expnumber_named _ -> raise (Failure "Not yet implemented")
  | Custom _ -> raise (Failure "Not yet implemented")

let loose_labels ?(ntics=5) xmin xmax label =
  let range = nicenum (xmax -. xmin) false in
  let d = nicenum (range /. float (pred ntics)) true in
  let graphmin = (floor (xmin /. d)) *. d in
  let graphmax = (ceil (xmax /.d)) *. d in
  let rec aux tics x =
    if x > graphmax +. 0.5 *. d then tics
    else aux (Major (label_of_float label x, x) :: tics) (x +. d)
  in
  aux [] graphmin

let equi_labels offset d_major num_minor xmin xmax labels =
  let first_major = xmin +. (mod_float (offset -. xmin) d_major) in
  let d_minor = d_major /. float (succ num_minor) in
  let rec aux_tics acc n x =
    let n' = n mod (succ num_minor) in
    let major = n' = 0 in
    if x > xmax then acc
    else
      let tic =
        if major then
          Major (label_of_float labels x, x)
        else
          Minor x
      and x' = x +. d_minor in
      aux_tics (tic :: acc) (succ n') x'
  in
  if d_major > 0. then aux_tics [] 0 first_major else []

let tics xmin xmax = function
  | Fixed _ -> raise (Failure "Not yet implemented")
  | Fixed_norm _ -> raise (Failure "Not yet implemented")
  | Equidistants (labels, offset, d_major, num_minor) ->
      equi_labels offset d_major num_minor xmin xmax labels
  | Auto labels -> loose_labels xmin xmax labels

(*  val loose_labels : float -> float -> labels -> tic list*)
