(* File: utils.ml

   Copyright (C) 2011

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Bigarray

let is_nan (x: float) = x <> x
let is_inf y = 1. /. y = 0.
let is_finite x = neg_infinity < x && x < infinity
let min_float x y = if (x: float) <= y then x else y
let max_float x y = if (x: float) >= y then x else y

let ba_copy x =
  let x' = Array1.create (Array1.kind x) (Array1.layout x) (Array1.dim x) in
  Array1.blit x x';
  x'

let failwithf fmt = Printf.ksprintf failwith fmt
