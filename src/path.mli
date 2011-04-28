(* File: path.mli

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

type path_data =
  | MOVE_TO of float * float
  | LINE_TO of float * float
  | RECTANGLE of float * float * float * float
      (* RECTANGLE(x, y, width, height) *)
  | CURVE_TO of float * float * float * float * float * float * float * float
  | CLOSE_PATH of float * float

type t = path_data list

val make: unit -> t

val clear: t -> unit

val move_to: t -> x:float -> y:float -> unit

val line_to: t -> x:float -> y:float -> unit

val rel_move_to: t -> x:float -> y:float -> unit

val rel_line_to: t -> x:float -> y:float -> unit

val rectangle: t -> x:float -> y:float -> w:float -> h:float -> unit

val curve_to: t -> x1:float -> y1:float -> x2:float -> y2:float ->
  x3:float -> y3:float -> unit

val arc: t -> r:float -> a1:float -> a2:float -> unit

val close: t -> unit
