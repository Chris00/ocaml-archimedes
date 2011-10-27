(* File: tics.mli

   Copyright (C) 2009-present

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     Pierre Hauweele <pierre@hauweele.net>
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

(** Tics position and labels. *)


type labels =
| No_label
| Text of (string * float) array (* TODO use lists *)
| Number of int
| Expnumber of float
| Expnumber_named of float * string
| Custom of (float -> string)

type t =
| Fixed of labels * float list
| Fixed_norm of labels * float list
| Equidistants of labels * float * float * int
| Auto of labels

type tic =
| Major of string * float
| Minor of float

val tics: ?log:bool -> float -> float -> t -> tic list
(** [tics xmin xmax spec] return a description of the tics for the
    interval [xmin .. xmax] according to the specification [spec].

    @param log whether log scales are desired.  Default: [false]. *)
