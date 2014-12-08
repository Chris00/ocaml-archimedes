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
| No_label (** [No_label] just draw a vertical bar without any text *)
| Text of (string * float) array (** Not yet implemented *)
| Number of int (** [Number n] draw number labels using [n] digits *)
| Expnumber of float (** Not yet implemented *)
| Expnumber_named of float * string (** Not yet implemented*)
| Custom of (float -> string)
(** [Custom f] for labeling with a function [f] *)

type t =
| Fixed of labels * float list
(** [Archimedes.Tics.Fixed (label, tics)] draws major tics at values
    specified by the [tics] list and labels them using [label]. *)
| Fixed_norm of labels * float list
(** Not Yet implemented *)
| Equidistants of labels * float * float * int
(** [Archimedes.Tics.Equidistants (label, start, step, n)]
    draws [Major Tics] from [start] equidistants of [step] along the axe
    and with [n] [Minor Tics] between each of them and the labels defined
    with [label] *)
| Auto of labels
(** [Archimedes.Tics.Auto label] draws [Major Tics] to fit the axe and the
    [label] automaticaly*)

(**/**)
type tic =
| Major of string * float
(** Major Tic is a big vertical bar on an axe *)
| Minor of float
(** Minor Tic is a little vertical bar on an axe *)

val tics: ?log:bool -> float -> float -> t -> tic list
(** [tics xmin xmax spec] return a description of the tics for the
    interval [xmin .. xmax] according to the specification [spec].

    @param log whether log scales are desired.  Default: [false]. *)

(**/**)
