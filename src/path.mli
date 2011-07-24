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


(** Creating abstract paths. *)

type t

val make: unit -> t
(** [make ()] creates a new empty path *)

val copy: t -> t
(** [copy p] copies a path *)

val make_at: float -> float -> t
(** [make x y] creates a new empty path and moves it to ([x], [y]) *)

val clear: t -> unit
(** [clear p] clears the path (removes all operations) *)

val extents: t -> Matrix.rectangle
(** [extents p] returns a copy of the path's extents *)

val move_to: t -> x:float -> y:float -> unit
(** [move_to p x y] moves the path's current point to ([x], [y]) *)

val line_to: t -> x:float -> y:float -> unit
(** [line_to p x y] draws a line from the path's current point to ([x],
    [y]) and sets the current point to ([x], [y]) *)

val rel_move_to: ?rot:float -> t -> x:float -> y:float -> unit
(** [rel_move_to p x y] shifts the path's current point of [x]
    horizontally and [y] vertically.

    @param rot to consider a rotation of [rot] radians (default: [0.]). *)

val rel_line_to: ?rot:float -> t -> x:float -> y:float -> unit
(** [rel_line_to p x y] shifts the path's current point of [x]
    horizontally and [y] vertically and draws a line between the
    current and the new point.

    @param rot (default: 0.) to consider a rotation of [rot] radians *)

val rectangle: t -> x:float -> y:float -> w:float -> h:float -> unit
(** [rectangle p x y w h] draws a rectangle specified by ([x], [y], [w],
    [h]) but does not move the path's current point. *)

val curve_to: t -> x1:float -> y1:float -> x2:float -> y2:float ->
  x3:float -> y3:float -> unit
(** [curve_to p x1 y1 x2 y2 x3 y3] draws a cubic Bezier curve using the
    path's current point as first point (x0, y0) if it is set, else
    ([x1, y1]).  Sets the path's current point to ([x3], [y3]) *)

val arc: t -> r:float -> a1:float -> a2:float -> unit
(** [arc p r a1 a2] draws an arc starting at the path's current
    point. The starting angle is [a1], the radius [r] and the arc is
    drawn clockwise to the angle [a2]. The angles are given in
    radians.  *)

val close: t -> unit
(** [close p] Closes the path. It is usually not required to close a
    path, this is useful only to ensure the path won't be extended. *)

val stroke_on_backend: ?limits:float * float * float * float ->
  t -> Backend.t -> unit
(** [stroke_on_backend p bk] strokes the path [p] on the backend
    [bk]. It will not clear the path [p] but will clear the path of
    [bk].

    @param limits a quartet of float indicating where to clip the
    path. Default: the unit rectangle. One can use (-infinity,
    -infinity, infinity, infinity) for no clip. *)

val fill_on_backend: ?limits:float * float * float * float ->
  t -> Backend.t -> unit
(** [fill_on_backend p bk] fills in the path [p] on the backend
    [bk]. It will not clear the path [p] but will clear the path of
    [bk].

    @param limits a quartet of float indicating where to clip the
    path. Default: the unit rectangle. One can use (neg_infinity,
    neg_infinity, infinity, infinity) for no clip. *)

val current_point: t -> float * float
(** [current_point p] returns the current point of the path *)

val transform: t -> (float * float -> float * float) -> t
(** [transform p f] returns a new path that is the path [p] transformed by
    function [f]. It only modifies end points of paths primitives and
    extents are leaved the same. *)

val print_path: t -> unit
(** [print_path p] Debug function (TODO hide it) *)

val add: t -> t -> unit
(** [add p to_add] Adds the path [to_add] to the end of [p] *)

