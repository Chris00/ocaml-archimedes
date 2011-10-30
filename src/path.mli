(* File: path.mli

   Copyright (C) 2009-2015

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


(** Creating abstract paths. *)

(**/**)
type t
(** Abstract mutable path. *)
(**/**)

val make: unit -> t
(** [make ()] creates a new empty path. *)

val is_empty : t -> bool
(** Tells whether the path is empty. *)

val current_point : t -> float * float
(** Returns the point where the path currently ends. *)

val copy: t -> t
(** [copy p] copies a path *)

val clear: t -> unit
(** [clear p] clears the path (removes all operations). *)

val extents: t -> Matrix.rectangle
(** [extents p] returns the path's extents. *)

val move_to: t -> x:float -> y:float -> unit
(** [move_to p x y] moves the path current point to ([x], [y]) if both
    [x] and [y] are finite.  Otherwise, does nothing.  *)

val rel_move_to: t -> x:float -> y:float -> unit
(** [rel_move_to p x y] shifts the path's current point of [x]
    horizontally and [y] vertically, provided both [x] and [y] are
    finite.  Otherwise does nothing. *)

val line_to: t -> x:float -> y:float -> unit
(** [line_to p x y] draws a line from the path's current point to
    ([x], [y]) and sets the current point to ([x], [y]), provided both
    [x] and [y] are finite.  Otherwise does nothing. *)

val rel_line_to: t -> x:float -> y:float -> unit
(** [rel_line_to p x y] shifts the path's current point of [x]
    horizontally and [y] vertically and draws a line between the
    current and the new point, provided both [x] and [y] are finite.
    Otherwise does nothing. *)

val line_of_array: t -> ?i0:int -> ?i1:int ->
  ?const_x:bool -> float array -> ?const_y:bool -> float array -> unit
(** [line_of_array p x y] continue the current line (or start a new
    one) with the line formed by joining the points [x.(i), y.(i)],
    [i=i0,...,i1] (with possibly [i0 > i1] to indicate that the
    indices must be followed in decreasing order).  Points with at
    least one NaN or infinite coordinate are skipped when they are at
    the beginning or the end and result in a {!move_to} when
    separating finite points.

    @param const_x by setting it to [true], you indicate that you will
    not modify [x] (so it will not be copied).  Default: false.
    @param const_y Same as [const_x] but for [y].
    @param i0 start index.  Default: [0].
    @param i1 last index.  Default: [Array.length x - 1].
    @raise Failure if [y] is to small to possess the indices [i0 .. i1]. *)

type vec =
  (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array1.t

val line_of_vec: t -> ?i0:int -> ?i1:int ->
  ?const_x:bool -> vec -> ?const_y:bool -> vec -> unit
(** Same as {!line_of_array} but for FORTRAN bigarrays. *)

type cvec = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

val line_of_cvec: t -> ?i0:int -> ?i1:int ->
  ?const_x:bool -> cvec -> ?const_y:bool -> cvec -> unit
(** Same as {!line_of_array} but for C bigarrays. *)


val rectangle: t -> x:float -> y:float -> w:float -> h:float -> unit
(** [rectangle p x y w h] draws a rectangle specified by ([x], [y],
    [w], [h]) if all four quantities [x], [y], [w] and [h] are finite.
    Does nothing otherwise. *)

val curve_to: t -> x1:float -> y1:float -> x2:float -> y2:float ->
  x3:float -> y3:float -> unit
(** [curve_to p x1 y1 x2 y2 x3 y3] draws a cubic Bezier curve using
    the path's current point as first point (x0, y0) if it is set,
    else ([x1, y1]).  Sets the path's current point to ([x3], [y3]).
    The above holds provided all values [x1], [y1], [x2], [y2], [x3],
    and [y3] are finite.  The function does nothing otherwise. *)

val arc: t -> r:float -> a1:float -> a2:float -> unit
(** [arc p r a1 a2] draws an arc starting at the path's current
    point. The starting angle is [a1], the radius [r] and the arc is
    drawn clockwise to the angle [a2]. The angles are given in
    radians.  The above holds provided the three values [r], [a1], and
    [a2] are finite.  The function does nothing otherwise.  *)

val close: t -> unit
(** [close p] Closes the path. It is usually not required to close a
    path, this is useful only to ensure the path won't be extended. *)

val current_point: t -> float * float
(** [current_point p] returns the current point of the path.
    @raise Failure if no current point exists. *)

val append: t -> t -> unit
(** [append p1 p2] append the path [p2] to the end of [p1] and {i
    clear} [p2].  The current point of [p1] becomes the one of
    [p2]. *)

val transform : Matrix.t -> t -> t
(** [transform m p] returns a new path resulting from applying the
    affine transformation [m] to [p]. *)

(**/**)

(*----------------------------------------------------------------------*)
(** {4 Internal functions for efficient path rendering} *)

(** All the float quantities in a path data are guaranteed to be
    finite. *)
type data = private
  | Move_to of float * float
  | Line_to of float * float
  | Curve_to of float * float * float * float * float * float * float * float
    (* Bézier curve *)
  | Close of float * float
  (* Optimizations for some specific data structures that are used
     for caching data points.  *)
  | Array of float array * float array * int * int
  (** [Array(x, y, i0, i1)] correspond to [line_to x.(i) y.(i)] for [i
      = i0,...,i1].  Beware that it is possible that [i0 > i1] to
      indicate that indices must be followed in decreasing order.  It
      is guaranteed that [x] and [y] have the same length and that
      [i0] and [i1] are valid indices. *)
  | Fortran of vec * vec * int * int
  | C of cvec * cvec * int * int

val subpath_x : t -> float
(** Beginning of the current subpath (if the path is non empty which
    is not checked). *)

val iter : t ->  (data -> unit) -> unit
(** [iter p f] iterates [f] on all components of the path [p]. *)

val fprint: out_channel -> ?update_extents:bool -> t -> unit
(** [print_path p] Debug function. *)

val unsafe_line_of_array : t -> float array -> float array -> int -> int -> unit
(** [unsafe_line_of_array vp x y i0 i1]: same as {!line_of_array}
    except that the arrays are ASSUMED to be of the same length, the
    indices valid, and the arrays are NOT copied.  The presence of
    points that are not-finite is checked. *)

val unsafe_subpath_line_of_array :
  t -> float array -> float array -> int -> int -> (t -> int -> int -> unit) -> unit
(** [unsafe_subpath_line_of_array vp x y i0 i1 f] does the same as
    {!unsafe_line_of_array} except that [f subpath j0 j1] is called on
    each subpath (each contiguous set of indices [j0]..[j1] in the
    range [i0]..[i1] delimited by non-finite points). *)

val unsafe_line_of_vec: t -> vec -> vec -> int -> int -> unit
(** Same as {!line_of_vec} except that the arrays are ASSUMED to be of
    the same length, the indices valid, and the arrays are NOT copied. *)

val unsafe_subpath_line_of_vec: t -> vec -> vec -> int -> int ->
  (t -> int -> int -> unit) -> unit
(** See {!unsafe_subpath_line_of_array}. *)

val unsafe_line_of_cvec: t -> cvec -> cvec -> int -> int -> unit
(** Same as {!line_of_cvec} except that the arrays are ASSUMED to be
    of the same length, the indices valid, and the arrays are NOT copied. *)

val unsafe_subpath_line_of_cvec:
  t -> cvec -> cvec -> int -> int -> (t -> int -> int -> unit) -> unit
(** See {!unsafe_subpath_line_of_array}. *)

val bezier_of_arc : 'a ->
  ('a -> x0:float -> y0:float -> x1:float -> y1:float -> x2:float -> y2:float ->
   x3:float -> y3:float -> unit) ->
  x0:float -> y0:float -> r:float -> a1:float -> a2:float -> unit
(** [bezier_of_arc f x0 y0 a1 a2] transform the arc stating at
    [(x0,y0)] into bezier curves, calling [f] on each bezier curve
    produced. *)

val map: t -> (float * float -> float * float) -> t
(** [transform p f] returns a new path that is the path [p] where all
    points (xi, yi) are substituted by their image [f xi yi].  It
    only modifies end points of paths, primitives and extents are
    leaved the same. *)
