(* File: arrows.mli

   Copyright (C) 2009-2011

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


(** Arrow styles  *)

(** Style of the arrow end. Below are textual representations of
    those endings *)
type style =
| Unstyled      (** ------- *)
| Simple        (** ------> *)
| Double        (** ----->> *)
| Triple        (** ---->>> *)
| Diamond       (** -----<> *)
| Circle        (** ------O *)
| Stop          (** ------| *)
| Custom of (Path.t -> unit) (** It is also possible to give a path in
                                the Custom style, leading to a
                                completely customised arrow *)

val path_line_to : ?size:float -> ?head:style -> ?tail:style ->
  Path.t -> float -> float -> unit
(** [path_line_to p x y] Adds an arrow to ([x], [y]) into the path
    [p].  See {!line} for explantation on the optional arguments. *)

val line_direct : ?size:float -> ?head:style -> ?tail:style ->
  Viewport.t -> float -> float -> float -> float -> unit -> unit
(** [line_direct vp x0 y0 x y ()] draws a line directly on the
    viewport, withtout using an instruction (see {!line} for usage) *)

val line : ?size:float -> ?head:style -> ?tail:style ->
  Viewport.t -> float -> float -> float -> float -> unit
(** [line vp x0 y0 x y] Draws a arrowed line on the viewport [vp] from
    ([x0], [y0]) to ([x], [y]) using an instruction (the drawing of
    the line is put on the queue of elements to draw on the viewport)

    @param size the size of the endings, in marks size

    @param head the head ending style

    @param tail the tail ending style *)

val arc_direct : ?size:float -> ?head:style -> ?tail:style ->
  Viewport.t -> float -> float -> float -> float -> float -> unit -> unit
(** [arc_direct vp x0 y0 r a1 a2 ()] draws an arc directly on the
    viewport, withtout using an instruction (see {!arc} for usage) *)

val arc : ?size:float -> ?head:style -> ?tail:style ->
  Viewport.t -> float -> float -> float -> float -> float -> unit
(** [arc vp x0 y0 r a1 a2] Draws a arrowed arc on the viewport [vp]
    from ([x0], [y0]) with a starting angle [a1], a ending angle [a2] and
    a radius [r]. Note that the starting point ([x0], [y0]) is called the
    tail of the arrow.

    @param size the size of the endings, in marks size

    @param head the head ending style

    @param tail the tail ending style *)
