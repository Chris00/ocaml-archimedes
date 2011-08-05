(* File: axes.mli

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

(** Routines to draw basic axes systems in a 2-dimensional space. One can
    either draw axes separately using add_(x|y)_axis or use a full default
    axes system with box or cross. *)

(** The axis can be padded using an offset. It is used to control
    where to place the axis (according to the other axis) *)
type offset =
| Relative of float
(** A relative offset is given in the Data coordinate system. So
    you can ensure that the axis is drawn at the other axis'
    origin (offset: Relative 0.) *)
| Absolute of float
(** An absolute offset is given in the Graph coordinate system and
    should have a value between 0 and 1. Using this kind of
    offset, one can ensure to always get the same rendering *)

val x : ?grid:bool ->
  ?major:(string * float) -> ?minor:(string * float) ->
  ?start:Arrows.style -> ?stop:Arrows.style ->
  ?tics:Tics.t -> ?offset:offset -> Viewport.t -> unit
(** [x vp] adds an x-axis to the viewport [vp].

    @param major is a couple [(mark, size)] drawn at each major tic
    position.

    @param start the arrow ending style on the left (x0) (see the
    Arrows module)

    @param stop the arrow ending style on the right (xend) (see the
    Arrows module)

    @param tics the "tics policy" for this axis (see the Tics module)

    @param offset where to place the axis (y-coordinate) *)

val y : ?grid:bool ->
  ?major:(string * float) -> ?minor:(string * float) ->
  ?start:Arrows.style -> ?stop:Arrows.style ->
  ?tics:Tics.t -> ?offset:offset -> Viewport.t -> unit
(** [y vp] adds an y-axis to the viewport [vp].

    @param start the arrow ending style on the bottom (y0) (see the
    Arrows module)

    @param stop the arrow ending style on the top (yend) (see the
    Arrows module)

    @param tics the "tics policy" for this axis (see the Tics module)

    @param offset where to place the axis (x-coordinate) *)

val box : ?grid:bool -> ?tics:Tics.t -> ?tics_alt:Tics.t -> Viewport.t -> unit
(** [box vp] A default system of axes consisting of four axes, one on
    each border of the viewport [vp], resulting in a box surrounding
    the viewport.

    @param tics the "tics policy" for the left and bottom axes (see
    the Tics module for more information over tics policies)

    @param tics_alt the "tics policy" for the right and top axes (see
    the Tics module for more information over tics policies) *)

val cross : ?tics:Tics.t -> Viewport.t -> unit
(** [cross vp] A default axes system consisting of two axes, centered
    on the origin ((0, 0) in Data coordinates).

    @param tics the "tics policy" of the axes (see the Tics module for
    more information over tics policies) *)
