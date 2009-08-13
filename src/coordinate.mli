(* File: coordinate.mli

   Copyright (C) 2009

     Bertrand Desmons <Bertrand.Desmons@umons.ac.be>
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

(** Affine systems of coordinates relative to other coordinate systems
    with automatic updates. *)

type t
  (** Mutable affine coordinate system. *)

type ctm
  (** Current transformation matrix of the backend (to be able to
      restore it through {!Coordinate.restore}. *)

val use : Backend.t -> t -> ctm
  (** After a call to [use b c], all backend operations will be
      performed in the corrdinates [c].  It returns the current
      coordinate system so one can restore it with
      {!Coordinate.restore}. *)

val restore : Backend.t -> ctm -> unit
  (** [restore b c] restore the coordinate transformation matrix [ctm]
      for the backend [b]. *)


(** {2 Creating new coordinate systems} *)

val make_identity : unit -> t
  (** Make a system of coordinates which, when used, amounts to plot
      in the "raw" device coordinates. *)

val make_translate : t -> x:float -> y:float -> t
  (** [make_translate coord x y] defines a new coordinate system that
      consists in moving the origin of [coord] to the point [(x,y)]
      (understood as coordinates in the system [coord]).  If [coord]
      if modified, the new system will be updated as well. *)

val make_scale : t -> x:float -> y:float -> t
  (** [make_scale coord x y] defines a new coordinate system that
      consists in dilating axis X and Y of [coord] by a factor of [x]
      and [y] respectively.  If [coord] if modified, the new system
      will be updated as well. *)

val make_rotate : t -> angle:float -> t
  (** [make_rotate coord a] defines a new coordinate system that
      consists in rotating the axis X and Y of [coord] by [a] radians
      (assuming the axis of the system [coord] are orthonormal).  If
      [coord] if modified, the new system will be updated as well.  *)

val make_from_transform : t -> Backend.Matrix.t -> t
  (** [make_from_transform coord tm] defines a new coordinate system
      that consists first in applying [tm] and then the tranformation
      in [coord].  In other words, [tm] is the affine transformation
      from the desired coordinate system to [coord].  If [coord] if
      modified, the new system will be updated as well. *)

val copy : t -> t
  (** Returns a completely independent copy of the current coordinate
      system. *)


(** {2 Modifying this coordinate system} *)

val translate : t -> x:float -> y:float -> unit
  (** [translate coord x y] modifies the coordinate system [coord]
      translating its origin to the point [(x,y)] (understood as
      coordinates in the system [coord]). *)

val scale : t -> x:float -> y:float -> unit
  (** [scale coord x y] modifies the coordinate system [coord]
      dilating its axis X and Y by a factor of [x] and [y]
      respectively. *)

val rotate : t -> angle:float -> unit
  (** [rotate coord a] modifies the coordinate system [coord] rotating
      its axis X and Y by [a] radians (assuming the axis of the system
      [coord] are orthonormal). *)

val transform : t -> Backend.Matrix.t -> unit
  (** [transform coord tm] modifies the coordinate system [coord]
      changing the transformation matrix to its parent (the one it was
      created from) to [tm]. *)


(** {2 Monitoring coordinate systems for updates} *)

type monitor
  (** Handle to monitor the updates to a coordinate system. *)

val monitor : t -> monitor
  (** [monitor coord] creates a new monitor for changes to [coord]
      (initially not set). *)

val reset : monitor -> unit
  (** [reset m] reset the monitor.  See {!Coordinate.changed}. *)

val changed : monitor -> bool
  (** [changed m] tell whether the coordinate system [m] is attached
      to was updated (possibly because of one of the coordinate sytems
      it (transitively) depends on was mofidied) since the last
      [reset]. *)
