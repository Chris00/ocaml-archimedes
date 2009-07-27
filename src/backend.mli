(* File: backend.mli

   Copyright (C) 2009

     Bertrand Desmons <Bertrand.Desmons@student.umons.ac.be>
     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


(** Module managing the dynamic loading of the backends. *)


type line_cap =
  | BUTT  (** start(stop) the line exactly at the start(end) point *)
  | ROUND (** use a round ending, the center of the circle is the end point *)
  | SQUARE (** use squared ending, the center of the square is the end point *)

type line_join =
  | JOIN_MITER (** use a sharp (angled) corner, see {!Cairo.set_miter_limit} *)
  | JOIN_ROUND (** use a rounded join, the center of the circle is the
                   joint point *)
  | JOIN_BEVEL (** use a cut-off join, the join is cut off at half the line
                   width from the joint point *)

type slant = Upright | Italic

type weight = Normal | Bold


exception Non_existent

val registered: unit -> string list
  (** Return the list of registered backends. *)

val load: ?exts:string * string * string -> ?dep:string  -> ?unsafe:bool ->
  string -> unit


module type T =
sig
  type t
    (** Handle of a backend or a coordinate system. *)

  val close : t -> unit
    (** Close the handle.  For some backends, the output will not be
        complete until this function is called. *)

  val set_color : t -> Color.t -> unit
  val set_line_width : t -> float -> unit
  val set_line_cap : t -> line_cap -> unit
  val set_dash : t -> float -> float array -> unit
  val set_line_join : t -> line_join -> unit

  val get_line_width: t -> float
  val get_line_cap: t -> line_cap
  val get_dash: t -> float array * float
  val get_line_join: t -> line_join

  val move_to : t -> x:float -> y:float -> unit
  val line_to : t -> x:float -> y:float -> unit
  val rel_move_to : t -> x:float -> y:float -> unit
  val rel_line_to : t -> x:float -> y:float -> unit

  val curve_to : t ->
    x1:float -> y1:float -> x2:float -> y2:float -> x3:float -> y3:float -> unit

  val rectangle : t -> x:float -> y:float -> width:float -> height:float -> unit

  val arc : t -> x:float -> y:float -> r:float -> a1:float -> a2:float -> unit

  val close_path : t -> unit

  val path_extents : t -> rectangle

  val stroke : t -> unit
  val stroke_preserve : t -> unit
  val fill : t -> unit
  val fill_preserve : t -> unit
  val clip : t -> unit
  val clip_preserve : t -> unit

  val save : t -> unit
  val restore : t -> unit

  val translate : t -> x:float -> y:float -> unit
  val scale : t -> x:float -> y:float -> unit
  val rotate : t -> angle:float -> unit

  val text : t -> ?size:float -> x:float -> y:float -> string -> unit
end

include T

val make : string -> float -> float -> t

val height : t -> float
  (** Returns the width of the backend canvas. *)

val width : t -> float
  (** Returns the height of the backend canvas. *)


(************************************************************************)
(** {2 Registering new modules} *)

module type Capabilities =
sig
  include T

  val make : string -> float -> float -> t
    (** [create options width height] must creates a new handle of
        size [width]×[height] (in units proper to the module) on which
        the subsequent drawing functions operate.  [options] allows to
        pass options to the backend (this is backend specific). *)

  val name : string
    (** Name under which to register the backend. *)
end

module Register(B: Capabilities) : sig end
  (** The {i side effect} of this functor application is to register
      the functions of the backend [B] under the name [B.name].

      A backend [B] must be declared in a file archimedes_[B.name]
      (compiled to a .cmo and/or .cmxs library) and the functor
      application must be executed as part of the initialisation code.
      We recommend the use of [let module U = Register(B) in ()] to
      perform the registration.  *)

(* Are [save] and [restore] needed?  Is [clip] needed ?  How to
   implement it for graphics ?  Do we want to rotate text ?
*)
