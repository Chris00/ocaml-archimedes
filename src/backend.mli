(* File: backend.mli

   Copyright (C) 2009

     Bertrand Desmons <Bertrand.Desmons@umons.ac.be>
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
  | JOIN_MITER (** use a sharp (angled) corner *)
  | JOIN_ROUND (** use a rounded join, the center of the circle is the
                   joint point *)
  | JOIN_BEVEL (** use a cut-off join, the join is cut off at half the line
                   width from the joint point *)

(** A data structure for holding a rectangle. *)
type rectangle = {
  x:float;   (** X coordinate of the left side of the rectangle *)
  y:float;   (** Y coordinate of the the top side of the rectangle  *)
  w:float;   (** width of the rectangle *)
  h:float;   (** height of the rectangle  *)
}

(** Holds an affine transformation, such as a scale, rotation, shear,
    or a combination of those. The transformation of a point (x, y) is
    given by:
    {[
    x_new = xx *. x +. xy *. y +. x0;
    y_new = yx *. x +. yy *. y +. y0;
    ]} *)
type matrix = { mutable xx: float; mutable yx: float;
                mutable xy: float; mutable yy: float;
                mutable x0: float; mutable y0: float; }


type slant = Upright | Italic
    (** Specifies variants of a font face based on their slant. *)

type weight = Normal | Bold
    (** Specifies variants of a font face based on their weight. *)

type text_position =
  | CC  (** centrer horizontally and vertically *)
  | LC  (** align left horizontally and center vertically *)
  | RC  (** align right horizontally and center vertically *)
  | CT  (** center horizontally and align top vertically *)
  | CB  (** center horizontally and align bottom vertically *)
  | LT  (** align left horizontally and top vertically *)
  | LB  (** align left horizontally and bottom vertically *)
  | RT  (** align right horizontally and top vertically *)
  | RB  (** align right horizontally and bottom vertically *)


module type T =
sig
  type t
    (** Handle of a backend or a coordinate system. *)

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

  val rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit

  val arc : t -> x:float -> y:float -> r:float -> a1:float -> a2:float -> unit

  val close_path : t -> unit
    (** Adds a line segment to the path from the current point to the
        beginning of the current sub-path (the most recent point
        passed to {!Bmove_to}) and closes this sub-path. *)
  val clear_path : t -> unit
    (** Clears the current path. After this call there will be no path.
        Nothing is guaranteed about the current point. *)
  val path_extents : t -> rectangle

  val stroke : t -> unit
  val stroke_preserve : t -> unit
  val fill : t -> unit
  val fill_preserve : t -> unit

  val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
    (** Establishes a new clip rectangle by intersecting the current
        clip rectangle.  This {i may clear} the current path. *)

  val save : t -> unit
  val restore : t -> unit

  val translate : t -> x:float -> y:float -> unit
    (** [translate cr tx ty] modifies the current transformation
        matrix by translating the user-space origin by ([tx],[ty]). *)
  val scale : t -> x:float -> y:float -> unit
    (** [scale sx sy] modifies the current transformation matrix by
        scaling the X and Y user-space axes by [sx] and [sy]
        respectively. *)
  val rotate : t -> angle:float -> unit
    (** Modifies the current transformation matrix by rotating the
        user-space axes by [angle] radians. *)
  val set_matrix : t -> matrix -> unit
    (** Set the current transformation matrix which is the matrix
        transorming user to device coordinates. *)
  val get_matrix : t -> matrix
    (** Return the current transformation matrix.  Modifying this
        matrix should not affect the matrix held in [t]. *)

  val select_font_face : t -> slant -> weight -> string -> unit
    (** [select_font_face t slant weight family] selects a family
      and style of font from a simplified description as a family
      name, slant and weight.  Family names are bakend dependent.  *)
  val set_font_size : t -> float -> unit
    (** Set the scaling of the font. *)
  val show_text : t -> rotate:float -> x:float -> y:float ->
    text_position -> string -> unit
    (** [show_text angle x y pos txt] display [txt] at the point
        ([x],[y]) as indicated by [pos].  The point ([x],[y]) is in
        the current coordinate system but the current transformation
        matrix will NOT be applied to the text itself.  [angle]
        indicates by how many radians (in the current coordinate
        system) the text must be rotated -- [rotate <> 0.] may not be
        supported on all devices.  This is an immediate operation: no
        [stroke] nor [fill] are required (nor will have any effect).  *)
end

include T

type error =
  | Corrupted_dependency of string
  | Non_loadable_dependency of string
  | Nonexistent of string  (** Cannot find the backend in the directories *)
  | Corrupted of string    (** Cannot load the backend *)
  | Not_registering of string (** Not applying the {!Backend.Register}
                                  functor. *)

exception Error of error

val make : ?dirs:string list -> string -> float -> float -> t
  (** [make backend width height] creates a new backend of the given
      dimensions.

      [backend] is the name of the underlying engine, followed by one
      or several options separated by spaces.  For example, "Graphics"
      for the graphics backend or "Cairo PNG filename" for the Cairo
      backend, using a PNG surface to be saved in [filename]. *)

val close : t -> unit
  (** Close the handle.  For some backends, the output will not be
      complete until this function is called. *)

val height : t -> float
  (** Returns the width of the backend canvas. *)

val width : t -> float
  (** Returns the height of the backend canvas. *)

val registered: unit -> string list
  (** Return the list of registered (i.e. loaded) backends. *)

val available : dirs:string list -> string list
  (** Return the list of available backends in the given directories. *)


(************************************************************************)
(** {2 Registering new modules} *)

module type Capabilities =
sig
  include T

  val name : string
    (** Name under which to register the backend. *)

  val make : options:string list -> float -> float -> t
    (** [create options width height] must creates a new handle of
        size [width]×[height] (in units proper to the module) on which
        the subsequent drawing functions operate.  [options] allows to
        pass options to the backend (this is backend specific). *)

  val close : options:string list -> t -> unit
    (** Close the handle.  This function will be given the options
        specified at backend creation so it can react appropriately if
        some final work need to be done for some of them. *)
end

module Register(B: Capabilities) : sig end
  (** The {i side effect} of this functor application is to register
      the functions of the backend [B] under the name [B.name].

      A backend [B] must be declared in a file archimedes_[B.name]
      (compiled to a .cmo and/or .cmxs library) and the functor
      application must be executed as part of the initialisation code.
      We recommend the use of [let module U = Register(B) in ()] to
      perform the registration.  *)
