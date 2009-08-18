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
  val text_extents : t -> string -> rectangle
    (** Returns a rectangle whose width and height specify
        respectively the length and the height of the text. The x and
        y values give the lower bottom point of the rectangle as if
        the text was placed at the origin.*)
  val show_text : t -> rotate:float -> x:float -> y:float ->
    text_position -> string -> unit
    (** [show_text t angle x y pos txt] displays [txt] at the point
        ([x],[y]) as indicated by [pos].  The point ([x],[y]) is in
        the current coordinate system but the current transformation
        matrix will NOT be applied to the text itself.  [angle]
        indicates by how many radians the text must be rotated
        w.r.t. the x-axis (in the current coordinate system, assuming
        it is orthonormal) -- not all device support rotations of
        angles [<> 0.] (in device coordinates).  This is an immediate
        operation: no [stroke] nor [fill] are required (nor will have
        any effect).  *)
end

include T

type error =
  | Corrupted_dependency of string
  | Non_loadable_dependency of string
  | Nonexistent of string  (** Cannot find the backend in the directories *)
  | Not_loadable of string * Dynlink.error
      (** Cannot load the backend because of the dynlink error. *)
  | Not_registering of string (** Not applying the {!Backend.Register}
                                  functor. *)

val string_of_error : error -> string

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


(************************************************************************)
(** {2 Affine transformations} *)

(** Module implementing affine transformations and various operations
    on them. *)
module Matrix :
sig
  type t = matrix

  exception Not_invertible

  val make_identity : unit -> t
    (** [make_identity()] returns the identity transformation. *)

  val make_translate : x:float -> y:float -> t
    (** [make_translate tx ty] returns a transformation that translates
        by [tx] and [ty] in the X and Y dimensions, respectively. *)

  val make_scale : x:float -> y:float -> t
    (** [make_scale sx sy] returns a transformation that scales by [sx]
        and [sy] in the X and Y dimensions, respectively. *)

  val make_rotate : angle:float -> t
    (** [make_rotate radians] returns a transformation that rotates
        by [radians]. *)

  val set_to_identity : t -> unit
    (** Sets the current transformation to the identity transformation. *)

  val copy: t -> t
    (** [copy matrix] returns a copy of [matrix]. *)

  val blit : t -> t -> unit
    (** [blit m1 m2] copies the content of [m1] into [m2]. *)

  val translate : t -> x:float -> y:float -> unit
    (** [translate m tx ty] applies a translation by [tx], [ty] to the
        transformation in [m].  The effect of the new transformation
        is to {i first} translate the coordinates by [tx] and [ty],
        then apply the original transformation to the coordinates. *)

  val scale : t -> x:float -> y:float -> unit
    (** [scale m sx sy] applies scaling by [sx], [sy] to the
        transformation in [m].  The effect of the new transformation
        is to {i first} scale the coordinates by [sx] and [sy], then
        apply the original transformation to the coordinates. *)

  val rotate : t -> angle:float -> unit
    (** [rotate m radians] applies rotation by [radians] to the
        transformation in [m].  The effect of the new transformation
        is to {i first} rotate the coordinates by [radians], then
        apply the original transformation to the coordinates. *)

  val invert : t -> unit
    (** [invert m] changes [matrix] to be the inverse of it's original
        value.  Not all transformation matrices have inverses; if the
        matrix collapses points together (it is degenerate), then it
        has no inverse and this function will raise
        {!Backend.Matrix.Not_invertible}. *)

  val det : t -> float
    (** [det m] returns the determinant of the linear part of [m].  It
        is the (signed) area that gets the unit square after
        transformation.  *)

  val mul : t -> t -> t
    (** [multiply b a] multiplies the affine transformations in [a]
        and [b] together and return the result.  The effect of the
        resulting transformation is to {i first} apply the
        transformation in [a] to the coordinates and then apply the
        transformation in [b] to the coordinates.

        BEWARE that the order of the arguments is different from
        e.g. {!Cairo.Matrix.multiply}. *)

  val mul_in : t -> t -> t -> unit
    (** [mul_in c b a] computes [mul b a] and put the result in [c]. *)

  val transform_point : t -> x:float -> y:float -> float * float
    (** [transform_point m x y] transforms the point ([x], [y]) by [m]. *)

  val transform_distance : t -> dx:float -> dy:float -> float * float
    (** [transform_distance m dx dy] transforms the distance vector
        ([dx],[dy]) by [m].  This is similar to
        {!Cairo.Matrix.transform_point} except that the translation
        components of the transformation are ignored.  The calculation
        of the returned vector is as follows:
        {[
        dx2 = dx1 * a + dy1 * c;
        dy2 = dx1 * b + dy1 * d;
        ]}
        Affine transformations are position invariant, so the same
        vector always transforms to the same vector.  If (x1,y1)
        transforms to (x2,y2) then (x1+dx1,y1+dy1) will transform to
        (x1+dx2,y1+dy2) for all values of x1 and x2.  *)

  val inv_transform_point : t -> x:float -> y:float -> float * float
    (** Makes the inverse transformation of a point. *)

  val inv_transform_distance : t -> dx:float -> dy:float -> float * float
    (** Makes the inverse transformation of a distance. *)

  val has_shear: t -> bool
    (** Tests whether the transformation has shears.  This is also the
       case if the transformation does a rotation.  *)
end
