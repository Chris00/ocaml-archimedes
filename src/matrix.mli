(** Module implementing affine transformations and various operations
    on them. *)


(** Holds an affine transformation, such as a scale, rotation, shear,
    or a combination of those. The transformation of a point (x, y) is
    given by:
    {[
    x_new = xx *. x +. xy *. y +. x0;
    y_new = yx *. x +. yy *. y +. y0;      ]} *)
type affine = { mutable xx: float; mutable yx: float;
                mutable xy: float; mutable yy: float;
                mutable x0: float; mutable y0: float; }

type t = affine

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
    {!Matrix.Not_invertible}. *)

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
    e.g. [Cairo.Matrix.multiply]. *)

val mul_in : t -> t -> t -> unit
(** [mul_in c b a] computes [mul b a] and put the result in [c]. *)

val transform_point : t -> x:float -> y:float -> float * float
(** [transform_point m x y] transforms the point ([x], [y]) by [m]. *)

val transform_distance : t -> dx:float -> dy:float -> float * float
(** [transform_distance m dx dy] transforms the distance vector
    ([dx],[dy]) by [m].  This is similar to
    {!Matrix.transform_point} except that the translation
    components of the transformation are ignored.  The calculation
    of the returned vector is as follows:
    {[
    dx2 = dx1 * xx + dy1 * xy;
    dy2 = dx1 * yx + dy1 * yy;
    ]}
    Affine transformations are position invariant, so the same
    vector always transforms to the same vector.  If (x1,y1)
    transforms to (x2,y2) then (x1+dx1,y1+dy1) will transform to
    (x2+dx2,y2+dy2) for all values of dx1 and dy1.  *)

val inv_transform_point : t -> x:float -> y:float -> float * float
(** Makes the inverse transformation of a point. *)

val inv_transform_distance : t -> dx:float -> dy:float -> float * float
(** Makes the inverse transformation of a distance. *)

val has_shear: t -> bool
(** Tests whether the transformation has shears.  This is also the
    case if the transformation does a rotation.  *)

(** A data structure for holding a rectangle. *)
type rectangle = {
  x:float;   (** X coordinate of the left side of the rectangle *)
  y:float;   (** Y coordinate of the the top side of the rectangle  *)
  w:float;   (** width of the rectangle  [>= 0]. *)
  h:float;   (** height of the rectangle [>= 0]. *)
}

val transform_rectangle: ?dist_basepoint:bool -> t -> rectangle -> rectangle
(** Transformation of rectangles. This returns the smallest
    rectangle containing the transformation of the rectangle argument
    by the matrix. The optional argument [dist_basepoint] has the
    following meaning:

    - Not specified: transform the base point as a point.
    - Specified as [true]: transform the base point as a distance.
    - Specified as [false]: no transformation of the base point.*)


(** Transformations that are the composition of translations and
    inhomogeneous dilations (different scaling factors are allowed in
    each canonical direction). *)
module Homothety :
sig
  type t
  (** See {!Matrix.t}, setting [xy = 0 = yx]. *)

  val of_matrix : affine -> t
  (** [of_matrix m] returns a copy of the transformation [m] if it
      contains no rotation or raise [Invalid_argument] otherwise. *)

  val to_matrix : t -> affine
  (** [to_matrix m] returns a copy of the transformation [m]. *)

  val make_identity : unit -> t
  (** See {!Matrix.make_identity}. *)
  val make_translate : x:float -> y:float -> t
  (** See {!Matrix.make_translate}. *)
  val make_scale : x:float -> y:float -> t
  (** See {!Matrix.make_scale}. *)
  val set_to_identity : t -> unit
  (** See {!Matrix.set_to_identity}. *)
  val copy: t -> t
  (** See {!Matrix.copy}. *)
  val blit : t -> t -> unit
  (** See {!Matrix.blit}. *)
  val translate : t -> x:float -> y:float -> unit
  (** See {!Matrix.translate}. *)
  val scale : t -> x:float -> y:float -> unit
  (** See {!Matrix.scale}. *)
  val invert : t -> unit
  (** See {!Matrix.invert}. *)
  val det : t -> float
  (** See {!Matrix.det}. *)

  val mul : t -> t -> t
  (** See {!Matrix.mul}. *)
  val mul_in : t -> t -> t -> unit
  (** See {!Matrix.mul_in}. *)
  val transform_point : t -> x:float -> y:float -> float * float
  (** See {!Matrix.transform_point}. *)
  val transform_distance : t -> dx:float -> dy:float -> float * float
  (** See {!Matrix.transform_distance}. *)
  val inv_transform_point : t -> x:float -> y:float -> float * float
  (** See {!Matrix.inv_transform_point}. *)
  val inv_transform_distance : t -> dx:float -> dy:float -> float * float
  (** See {!Matrix.inv_transform_distance}. *)

  val transform_rectangle: ?dist_basepoint:bool -> t -> rectangle -> rectangle
  (** See {!Matrix.transform_rectangle}. *)
end


(**/**)

val inv_transform_rectangle: ?dist_basepoint:bool -> t -> rectangle -> rectangle
(** Inverse transformation of rectangles. *)

external unsafe_to_homothety : t -> Homothety.t = "%identity"
(** Same as {!Homothety.of_matrix} but without checks and copying. *)

external unsafe_of_homothety : Homothety.t -> t = "%identity"
(** Same as {!Homothety.to_matrix} but without copying. *)



(* Local Variables: *)
(* compile-command: "make -C .." *)
(* End: *)
