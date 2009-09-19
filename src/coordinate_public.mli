(** Affine systems of coordinates relative to other coordinate systems
    with automatic updates. *)
module type T = sig
  type t
    (** Mutable affine coordinate system. *)

  type ctm
    (** Current transformation matrix of the backend (to be able to
        restore it with {!Coordinate.restore}. *)

  val use : Backend.t -> t -> ctm
    (** After a call to [use b c], all backend operations will be
        performed in the corrdinates [c].  It returns the current
        coordinate system so one can restore it with
        {!Coordinate.restore}. *)

  val restore : Backend.t -> ctm -> unit
    (** [restore b c] restore the coordinate transformation matrix [ctm]
        for the backend [b]. *)


  (** {2 Transforming coordinates} *)

  val to_device : t -> x:float -> y:float -> float * float
    (**[to_device coord x y] returns the location of the point [(x,y)]
       in device coordinates.*)

  val to_device_distance : t -> dx:float -> dy:float -> float * float
    (**[to_device coord dx dy] returns the distance of [(dx,dy)]
       in device coordinates.*)

  val to_coord : t -> x:float -> y:float -> float * float
    (**[to_coord coord x y] converts the (device) point [(x,y)] into
       the corresponding point, expressed in [coord] coordinates.*)

  val to_coord_distance : t -> dx:float -> dy:float -> float * float
    (**[to_coord coord x y] converts the (device) distance [(dx,dy)] into
       the corresponding distance, expressed in [coord] coordinates.*)


  (** {2 Creating new coordinate systems} *)

  val make_root : Matrix.t -> t
    (** [make_from m] make a system of coordinates which, when used,
        amounts to use [m].  This coordinate system depends on no
        other so will never be updated -- but can be modified. *)

  val make_translate : t -> x:float -> y:float -> t
    (** [make_translate coord x y] defines a new coordinate system that
        consists in moving the origin of [coord] to the point [(x,y)]
        (understood as coordinates in the system [coord]).  If [coord]
        is modified, the new system will be updated as well. *)

  val make_scale : t -> x:float -> y:float -> t
    (** [make_scale coord x y] defines a new coordinate system that
        consists in dilating axis X and Y of [coord] by a factor of [x]
        and [y] respectively.  If [coord] is modified, the new system
        will be updated as well. *)

  val make_rotate : t -> angle:float -> t
    (** [make_rotate coord a] defines a new coordinate system that
        consists in rotating the axis X and Y of [coord] by [a] radians
        (assuming the axis of the system [coord] are orthonormal).  If
        [coord] is modified, the new system will be updated as well.  *)

  val make_from_transform : t -> Matrix.t -> t
    (** [make_from_transform coord tm] defines a new coordinate system
        that consists first in applying [tm] and then the tranformation
        in [coord].  In other words, [tm] is the affine transformation
        from the desired coordinate system to [coord].  If [coord] is
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

  val transform : t -> Matrix.t -> unit
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
        to was updated (possibly because of one of the coordinate systems
        it (transitively) depends on was mofidied) since the last
        [reset]. *)
end
