(** Systems of coordinates (inhomogeneous homotheties) relative to
    other coordinate systems with automatic updates.  The automatic
    update refers to the fact that, if a coordinate system is upated,
    all coordinate systems which depend on it (possibly through
    several intermediate coordinate systems), they will use the
    updated version. *)

type t
(** Mutable affine coordinate system. *)

type ctm
(** Current transformation matrix of the backend (to be able to
    restore it with {!Coordinate.restore}. *)

val use : Backend.t -> t -> ctm
(** After a call to [use b c], all backend operations will be
    performed in the coordinates [c].  It returns the current
    coordinate system so one can restore it with
    {!Coordinate.restore}. *)

val restore : Backend.t -> ctm -> unit
(** [restore b c] restore the coordinate transformation matrix [ctm]
    for the backend [b]. *)


(** {2 Transforming coordinates} *)

val to_parent : t -> x:float -> y:float -> float * float
(** [to_parent coord x y] returns the location of the point [(x,y)]
    in parent's coordinates.*)

val from_parent : t -> x:float -> y:float -> float * float
(** [from_child coord x y] returns the location of the point [(x,y)]
    from parent's coordinates. *)

val to_device : t -> x:float -> y:float -> float * float
(** [to_device coord x y] returns the location of the point [(x,y)]
    in device coordinates.*)

val to_device_distance : t -> dx:float -> dy:float -> float * float
(** [to_device coord dx dy] returns the distance [(dx,dy)] in device
    coordinates (i.e. the translation in [coord] is ignored).  *)

val to_coord : t -> x:float -> y:float -> float * float
(** [to_coord coord x y] converts the (device) point [(x,y)] into
    the corresponding point, expressed in [coord] coordinates. *)

val to_coord_distance : t -> dx:float -> dy:float -> float * float
(** [to_coord coord x y] converts the (device) distance [(dx,dy)]
    into the corresponding distance, expressed in [coord]
    coordinates. *)


(** {2 Creating new coordinate systems} *)

val make_root : Matrix.Homothety.t -> t
(** [make_root m] make a system of coordinates which, when used,
    amounts to use [m].  This coordinate system depends on no
    other  so will never be updated.  It can be modified however
    (the matrix [m] is copied so no modification will affect [m]). *)

val make_identity : t -> t
(** [make_identity coord] defines a new system of coordinates that
    initially consist in the identity transformation to [coord]. *)

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

val make_from_transform : t -> Matrix.Homothety.t -> t
(** [make_from_transform coord tm] defines a new coordinate system
    that consists first in applying [tm] and then the tranformation in
    [coord].  In other words, [tm] is the transformation from the
    desired coordinate system to [coord].  If [coord] is modified, the
    new system will be updated as well. *)

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
    it (transitively) depends on was mofidied) since the last [reset]. *)

