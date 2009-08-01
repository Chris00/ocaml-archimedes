type t = Backend.matrix
  (**The type storing a coordinate transformation.*)

val identity : unit -> t
  (**Identity transformation.*)

val create : float -> float -> float -> float -> float -> float -> t
  (**To create a transformation, specify the four components of the
  rotation matrix, and two components of translation.*)

val translate : t -> float -> float -> unit
  (**Modify t so that after the initial transformation, a translation
  of the specified vector is performed.*)

val scale : t -> float -> float -> unit
  (**Modify t so that a rescaling is done.*)

val rotate : t -> float -> unit
  (**Rotation of angle*)

val transform : t -> float -> float -> float * float
  (**[transform t x y] transforms the point (x,y) under the transformation [t]*)

val transform_dist : t -> float -> float -> float * float
  (**[transform t x y] transforms the distance (vector) (x,y) under the
     transformation [t]. (This implies that no translation is done.*)

val det : t -> float
  (**Returns the determinant of the rotation transformation. It is
     precisely, up to sign, the area that gets the unit square after
     transformation.*)

val invert : t -> t
  (**Gets the inverse transformation of the parameter.*)

val inv_transform : t -> float -> float -> float * float
  (**Makes the inverse transformation of a point.*)

val inv_transform_dist : t -> float -> float -> float * float
  (**Makes the inverse transformation of a distance (vector).*)

val apply : ?result:t -> next_t:t -> t -> unit
  (**Applies the transformations contained in [next_t] to the results
  given by [t]. The resulting transformation is stored in [result] or,
  if not given, in [t].*)

val copy:t -> t
  (**Returns a fresh copy of its argument.*)

val reset_to_id : t -> unit
  (**Resets the current transformation to the identity transformation.*)


(*Local variables:*)
(*compile-command: "ocamlc -c coordinate.mli"*)
(*End:*)
