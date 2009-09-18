include Coordinate_public.T

val make_root_from : Matrix.t -> t
  (** [make_root_from m] make coordinate system dpending on no other
      (and so never updated) which transformation matrix is [m].  *)
