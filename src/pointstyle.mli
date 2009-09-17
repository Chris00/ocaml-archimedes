include Pointstyle_public.T

val render : name -> Backend.t -> unit
  (** This function renders the point style referenced by the name on
      the specified backend. Raises [Error name] if name does not refer
      to a point style.*)

val extents : name -> Matrix.rectangle
  (** Returns the extents of a point style. Raises [Error name] if
      name does not refer to a point style.*)

val render_extents : name -> Backend.t -> Matrix.rectangle
  (** [render_extents name backend] is equivalent to [render name
     backend; extents name], but is more efficient (access only once
     to the registered point style).  Raises [Error name] if name does
     not refer to a point style.*)

