exception Error of string

type name = string
(**Type to specify a point style*)

val add : name:name -> (Backend.t -> unit) -> Backend.rectangle -> unit
val render : name -> Backend.t -> unit
val extents : name -> Backend.rectangle
val render_extents : name -> Backend.t -> Backend.rectangle
