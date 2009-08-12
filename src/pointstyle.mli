exception Error of string

type name = string
(**Type to specify a point style*)

val add : name:name -> (Backend.t -> unit) -> unit
val render : name -> Backend.t -> unit
