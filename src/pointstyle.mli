exception Error of string

val add : name:string -> (Backend.t -> unit) -> unit
val render : string -> Backend.t -> unit
