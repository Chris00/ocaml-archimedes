(** Module handling point styles and marks. *)
module type T = sig
  exception Error of string
    (**Raised for undefined point styles.*)

  type name = string
    (** Point styles are identified by strings. *)

  val add : name:name -> (Backend.t -> unit) -> Matrix.rectangle -> unit
    (**[add name f extents] adds to the existing point styles, a new
       point style, referenced under the name [name]. This point style is
       made using the function [f]; the extents it takes is given by
       [extents]. The behaviour of adding a new point style whose name is
       already used by another is the same as the core [Map.S.add] (that
       is, the previous binding disappears).*)

  val names : unit -> name list
    (** returns a list of all names declared *)
end
