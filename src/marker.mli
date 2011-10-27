(** Module handling point styles and marks. *)

exception Error of string
  (**Raised for undefined point styles.*)

type name = string
(** Point styles are identified by strings. *)

val add : name:name -> (Backend.t -> unit) -> Matrix.rectangle -> unit
(** [add name f extents] adds to the existing point styles, a new
    point style, referenced under the name [name]. This point style
    is made using the function [f]; the extents it takes is given by
    [extents]. The behaviour of adding a new point style whose name
    is already used by another is the same as the core [Map.S.add]
    (that is, the previous binding disappears).*)

val names : unit -> name list
(** @return a list of all names currently declared.

    By default, the following marks are defined (a short explanation
    is given if the mark is not clear from the string):
    - ["x"], ["-"], ["|"], ["+"], ["*"],
    - ["o"] (a circle), ["O"] (a disk, i.e. same as ["o"] but filled),
    - ["s"] (a square), ["S"] (a filled square),
    - ["d"] (a diamond), ["D"] (a filled diamond),
    - ["^"] (an inverted V),  ["v"], [">"], ["<"],
    - ["^-"] (a triangle pointing upward), ["v-"], ["|>"], ["<|"],
    - ["^--"] (a filled triangle pointing upward), ["v--"], ["||>"], ["<||"],
    - ["p"] (a pentagon), ["P"] (a filled pentagon),
    - ["h"] (an hexagon), ["H"] (a filled hexagon),
    - ["tic_up"] (a small bar above the current location),
      ["tic_down"], ["tic_left"], ["tic_right"].
*)

(**/**)

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
