exception Not_available
type axes =
    [ `None of bool * bool
    | `Rectangle of bool * bool
    | `Two_lines of float * float ]
val print_axes :
  [> axes] ->
  xmin:float -> xmax:float -> ymin:float -> ymax:float -> Backend.t -> unit
val axes_meeting :
  [> axes] ->
  xmin:float -> xmax:float -> ymin:float -> ymax:float -> float * float


type tic = [ `P of Pointstyle.name ]
val print_tic : Backend.t -> [> tic] -> unit
val tic_extents : [> tic] -> Backend.rectangle

type label = {
  action : Backend.t -> unit;
  box : Backend.rectangle;
  rotation : float;
}
val tic_label_extents : Backend.rectangle -> label option -> Backend.rectangle


type position =
     float -> float -> float -> float -> (float*float*label option) list
  (**Given the bounds on which the axis applies, returns where the tics
     should be and what to do there.*)
  (*
    type data = [`Text_labels of string list]
  *)

type loc_tics =
    [ `Fixed_pos of (float * label option) list
    | `Fixed_numbers of int list * label list]
(**Convenient ways to specify where we want tics.*)

val get_labels : [>loc_tics] -> position
(**From the 'convenience' to the manipulated data.*)

type 'a axis
(**This type stores all information about an axis.*)
type ('a, 'b) t
(**This type stores information about a pair of axes.*)

val make_axis :
  ([> tic] as 'a) -> 'a ->
  ?get_labels:(([>loc_tics] as 'b) -> position) -> 'b -> 'a axis

(**[make_axis major minor loc] makes an axis whose major tics will be
[major], minor tics [minor], positioned using [loc] and the optional
[get_labels].*)

val make : ([>axes] as 'a) -> 'b axis -> 'b axis -> ('a, 'b) t
(**Given two axes and a way to render them, makes a [t].*)


val get_margins :
  ([> `None of bool * bool
    | `Rectangle of bool * bool
    | `Two_lines of float * float ]
   as 'a, [> `P of Pointstyle.name ] as 'b)
  t ->
  ?axes_meeting:('a ->
                 xmin:float ->
                 xmax:float -> ymin:float -> ymax:float -> float * float) ->
  ?tic_extents:('b -> Backend.rectangle) ->
  float -> float -> float -> float -> Backend.rectangle * Backend.rectangle
  (**Returns the margins needed to print the axes. returns a pair of
     [Backend.rectangle], the first one represents the extents for the X
     axis, and the second one the extents for the Y axis.*)
val print :
  ([> axes] as 'a, [> tic] as 'b) t ->
  lines:Coordinate.t ->
  xmin:float ->
  xmax:float ->
  ymin:float ->
  ymax:float ->
  ?print_axes:('a ->
                 xmin:float ->
                xmax:float -> ymin:float -> ymax:float -> Backend.t -> unit) ->
  ?axes_meeting:('a ->
                   xmin:float ->
                  xmax:float -> ymin:float -> ymax:float -> float * float) ->
  ?print_tic:(Backend.t -> 'b -> unit) -> Backend.t -> unit

(**Prints axes, following the parameters stored in [t] and the
   optional arguments, if given.*)


