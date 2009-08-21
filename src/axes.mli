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

type label1 =
{action:B.t -> unit;
 (**Action to perform as a label.*)
 box:B.t -> Backend.rectangle;
 (**Place needed to do it; it is the smallest rectangle containing the action.*)
}

type label = {label:float -> float -> label1; rotation:float}

val tic_label_extents : Backend.rectangle -> label option ->
  float -> float -> Backend.t -> Backend.rectangle

type data =
    [ `Label of label
    | `Text_label of string
    | `Abscissa
    | `Ordinate
    | `Expabscissa
    | `Expordinate ]

val get_label: [> data] -> label

type position =
     float -> float -> float -> float -> (float*float*label option) list
  (**Given the bounds on which the axis applies, returns where the tics
     should be and what to do there.*)
  (*
    type data = [`Text_labels of string list]
  *)

type loc_tics =
    [ `Fixed_pos of (float * label option) list
        (**List of pairs [(x, label)] with [x] a number between 0 and
           1, specifying the relative position of the tic and [label] the
           optional label to make at this place (it is [None] if and only
           if the tic we want is a minor tic).*)
    | `Fixed_numbers of int list * label list
        (**A list which specifies the number of minor tics between two
           consecutive major tics; another list which gives which label
           to apply on major tics. The second list has to be of length
           strictly greater (of 1) than the first one (if it is not, no
           labels will be applied on last major tics).*)
    | `Regular of int * label array]
      (**Fixed number of minor tics between two consecutive major
         tics, and labels associated to these major tics. The number
         of major tics will be precisely the length of the labels
         array.*)

(**Convenient ways to specify where we want tics.*)

val get_position : [>loc_tics] -> position
(**From the 'convenience' to the manipulated data.*)

type 'a axis
(**This type stores all information about an axis.*)
type ('a, 'b) t
(**This type stores information about a pair of axes.*)

val make_axis :
  ([> tic] as 'a) -> 'a ->
  ?get_position:(([>loc_tics] as 'b) -> position) -> 'b -> 'a axis

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
  float -> float -> float -> float ->
  Backend.t -> Backend.rectangle * Backend.rectangle
  (**Returns the margins needed to print the axes. Returns a pair of
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


