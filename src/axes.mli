type axes = [ `Rectangle of bool * bool | `Two_lines of float * float ]
(**Type for printing axes.*)

type tic = [`P of Pointstyle.name]
(**Type indicating a tic mode.*)

type loc_tics =
    [ `Linear
    | `Logarithmic
    | `Manual of int -> float]

(**Type which defines localization of tics.*)

type mode_tics =
    Automatic(**Find the number of major tics so that labels are
                espaced by the max width/height of the
                extrema.*)
  | Semi_automatic of float(**Find the number of major tics so that
                              labels are spaced by the given ratio*)
  | Fixed of int * int (**Number of major tics and minor tics are given.*)
(**Type determining how the tics will be made (number of major, minor tics).*)

type data =
    [ `Numbers
    | `Other of string list
    | `Iter of int -> string ]
(**Data to be printed on major tics.*)


type ('a, 'b, 'c) axis
(**The type storing all information of an axis.*)
type ('a, 'b, 'c, 'd) t
  (**Main type. It stores two axes and a way to print it (see type [axes]).*)

exception Not_available
  (**Raised if some function is employed but does not define
     operations for some used variant. More precisely, if you define a
     new variant, but use a 'default' function on it, then this
     exception is raised.*)

val make_axis :
  ([> `P of Pointstyle.name] as 'a) -> 'a ->
  ([> `Linear | `Logarithmic | `Manual of int -> float] as 'b) ->
  mode_tics ->
  ([> `Numbers | `Other of string list | `Iter of int -> string] as 'c) ->
  ('a, 'b, 'c) axis
  (**Makes an axis, given a way to make tics, their positionment, their
     number and the text to apply near them.*)

val make :
  ([> `Rectangle of bool * bool | `Two_lines of float * float ] as 'a) ->
  ('b, 'c, 'd) axis -> ('b, 'c, 'd) axis -> ('a, 'b, 'c, 'd) t
  (**Given axes and how to stroke them, makes a [t].*)

val print_axes :
  [> `Rectangle of bool * bool | `Two_lines of float * float ] ->
  xmin:float ->
  xmax:float -> ymin:float -> ymax:float -> Coord_handler.t -> float * float
  (**Function used to print axes. It returns the point where the axes meet.*)


val print_tic : Coord_handler.t ->
  [> `P of Pointstyle.name ] -> Backend.rectangle
  (**This function is used to tell how to print a tic. It returns the
     smallest rectangle containing the tic (cf. [Pointstyle.extents]).*)

val get_funct :
  [> `Linear | `Logarithmic | `Manual of int -> float] ->
  int -> int -> int -> float
  (**[get_funct loc] returns a function whose purpose is to tell where
     a tic is needed. More precisely, the arguments of this function
     are, in order, [majors minors num], where [majors] is the number
     of major tics (including the first and the last one), [minors]
     the number of minor tics between two major tics, and [num] the
     number of the tic considered. The function then returns a number
     between [0.] and [1.] which gives the relative position of the
     tic.*)

val get_labels :
  [> `Numbers | `Other of string list | `Iter of int -> string] -> int -> string
  (**Returns an iterator to make labels on an axis.*)

val print :
  ([> `Rectangle of bool * bool | `Two_lines of float * float ] as 'a,
   [> `P of Pointstyle.name] as 'b,
   [> `Linear | `Logarithmic | `Manual of int -> float] as 'c,
   [> `Numbers | `Other of string list | `Iter of int -> string] as 'd) t ->
  xmin:float -> xmax:float -> ymin:float -> ymax:float ->
  ?print_axes:('a ->
                 xmin:float -> xmax:float -> ymin:float -> ymax:float ->
                Coord_handler.t -> float * float) ->
  ?print_tic:(Coord_handler.t -> 'b -> Backend.rectangle) ->
  ?get_funct:('c -> int -> int -> int -> float) ->
  ?get_labels:('d -> int -> string) ->
  Coord_handler.t -> unit

(**Print axes following the parameters stored in t and according to
the (optional) functions of rendering. The X axis will be made in the
interval [xmin,xmax] and the Y axis in the interval [ymin, ymax]; they
can be printed further (according to the [print_axes] function), but
the labels concern only these intervals.*)
