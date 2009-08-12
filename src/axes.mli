type axes = [ `Rectangle of bool * bool | `Two_lines of float * float ]
(**Type for printing axes.*)
type tic = [`P of Pointstyle.name]
(**Type indicating a tic mode.*)
type loc_tics = [ `Linear | `Logarithmic ]
(**Type which defines localization of tics.*)
type mode_tics = Automatic | Fixed of int * int
(**Type determining how the tics will be made (number of major, minor tics).*)
type data = [ `Numbers | `Other of string list ]
(**Data to be printed on major tics.*)
type ('a, 'b, 'c) axis
(**The type storing all information of an axis.*)
type ('a, 'b, 'c, 'd) t
(**Main type. It stores two axes and a way to print it (see type [axes]).*)

val make_axis :
  ([> `P of Pointstyle.name] as 'a) -> 'a ->
  ([> `Linear | `Logarithmic ] as 'b) ->
  mode_tics ->
  ([> `Numbers | `Other of string list ] as 'c) -> ('a, 'b, 'c) axis
(**Makes an axis, given a way to make tics, their positionment, their
number and the text to apply near them.*)

val make :
 ([> `Rectangle of bool * bool | `Two_lines of float * float ] as 'a) ->
  ('b, 'c, 'd) axis -> ('b, 'c, 'd) axis -> ('a, 'b, 'c, 'd) t

val print_tic : Coord_handler.t -> [< `P of Pointstyle.name ] -> unit

val get_funct :
  [< `Linear | `Logarithmic ] -> int -> int -> int -> float * bool

val get_text :
  [< `Linear | `Logarithmic ] ->
  [< `Numbers | `Other of string list ] ->
  float -> float -> int -> int -> string

val print :
  ([< `Rectangle of bool * bool | `Two_lines of float * float ],
   [< `P of Pointstyle.name],
   [< `Linear | `Logarithmic ],
   [< `Numbers | `Other of string list ])
  t ->
  xmin:float -> xmax:float -> ymin:float -> ymax:float -> Coord_handler.t -> unit
