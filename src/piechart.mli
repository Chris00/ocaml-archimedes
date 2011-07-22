
type style =
| Flat          (** A simple circle separated in regions *)
| Separated     (** The regions are separated by a gap *)
| HighlightFlat (** One of the regions is separated by a gap, and so
                    highlighted toward the others *)
| Relief        (** A simple 3D pie *)

type colorscheme =
| Default    (** A set of colors that should render good for any data *)
| Monochrome (** A gradient from black to white *)
| None       (** No colors at all, the Overpie or Outer keyscheme should then
                 be used *)
| CustomColors of (string * Color.t) list
(** A color scheme associating a custom color to each data *)
| ValueDependant of (float -> Color.t)
(** Sometimes it is desirable to use color to express something on
    the piechart depending on the data values, this color scheme
    permits it *)
| LevelValueDependant of (int -> int -> float -> Color.t -> float -> Color.t)
  (** For multi-levels pie charts, the color may depend of the
      level, the position (in the parent children's list), the
      parent data value/color and the actual value of the data to
      draw. The level 0 is the first level with at least two
      elements *)

type keyplacement =
| Rectangle (** A rectangle containing the color followed by the label
                of each data *)
| OverPie   (** The labels are drawn directly over the data *)
| Outer     (** The labels are drawn around the pie, next to the data they
                point out *)

type keylabels =
| Key          (** Just the name of the data *)
| WithValues   (** The name followed by the value between parentheses *)
| WithProcents (** The name followed by the procent among all data between
                   parentheses *)
| CustomLabels of (string -> float -> float -> string)
(** A custom label made of the name, the value and the percentage. *)

val simple : ?style:style -> ?colorscheme:colorscheme ->
  ?keyplacement:keyplacement -> ?keylabels:keylabels ->
  ?x0:float -> ?y0:float -> ?xend:float -> ?yend:float ->
  Viewport.t -> (string * float) list -> unit
(** [simple vp data] draws a pie chart on [vp].

    @param style the style, default is Relief

    @param colorscheme the color scheme, default is Default

    @param keyplacement where to place the key, default is Rectangle

    @param keylabels what are the labels, default is WithValues

    @param x0 the x-coordinate of the center (default: [0.]).

    @param y0 the y-coordinate of the center (default: [0.]).

    @param xend (default: [1.]).

    @param yend these parameters delimits the area of the pie chart
    over the viewport. They are given in Graph coordinates (i.e. from
    0 to 1). By default, some space is left on the top for a title for
    the pie chart *)

type multidata = {
  name: string;
  value: float;
  children: multidata list
}

val multilevel : ?style:style -> ?colorscheme:colorscheme ->
  ?keyplacement:keyplacement -> ?keylabels:keylabels ->
  ?x0:float -> ?y0:float -> ?xend:float -> ?yend:float ->
  Viewport.t -> multidata list -> unit
(** [multilevel vp data] draws a multilevel pie chart on [vp]. The
    default options are tuned for a multilevel pie chart

    @param style default is flat (better visualisation because there
    is usualy lots of data)

    @param colorscheme default is LevelValueDependant, colors of the
    first level to contain more than one data (= level 0) are chosen
    in the "Default" way, children colors are derived from their
    parent color and their value. Inner levels (those who contains
    only one data) are filled with blank

    @param keyplacement default is OverPie, this is usually the better
    way to visualize data over a multilevel pie chart

    @param keylabels default is Key, because the color scheme gives an
    idea of the values, it is preferable to save space by hiding the
    values / percentages of the data

    @param x0 the x-coordinate of the center (default: [0.]).

    @param y0 the y-coordinate of the center (default: [0.]).

    @param xend (default: [1.]).

    @param yend by default, space is left for the title, as for the
    simple pie charts *)

