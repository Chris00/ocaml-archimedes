(** {3 Plotting various datatypes} *)

(** Style of various plots.  Plotting functions only support the
    subset of these style that make sense for them.

    - [`Lines] Data points are joined by a simple line.
    - [`Markers] Data points are marked with the mark type given in
    argument of the Points constructor.
    - [`Linesmarkers] Data points are joined by a line and marked with
    the mark type given in argument.
    - [`Impulses] Data points are "hit" by lines starting from zero.
    - [`Bars w] Data points determine the height of a box of width [w]
    which must be given in [Data] coordinates (from 0 to 1).
    - [`HBars h] Data points determine the width of an horizontal box
    of height [h] which must be given in [Data] coordinates (from 0 to 1).

    For the list of default marks for [`Markers] and [`Linesmarkers]
    have a look to {!Marker.names}.  You can also define your own
    with {!Marker.add}.
*)
type style =
[ `Lines
| `Markers of string
| `Linesmarkers of string
| `Impulses
| `Bars of float
| `HBars of float ]

(** Plotting functions. *)
val fx : Viewport.t -> ?tlog:bool -> ?fn0:float -> ?n:int ->
  ?strategy:Sampler.strategy -> ?cost:Sampler.cost ->
  ?style:[`Lines | `Linesmarkers of string | `Markers of string ] ->
  ?base:(float -> float) -> ?fill:bool -> ?fillcolor:Color.t ->
  (float -> float) -> float -> float -> unit
(** [fx vp f a b] draws the graph of the function [f] on the interval
    [[a, b]].

    @param style the style of the plot.  Default: [`Lines].
    @param fill whether to fill the region between the graph of [f]
    and the base.  Default: [false].
    @param fillcolor the color for filling.  Default: {!Color.white_smoke}.
    @param base the second function for delimiting the filling
    region.  Default: the identically zero function.

    @param fn0 the fraction of the maximum number of function
    evaluations used to create an initial sampling of the function
    that will later be refined by the adaptive autosampling. Default: [0.1].
    @param n the maximum number of function evaluations.  Default: [100].
    @param strategy see {!Sampler.strategy}.
    @param cost see {!Sampler.cost}. *)

val xyf : Viewport.t -> ?tlog:bool -> ?fn0:float -> ?n:int ->
  ?strategy:Sampler.strategy -> ?cost:Sampler.cost ->
  ?style:[`Lines | `Linesmarkers of string | `Markers of string ] ->
  ?fill:bool -> ?fillcolor:Color.t ->
  (float -> float * float) -> float -> float -> unit
(** [xyf vp f a b] draws the image of the function [f] on the interval
    [[a, b]], that is the set of points (x,y) = [f](t) for t in [[a,b]].

    The optional arguments are the same as for {!fx}. *)


(** Plotting float Arrays. *)
module Array : sig
  val y : Viewport.t -> ?const_base:bool -> ?base:float array ->
    ?fill:bool -> ?fillcolor:Color.t -> ?style:style ->
    ?const_y:bool -> float array -> unit
  (** [y vp yvec] draws the set of points [(i, yvec.(i))].

      @param style the style used for the plot.  The default style is
      [`Marker "O"] which means data points are marked by a small disk.
      See {!Archimedes.style} for a full list.

      @param fill whether to fill the surface between the base and the
      values [yval].
      @param fillcolor the filling color (default: {!Color.white_smoke}).
      @param const_y whether the input vector [yvec] will not be modified
      anymore (so there is no need to cache its current values).

      @param base for the styles [`Lines], [`Markers], and
      [`Linesmarkers], it gives the bottom of the filling zone.  For
      the styles [`Impulses] and [`Bars w], it is the Y value above
      which the boxes (of heights given by [yvec]) are drawn.  For the
      style [`HBars], it is the (signed) distance to the Y axis at
      which the horizontal bar starts.
      @param const_base same as [const_y] for the base. *)

  val xy: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t -> ?style:style ->
    ?const_x:bool -> float array -> ?const_y:bool -> float array -> unit
  (** [xy cp xvec yvec] draws the set of points [(xvec.(i), yvec.(i))].
      The optional arguments are similar to {!Array.y}.

      @raise Invalid_argument if [xvec] and [yvec] do not have the same
      length.

      See {!Array.y} for the meaning of optional arguments. *)

  val xy_pairs: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:[`Lines | `Markers of string | `Linesmarkers of string ] ->
    (float * float) array -> unit
  (** See {!Array.xy}.  The only difference is that this function
      takes an array of couples (x,y) instead of two arrays, one for x
      and a second of y. *)

  val stack : Viewport.t ->
    ?fill:bool -> ?fillcolors:Color.t array -> ?style:style ->
    ?const:bool -> float array array -> unit
  (** [stack yvecs] plot the data in a stacked fashion, the Y values
      contained in [yvecs.(i)] are represented as the deviation above
      [yvecs.(i-1)].  This makes sense only if the data is non-negative.

      @param style how to represent each data point.  Default [`Bars 0.5].

      @param colors the colors for the data lines.

      @param fill whether to fill the boxes or area under the data
      points.  Default: [true].

      @param fillcolors the [i]th color is used to fill the area under
      the data points [yvecs.(i)].  If the array is empty, a default
      palette is used.  If there are less colors than vectors in
      [yvecs], they are used in a circular way.

      @param const_y whether the input vector [yvec] will not be modified
      anymore (so there is no need to cache its current values). *)
  ;;
end

(** Plotting Lists of floats. *)
module List : sig
  val y : Viewport.t -> ?base:float list -> ?fill:bool ->
    ?fillcolor:Color.t -> ?style:style -> float list -> unit
  (** See {!Array.y}.  *)

  val xy: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:[`Lines | `Markers of string | `Linesmarkers of string ] ->
    float list -> float list -> unit
  (** See {!Array.xy}.  The number of elements plotted the the minimum
      of the lengths of the two lists. *)

  val xy_pairs: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:[`Lines | `Markers of string | `Linesmarkers of string ] ->
    (float * float) list -> unit
  (** See {!Array.xy_pairs}.  *)
end

(** Plotting Fortran bigarrays. *)
module Vec : sig
  open Bigarray
  type t = (float, float64_elt, fortran_layout) Array1.t

  val y : Viewport.t -> ?const_base:bool -> ?base:t -> ?fill:bool ->
    ?fillcolor:Color.t -> ?style:style ->
    ?const_y:bool -> t -> unit
  (** See {!Array.y}.  *)

  val xy: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t -> ?style:style ->
    ?const_x:bool -> t -> ?const_y:bool -> t -> unit
  (** See {!Array.xy}.  *)

  val stack : Viewport.t ->
    ?fill:bool -> ?fillcolors:Color.t array -> ?style:style ->
    ?const:bool -> t array -> unit
  (** See {!Array.stack}.  *)
end

(** Plotting C bigarrays. *)
module CVec : sig
  open Bigarray
  type t = (float, float64_elt, c_layout) Array1.t

  val y : Viewport.t -> ?const_base:bool -> ?base:t -> ?fill:bool ->
    ?fillcolor:Color.t -> ?style:style ->
    ?const_y:bool -> t -> unit
  (** See {!Array.y}.  *)

  val xy: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t -> ?style:style ->
    ?const_x:bool -> t -> ?const_y:bool -> t -> unit
  (** See {!Array.xy}.  *)

  val stack : Viewport.t ->
    ?fill:bool -> ?fillcolors:Color.t array -> ?style:style ->
    ?const:bool -> t array -> unit
  (** See {!Array.stack}.  *)
end


(*----------------------------------------------------------------------*)
(** {3 Plotting generic data} *)

val y : Viewport.t -> ?base:((float -> unit) -> unit) ->
  ?fill:bool -> ?fillcolor:Color.t -> ?style:style ->
  ((float -> unit) -> unit) -> unit
(** [y vp iter] draws on [vp] the values provided by the iterator [iter].
    See {!Array.y} for more information. *)

val xy : Viewport.t -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:[`Lines | `Markers of string | `Linesmarkers of string ] ->
  ((float -> float -> unit) -> unit) -> unit
(** [xy vp iter] plots on [vp] the values provided by the iterator
    [iter].
    See {!Array.xy} for more information. *)
