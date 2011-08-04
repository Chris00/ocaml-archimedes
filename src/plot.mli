(** Plotting various datatypes. *)

(** Style of various plots.  Plotting functions only support the
    subset of these style that make sense for them.

    - [`Lines] Data points are joined by a simple line.
    - [`Points] Data points are marked with the mark type given in
    argument of the Points constructor.
    - [`Linespoints] Data points are joined by a line and marked with
    the mark type given in argument.
    - [`Impulses] Data points are "hit" by lines starting from zero.
    - [`Boxes w] Data points are the top of a box of custom width [w]
    which must be given in [Data] coordinates (from 0 to 1).
*)

type style =
[ `Lines
| `Points of string
| `Linespoints of string
| `Impulses
| `Boxes of float ]

(** Plotting functions. *)
module Function : sig
  val x : ?tlog:bool -> ?n:int ->
    ?strategy:Sampler.strategy -> ?cost:Sampler.cost ->
    ?style:[`Lines | `Linespoints of string | `Points of string ] ->
    ?base:(float -> float) -> ?fill:bool -> ?fillcolor:Color.t ->
    Viewport.t -> (float -> float) -> float -> float -> unit

  val xy : ?tlog:bool -> ?n:int ->
    ?strategy:Sampler.strategy -> ?cost:Sampler.cost ->
    ?style:[`Lines | `Linespoints of string | `Points of string ] ->
    ?fill:bool -> ?fillcolor:Color.t ->
    Viewport.t -> (float -> float * float) -> float -> float -> unit

end

module Array : sig
  val y : Viewport.t -> ?base:float array -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:style ->
    ?const:bool -> float array -> unit
  (** [y vp yvec] draws the set of points (i, yvec.(i)).

      @param style the style used for the plot.  The default style is
      [`Points "O"] which means data points are marked by a small disk.

      @fill whether to fill the surface between the base and the
      values [yval].
      @fillcolor the filling color (default: {!Color.white_smoke}).
      @const whether the input vector [yvec] will not be modified
      anymore (so there is no need to cache its current values).

      @param base for the styles [`Lines], [`Points], and
      [`Linespoints], it gives the bottom of the filling zone.  For
      the styles [`Impulses] and [`Boxes w], it is the Y value above
      which the boxes (of heights given by [yvec]) are drawn. *)

  val xy: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:[`Lines | `Points of string | `Linespoints of string ] ->
    ?const_x:bool -> float array -> ?const_y:bool -> float array -> unit
  (** [xy cp xvec yvec] draws the set of points (i, yvec.(i)).
      The optional arguments are similar to {!Array.y}.

      @raise Invalid_argument if [xvec] and [yvec] do not have the same
      length.*)

  val stack : Viewport.t -> ?colors:Color.t array ->
    ?fill:bool -> ?fillcolors:Color.t array -> ?style:style ->
    float array array -> unit
end

module type Common = sig
  (** The Common module type is used by all the "standard" plot modules
      (Lists, Arrays, Bigarrays) *)

  type data
  (** The function type, e.g. float list *)
  type data2
  (** The curve type, e.g. (float * float) list *)

  val y : ?base:data -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:[`Lines|`Points of string|`Linespoints of string
           |`Impulses|`Boxes of float] ->
    Viewport.t -> data -> unit
  (** Same as the x function of the Plot module, but instead of
      applying to iterators, it applies to a particular data structure
      determined by the submodule which is used (Plot.Array,
      Plot.List, Plot.Fortran or Plot.C) *)

  val xy : ?fill:bool -> ?fillcolor:Color.t -> ?style:style ->
    Viewport.t -> data2 -> unit
  (** Same as the xy function of the Plot module, but instead of
      applying to iterators, it applies to a particular data structure
      determined by the submodule which is used (Plot.Array,
      Plot.List, Plot.Fortran or Plot.C) *)

  val stack : ?colors:(Color.t array) -> ?fillcolors:(Color.t array) ->
    ?style:style -> Viewport.t -> data array -> unit
  (** Same as the stack function of the Plot module, but instead of
      applying to iterators, it applies to a particular data structure
      determined by the submodule which is used (Plot.Array,
      Plot.List, Plot.Fortran or Plot.C) *)
end

module List : sig
  include Common
    with type data = float list
    and type data2 = (float * float) list
end

module Fortran : sig
  open Bigarray

  include Common
    with type data = (float, float64_elt, fortran_layout) Array1.t
    and type data2 = (float, float64_elt, fortran_layout) Array2.t
end

module C : sig
  open Bigarray

  include Common
    with type data = (float, float64_elt, c_layout) Array1.t
    and type data2 = (float, float64_elt, c_layout) Array2.t
end

(*----------------------------------------------------------------------*)
(** {3 Plotting functions for generic data} *)

(* val y : ?fill:bool -> ?fillcolor:Color.t -> ?style:style -> *)
(*   ?base:Iterator.t -> Viewport.t -> Iterator.t -> unit *)
(** [x vp iter] Plots the values of [iter] on [vp] according to the
    fact that the x-values of iter are 0, 1, 2, etc. or 1, 2, 3,
    etc. or 42, 43, 44, etc. or ...

    @param fill fill the region between the iterator and its base ?
    (default: false)

    @param fillcolor which color to use for the fill

    @param style which style to use (see the {!style} type).

    @param base the base iterator is the other delimiter of the region
    to fill. Default: the zero iterator (giving (0, 0), (1, 0), (2,
    0), etc.) *)

val xy : ?fill:bool -> ?fillcolor:Color.t -> ?style:style ->
  Viewport.t -> Iterator.t -> unit
(** [xy vp iter] Plots the values of [iter] on [vp] with no
    constraints over the values of iter.

    @param fill fill the region delimited by the curve ? (default:
    false)

    @param fillcolor which color to use for the fill

    @param style which style to use (see the {!style} type). *)

val stack : ?colors:(Color.t array) -> ?fillcolors:(Color.t array) ->
  ?style:style -> Viewport.t -> Iterator.t array -> unit
(** [stack vp iters] Stacks the iterators [iters] on [vp]; which means
    that the first iterator is plotted then used as the base for the
    second iterator, which is plotted and the sum of the two first
    iterators are used as the base for the third iterator,
    etc. Usually, the pathstyle used for a stack is Boxes, but one can
    use another pathstyle if he wants

    @param colors the colors to use for the iterators. If there are
    more iterators than colors available, a round-robin strategy is
    used to attribute colors

    @param fillcolors same as colors, but for filling colors

    @param style which style to use (see the {!style} type, default is
    [Boxes 0.5]).  *)
