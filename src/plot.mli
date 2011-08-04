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

module List : sig
  val y : Viewport.t -> ?base:float list -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:style -> float list -> unit
  (** See {!Array.y}.  *)

  val xy: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:[`Lines | `Points of string | `Linespoints of string ] ->
    (float * float) list -> unit
  (** See {!Array.xy}.  *)
end


module Fortran : sig
  open Bigarray
  type vec = (float, float64_elt, fortran_layout) Array1.t

  val y : Viewport.t -> ?base:float array -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:style ->
    ?const:bool -> vec -> unit
  (** See {!Array.y}.  *)

  val xy: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:[`Lines | `Points of string | `Linespoints of string ] ->
    ?const_x:bool -> vec -> ?const_y:bool -> vec -> unit
  (** See {!Array.xy}.  *)
end

(* module C : sig
  open Bigarray
  type vec = (float, float64_elt, c_layout) Array1.t

  include Common
    with type data = (float, float64_elt, c_layout) Array1.t
    and type data2 = (float, float64_elt, c_layout) Array2.t
end *)


(*----------------------------------------------------------------------*)
(** {3 Plotting functions for generic data} *)

val y : Viewport.t -> ?base:((float -> unit) -> unit) ->
  ?fill:bool -> ?fillcolor:Color.t -> ?style:style ->
  ((float -> unit) -> unit) -> unit
(** [y vp iter] draws on [vp] the values provided by the iterator [iter].
    See {!Array.y} for more information. *)

val xy : Viewport.t -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:[`Lines | `Points of string | `Linespoints of string ] ->
  ((float -> float -> unit) -> unit) -> unit
(** [xy vp iter] plots on [vp] the values provided by the iterator
    [iter].
    See {!Array.xy} for more information. *)
