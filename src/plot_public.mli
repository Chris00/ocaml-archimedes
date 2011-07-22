(** Plotting various datatypes. *)
module type T = sig

  type pathstyle =
    | Lines
        (** Data points are joined by a simple line *)
    | Points of string
        (** Data points are marked with the mark type given in argument of
            the Points constructor *)
    | Linespoints of string
        (** Data points are joined by a line and marked with the mark type
            given in argument *)
    | Impulses
        (** Data points are "hit" by lines starting from zero *)
    | Boxes of float
        (** Data points are the top of a box of custom width (from 0 to 1) *)
    | Interval of float
        (** Data points are represented with a line from a base
            point. That line is delimited by two small orthogonal lines *)

  val x : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    ?base:Iterator.t -> Viewport.t -> Iterator.t -> unit
    (** [x vp iter] Plots the values of [iter] on [vp] according to the
        fact that the x-values of iter are 0, 1, 2, etc. or 1, 2, 3,
        etc. or 42, 43, 44, etc. or ...

        @param fill fill the region between the iterator and its base ?
        (default: false)

        @param fillcolor which color to use for the fill

        @param pathstyle which pathstyle to use (see pathstyle type)

        @param base the base iterator is the other delimiter of the region
        to fill. Default: the zero iterator (giving (0, 0), (1, 0), (2,
        0), etc.) *)

  val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    Viewport.t -> Iterator.t -> unit
    (** [xy vp iter] Plots the values of [iter] on [vp] with no
        constraints over the values of iter.

        @param fill fill the region delimited by the curve ? (default:
        false)

        @param fillcolor which color to use for the fill

        @param pathstyle which pathstyle to use (see pathstyle type) *)

  val stack : ?colors:(Color.t array) -> ?fillcolors:(Color.t array) ->
    ?pathstyle:pathstyle -> Viewport.t -> Iterator.t array -> unit
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

        @param pathstyle which pathstyle to use (see pathstyle type,
        default is [Boxes 0.5]) *)

  module Function : sig
    type 'a sampling
      (** A sampling, the only interresting values for 'a are float and
          float * float *)

    val sampling : ?tlog:bool -> ?strategy:Sampler.strategy ->
      ?criterion:Sampler.criterion -> ?min_step:float -> ?nsamples:int ->
      (float -> 'a) -> float -> float -> 'a sampling
      (** [sampling f a b] Creates a sampling for the function [f] between
          [a] and [b], see Sampler for more explanations over the
          optional arguments *)

    val x : ?pathstyle:pathstyle -> ?base:(float -> float) -> Viewport.t -> float sampling -> unit
      (** [x vp sampling] Plots [sampling] on [vp], the sampling needs to
          be a function sampling, and not a curve sampling

          @param pathstyle which pathstyle to use (see pathstyle type) *)

    val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
      Viewport.t -> (float * float) sampling -> unit
      (** [xy vp sampling] Plots [sampling] on [vp], the sampling needs to
          be a curve sampling

          @fill fill the curve delimited by the function ? (default: false)

          @fillcolor color to use for the filling

          @param pathstyle which pathstyle to use (see pathstyle type) *)

    val fill : ?fillcolor:Color.t -> ?base:(float sampling) -> Viewport.t ->
      float sampling -> unit
      (** [fill vp sampling] Fills the region between two function samplings

          @param fillcolor the color to use for the filling

          @param base the other sampling used for the filling. The fill
          function will handle non concordant domains and samplings that
          "cross over" one another *)
  end

  module type Common = sig
    (** The Common module type is used by all the "standard" plot modules
        (Lists, Arrays, Bigarrays) *)

    type data
      (** The function type, e.g. float list *)
    type data2
      (** The curve type, e.g. (float * float) list *)

    val x : ?base:data -> ?fill:bool -> ?fillcolor:Color.t ->
      ?pathstyle:pathstyle -> Viewport.t -> data -> unit
      (** Same as the x function of the Plot module, but instead of
          applying to iterators, it applies to a particular data structure
          determined by the submodule which is used (Plot.Array,
          Plot.List, Plot.Fortran or Plot.C) *)

    val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
      Viewport.t -> data2 -> unit
      (** Same as the xy function of the Plot module, but instead of
          applying to iterators, it applies to a particular data structure
          determined by the submodule which is used (Plot.Array,
          Plot.List, Plot.Fortran or Plot.C) *)

    val stack : ?colors:(Color.t array) -> ?fillcolors:(Color.t array) ->
      ?pathstyle:pathstyle -> Viewport.t -> data array -> unit
      (** Same as the stack function of the Plot module, but instead of
          applying to iterators, it applies to a particular data structure
          determined by the submodule which is used (Plot.Array,
          Plot.List, Plot.Fortran or Plot.C) *)
  end

  module Array : sig
    include Common with type
      data = float array and type
      data2 = (float * float) array
  end

  module List : sig
    include Common with type
      data = float list and type
      data2 = (float * float) list
  end

  module Fortran : sig
    open Bigarray

    include Common with type
      data = (float, float64_elt, fortran_layout) Array1.t and type
      data2 = (float, float64_elt, fortran_layout) Array2.t
  end

  module C : sig
    open Bigarray

    include Common with type
      data = (float, float64_elt, c_layout) Array1.t and type
      data2 = (float, float64_elt, c_layout) Array2.t
  end
end
