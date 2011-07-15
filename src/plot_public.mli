(** Plotting various datatypes. *)
module type T = sig

  type pathstyle =
    | Lines
    | Points of string
    | Linespoints of string
    | Impulses
    | Boxes of float
    | Interval of float

  val x : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    ?base:Iterator.t -> Viewport.t -> Iterator.t -> unit

  val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    Viewport.t -> Iterator.t -> unit

  val stack : ?colors:(Color.t array) -> ?fillcolor:(Color.t array) ->
    Viewport.t -> Iterator.t array -> unit

  module Function : sig
    type sampling

    val xsampling : ?strategy:Sampler.strategy ->
      ?criterion:Sampler.criterion -> ?min_step:float -> ?nsamples:int ->
      (float -> float) -> float -> float -> sampling

    val xysampling : ?strategy:Sampler.strategy ->
      ?criterion:Sampler.criterion -> ?min_step:float -> ?nsamples:int ->
      (float -> float * float) -> float -> float -> sampling

    val x : ?pathstyle:pathstyle -> Viewport.t -> sampling -> unit

    val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
      Viewport.t -> sampling -> unit

    val fill : ?fillcolor:Color.t -> Viewport.t -> ?base:sampling ->
      sampling -> unit
  end

  module Array : sig
    type data = float array
    type data2 = (float * float) array

    val x : ?base:data -> ?fill:bool -> ?fillcolor:Color.t ->
      ?pathstyle:pathstyle -> Viewport.t -> data -> unit

    val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
      Viewport.t -> data2 -> unit

    val stack : ?colors:(Color.t array) -> ?fillcolor:(Color.t array) ->
      Viewport.t -> data array -> unit
  end

  module List : sig
    type data = float list
    type data2 = (float * float) list

    val x : ?base:data -> ?fill:bool -> ?fillcolor:Color.t ->
      ?pathstyle:pathstyle -> Viewport.t -> data -> unit

    val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
      Viewport.t -> data2 -> unit

    val stack : ?colors:(Color.t array) -> ?fillcolor:(Color.t array) ->
      Viewport.t -> data array -> unit
  end

  module Fortran : sig
    open Bigarray

    type data = (float, float64_elt, fortran_layout) Array1.t
    type data2 = (float, float64_elt, fortran_layout) Array2.t

    val x : ?base:data -> ?fill:bool -> ?fillcolor:Color.t ->
      ?pathstyle:pathstyle -> Viewport.t -> data -> unit

    val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
      Viewport.t -> data2 -> unit

    val stack : ?colors:(Color.t array) -> ?fillcolor:(Color.t array) ->
      Viewport.t -> data array -> unit
  end

  module C : sig
    open Bigarray

    type data = (float, float64_elt, c_layout) Array1.t
    type data2 = (float, float64_elt, c_layout) Array2.t

    val x : ?base:data -> ?fill:bool -> ?fillcolor:Color.t ->
      ?pathstyle:pathstyle -> Viewport.t -> data -> unit

    val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
      Viewport.t -> data2 -> unit

    val stack : ?colors:(Color.t array) -> ?fillcolor:(Color.t array) ->
      Viewport.t -> data array -> unit
  end
end
