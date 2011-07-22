(** Plotting various datatypes. *)

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

val stack : ?colors:(Color.t array) -> ?fillcolors:(Color.t array) ->
  ?pathstyle:pathstyle -> Viewport.t -> Iterator.t array -> unit

module Function : sig
  type 'a sampling

  val sampling : ?strategy:Sampler.strategy ->
    ?criterion:Sampler.criterion -> ?min_step:float -> ?nsamples:int ->
    (float -> 'a) -> float -> float -> 'a sampling

  val x : ?pathstyle:pathstyle -> Viewport.t -> float sampling -> unit

  val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    Viewport.t -> (float * float) sampling -> unit

  val fill : ?fillcolor:Color.t -> Viewport.t -> ?base:(float sampling) ->
    float sampling -> unit
end

module type Common = sig
  type data
  type data2

  val x : ?base:data -> ?fill:bool -> ?fillcolor:Color.t ->
    ?pathstyle:pathstyle -> Viewport.t -> data -> unit

  val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    Viewport.t -> data2 -> unit

  val stack : ?colors:(Color.t array) -> ?fillcolors:(Color.t array) ->
    ?pathstyle:pathstyle -> Viewport.t -> data array -> unit
end

module Array : sig
  include Common
  with type data = float array
  and type data2 = (float * float) array
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
