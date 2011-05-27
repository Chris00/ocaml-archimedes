(** Plotting various datatypes. *)
module type T = sig

  (************************************************************************)
  (** Functions common to all datatype submodules. *)
  module type Common = sig
    type pathstyle =
      | Lines
      | Points of string
      | Linespoints of string
      | Impulses
      | Boxes of float
      | Interval of float

    type filledcurves = Color.t * Color.t (* f1 > f2, f2 < f1 *)

    val fx : ?strategy:Sampler.strategy -> ?criterion:Sampler.criterion ->
      ?min_step:float -> ?nsamples:int ->
      ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
      ?g:(float -> float) -> Viewport.t -> (float -> float) ->
      float -> float -> unit

    val xy_param : ?min_step:float -> ?nsamples:int -> ?fill:bool ->
      ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
      Viewport.t -> (float -> float * float) -> float -> float -> unit

    (* TODO we want to have control over the stroke properties for each curve *)
  end

  module Array : sig
    include Common

    val x : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
      Viewport.t -> float array -> unit

    val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
      Viewport.t -> float array -> float array -> unit

    val stack : ?color:(Color.t array) -> ?fillcolor:(Color.t array) ->
      ?pathstyle:pathstyle -> Viewport.t -> float array array -> unit

  end

(*  module List : sig
    include COMMON

    val x : t -> ?color: Color.t -> ?mark:string -> ?n0:int ->
      float list -> unit

    val xy : t -> ?color: Color.t -> ?mark:string ->
      float list -> float list -> unit
  end

  module Fortran : sig
    include COMMON
    open Bigarray
    type vec = (float, float64_elt, fortran_layout) Bigarray.Array1.t

    val x : t -> ?color: Color.t -> ?mark:string -> ?n0:int ->
      vec -> unit

    val xy : t -> ?color: Color.t -> ?mark:string ->
      vec -> vec -> unit
  end

  module C : sig
    include COMMON
    open Bigarray
    type vec = (float, float64_elt, c_layout) Bigarray.Array1.t

    val x : t -> ?color: Color.t -> ?mark:string -> ?n0:int ->
      vec -> unit

    val xy : t -> ?color: Color.t -> ?mark:string ->
      vec -> vec -> unit
  end

  module Generic : sig
    include COMMON

    val x : t -> ?color: Color.t -> ?mark:string -> ?n0:int ->
      ((float -> unit) -> 'a -> unit) -> 'a -> unit

    val xy : t -> ?color: Color.t -> ?mark:string ->
      ((float -> float -> unit) -> 'a -> unit) -> 'a -> unit
  end*)
end
