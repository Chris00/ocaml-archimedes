(** Plotting various datatypes. *)
module type T = sig

  (************************************************************************)
  (** Functions common to all datatype submodules. *)
  module Common : sig
    type pathstyle =
      | Lines
      | Points of string
      | Linespoints of string
      | Impulses
    type filledcurves = Color.t * Color.t (* f1 > f2, f2 < f1 *)

    val fx : ?xlog:bool -> ?ylog:bool -> ?min_step:float ->
      ?max_yrange:float -> ?nsamples:int ->
      ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
      Viewport.Viewport.t -> (float -> float) -> float -> float -> unit

  val xy_param : ?min_step:float -> ?nsamples:int -> ?fill:bool ->
    ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    Viewport.Viewport.t -> (float -> float * float) -> float -> float -> unit

(*    (* TODO we want to have control over the stroke properties for each curve *)
    val filledcurves : V.t -> ?nsamples:int -> ?fill:filledcurves ->
      (float -> float) -> (float -> float) -> float -> float -> unit*)
  end
(*
  module Array : sig
    include COMMON

    val x : t -> ?color: Color.t -> ?mark:string -> ?n0:int ->
      float array -> unit

    val xy : t -> ?color: Color.t -> ?mark:string ->
      float array -> float array -> unit
  end

  module List : sig
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
