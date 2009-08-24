module Handle: sig
  type t

  val make: dirs:string list -> string -> float -> float -> t
    (** [make backend width height] creates a new handle, whose
        underlying backend has the given dimensions.

        [backend] is the name of the underlying engine, followed by one
        or several options separated by spaces.  For example, "Graphics"
        for the graphics backend or "Cairo PNG filename" for the Cairo
        backend, using a PNG surface to be saved in [filename]. *)

  val close : t -> unit
    (** Close the handle.  For some backends, the output will not be
        complete until this function is called. *)


  module Viewport: sig
    type vp

    val make: t ->
      xmin:float -> xmax:float -> ymin:float -> ymax:float -> vp
      (**Creates a new viewport for the Archimedes handle specified. The
         coordinate transformation stored by it will be the one for
         which the rectangle specified by the four float values will be
         expressed as a unit square (that is, [(xmin,ymin)] will be
         [(0.,0.)] and [(xmax,ymax)] will be [(1.,1.)]). The values [x/y
         min/max] are expressed in current Archimedes coordinates.*)

    val sub: vp -> xmin:float -> xmax:float -> ymin:float -> ymax:float -> vp
      (**Same as [new_viewport] except that the creation uses another
         viewport. Note that the created viewport will depend on the
         viewport which made it; more precisely, a changement on the
         coordinates in the initial viewport will induce a changement
         on the coordinates in the created one.*)

    val use: vp -> unit
      (**Makes the Archimedes handle use the coordinates stored in
         this viewport.*)

    (**{2 Convenience functions for creating viewports}*)

    val rows: t -> int -> vp array
      (**[rows a n] returns an array of viewports, the viewport stored
         in [i]th position makes the transformation for the [i+1]th row.*)
    val columns: t -> int -> vp array
      (**[columns a n] returns an array of viewports, the viewport stored
         in [i]th position makes the transformation for the [i+1]th column.*)
    val matrix: t -> int -> int -> vp array array
      (**[matrix a n m] returns an array of arrays of viewports, the
         viewport stored in [j]th position into array [i] (that is,
         result[.(i).(j)]) makes the transformation for the rectangle
         located in [i+1]th row, [j+1]th column.*)
    val sub_rows: vp -> int -> vp array
    val sub_columns: vp -> int -> vp array
    val sub_matrix: vp -> int -> int -> vp array array
  end


  val plotfx : t -> ?axes:([>Axes.axes],[>Axes.tic]) Axes.t ->
    ?nsamples:int -> ?min_step:float ->
    (float -> float) -> float -> float -> unit

  val plotxy :
    t ->
    ?axes:([> Axes.axes ], [> Axes.tic ]) Axes.t ->
    ?f:(t -> Pointstyle.name -> float -> float -> unit) ->
    ?mark:Pointstyle.name -> Iterator.t -> unit
end
