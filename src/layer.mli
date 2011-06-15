(**A [Layer.t] retains all orders that would be made and computes the
   extents of the drawing. Then, when flushing it, it applies all the
   orders with a correct scaling (so that, for example, all the
   drawing is visible on the device). See [flush] for more information.

   Note that text extents, which are backend-dependent, are treated
   differently than the other (path) extents. *)


type limitation =
    Unlimited
      (**All zooms are allowed*)
  | Limited_out of float
      (**Do not zoom out so that one user unit becomes greater than
         the specified float of device units*)
  | Limited_in of float
      (**Do not zoom in so that one user unit becomes lesser than the
         specified float of device units*)
  | Limited of float * float
      (**Combinations of the previous two: in, out*)

(**Type indicating the limitations of automatic rescaling*)

type scaling =
    Not_allowed
      (**No rescaling*)
  | Uniform of limitation
      (**Apply the same scale factor on the two axes*)
  | Free of limitation * limitation
      (**Each axis does its rescaling independently*)

(**Type indicating which rescaling will be used*)

(*
type styles =
    (float * float array) * Backend.line_join * Backend.line_cap * float *
      Coordinate.t * string * int * Backend.slant * Backend.weight*)
type t
(**The type for the layer*)

type error =
    No_current_point (**Cannot determine a current point*)
  | Restore_without_saving of string (**No saved states. The string can
                                        be either "settings" or "data".*)
  | No_current_path (**Cleared the previous path and/or not starting a new one.*)
  | Unset_style of string (**The style which will be used is what the backend
                             provides when flushing*)
 (* | Non_invertible_initial_matrix (**Need to invert the
                                     (non-invertible) backend's
                                     initial transformation matrix*)*)
      (**Possible errors when working with a layer.*)


exception Error of error
  (**Raised when there is a nonvalid task to perform -- see above.*)

val string_of_error: error -> string

val make : unit -> t
  (**Creates a new layer*)

(** {2 Coordinate transformations}*)
val translate : t -> x:float -> y:float -> unit
  (**[translate layer x y] makes a translation of the [layer] in the
     direction ([x],[y]). All subsequent drawings will be expressed in
     the new coordinates, that is, those which have been translated by
     the vector ([x],[y]).*)

val scale : t -> x:float -> y:float -> unit
  (**[scale layer x y] makes a rescaling of the [layer] with factors
     [x] and [y], respectively on abscissas and ordinates. All
     subsequent drawings will be expressed in the new coordinates,
     that is, those which have been scaled by [x] and [y].*)

val rotate: t -> angle:float -> unit

(*FIXME: useful?  NB: the others (transform,...) are NOT, because we
  can perform these tasks by getting the matrix, then apply what we want
  thanks to Backend.Matrix module.

  val apply : next:Backend.matrix -> t -> unit

(**[apply next layer] composes the existing transformation in [layer]
  with [next]. The coordinate system in [layer] is modified by
  applying [next] to the existing transformation.*)
*)
(*
val get_coord : t -> Backend.matrix
  (**Returns the coordinate system currently affecting the layer.*)

val reset_to_id : t -> unit
  (**Resets the layer's coordinate system to the identity.*)
*)
(**{2 Backend subsequent operations}*)
val set_color : t -> Color.t -> unit
  (**Sets the layer's current color to the specified [Color.t].*)

val set_line_width : t -> float -> unit
  (**Sets the layer's line width.*)

val set_line_cap : t -> Backend.line_cap -> unit
  (**Sets the layer's line cap mode.*)

val set_dash : t -> float  -> float array -> unit
  (**Sets the layer's dash mode.*)

val set_line_join : t -> Backend.line_join -> unit
  (**Sets the layer's line join mode.*)

val get_line_width : t -> float
  (**Returns the current line width.*)

val get_line_cap : t -> Backend.line_cap
  (**Returns the current line cap mode.*)

val get_dash : t -> float array * float
  (**Returns the current dash mode.*)

val get_line_join : t -> Backend.line_join
  (**Returns the current line join mode.*)
(*
val set_matrix : t -> Backend.matrix -> unit
  (** Set the current transformation matrix which is the matrix
      transforming user to layer coordinates. *)

val get_matrix : t -> Backend.matrix
  (** Return the current transformation matrix on the layer.  Modifying this
      matrix does not affect the matrix held in [t]. *)*)

val move_to : t -> x:float -> y:float -> unit
  (**Moves the current point to ([x],[y]).*)

val line : t -> ?x0:float -> ?y0:float -> float -> float -> unit
  (**Makes a line between ([x0],[y0]) (or, if not given, the current
     point) and the point specified.*)

val line_to : t -> x:float -> y:float -> unit
  (**Alias for line, without optional argument.*)

val get_point : t -> float * float
  (**Returns the current point. If there's no current point, an [Error
     (No_current_point)] is raised.*)

val rel_move_to : t -> x:float -> y:float -> unit
  (**Relative move to.*)

val rel_line_to : t -> x:float -> y:float -> unit
  (**Relative line to.*)

val curve :
  t ->
  ?x0:float ->
  ?y0:float ->
  x1:float -> y1:float -> ?x2:float -> ?y2:float -> float -> float -> unit
  (**Makes a Bezier curve between ([x0],[y0]) (or the current point)
     and the last point, with control ([x1],[y1]) (and also ([x2],[y2]),
     if given).*)

val curve_to :
  t ->
  x1:float -> y1:float -> x2:float -> y2:float -> x3:float -> y3:float -> unit
  (**Alias for [curve] without optional arguments.*)

val rel_curve_to :
  t ->
  x1:float -> y1:float -> ?x2:float -> ?y2:float -> x3:float -> y3:float -> unit
  (**Relative Bezier curve.*)

val arc : t -> x:float -> y:float -> r:float -> a1:float -> a2:float -> unit

val rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
  (**Draws a rectangle.*)

val save : t -> unit
  (**Saves the layer's current settings.*)

val restore : t -> unit
  (**Restores the layer's state to previous saved settings. If there are
     no saved settings, raises [Error(Restore_without_saving "settings")]*)

val close_path : t -> unit
  (**Closes the current path.*)

val clear_path: t -> unit

val path_extents : t -> Backend.rectangle
  (**Returns the current path extents.*)

val stroke : t -> unit
  (**Strokes the current path and start a new one. Using this function
     makes the line width interpreted in backend coordinates, so it is
     invariant under deformation.*)

val fill : t -> unit
  (**Fills the current path and start a new one.*)

(*val clip : t -> unit
(**Clips along the current path and start a new one.*)*)

val stroke_preserve :  t -> unit
  (**Same as [stroke], but preserves the current path.*)

val fill_preserve : t -> unit
  (**Same as [fill], but preserves the current path.*)

(*val clip_preserve : t -> unit
(**Same as [clip], but preserves the current path.*)*)

val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
  (** Establishes a new clip rectangle by intersecting the current
      clip rectangle.  This {i may clear} the current path. *)

val stroke_layer: t -> unit
  (**Same as [stroke] except that line width is understood as {i
     layer} coordinates.*)

val stroke_layer_preserve: t -> unit
  (**Same as [stroke_preserve] except that line width is understood as {i
     layer} coordinates.*)

val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit
  (** [select_font_face t slant weight family] selects a family
      and style of font from a simplified description as a family
      name, slant and weight.  Family names are backend dependent.  *)
val set_font_size : t -> float -> unit
  (** Set the scaling of the font. *)
val show_text : t -> rotate:float -> x:float -> y:float ->
  Backend.text_position -> string -> unit
  (** [show_text angle x y pos txt] display [txt] at the point
      ([x],[y]) as indicated by [pos].  The point ([x],[y]) is in
      the current coordinate system but the current transformation
      matrix will NOT be applied to the text itself.  [angle]
      indicates by how many radians (in the current coordinate
      system) the text must be rotated -- [rotate <> 0.] may not be
      supported on all devices.  This is an immediate operation: no
      [stroke] nor [fill] are required (nor will have any effect).  *)

(**{2 Point styles}*)

val point : t -> string -> unit
  (**Draws the point at the position specified by the floats, according
     to the point style set.*)


(**{2 Layer operations -- Flushing}*)
val save_layer: t -> unit
  (**Saves the current status of the layer. This puts a "break" into
     the orders, so that a [restore_layer] removes all orders added after
     this break.*)

val restore_layer: t -> unit
  (**Restores the layer in a previous saved status. Raises
     [Error(Restore_without_saving "data")] if there's no saved point.*)

(**The pair [save_layer]/[restore_layer] can be used, for example, to
   make a common background (to be flushed to all backends), then specify
   the drawings before flushing in a specific backend.*)

val layer_extents: ?autoscale:scaling -> ?handle:Coord_handler.t ->
  t -> Backend.rectangle
  (**Returns the extents of the layer. If no [handle] is given, then
     the extents does not take any text into account. The rectangle is
     expressed in layer coordinates.*)

val get_coord_transform : ?autoscale:scaling -> t -> ofsx:float -> ofsy:float ->
  width:float -> height:float -> Backend.matrix


val flush : ?autoscale:scaling -> t -> ofsx:float -> ofsy:float ->
  width:float -> height:float -> ?pos:Backend.text_position ->
  Coord_handler.t -> unit

(**[flush layer ofsx ofsy width height handle] copies the resulting
   drawing in the [layer], to the [handle], in the rectangle
   specified by the quantities [ofsx],[ofsy] (some corner of the
   rectangle), [width] and [height] (all these quantities are
   expressed in [handle]'s coordinates). If [width] and [height] are
   positive, then the point ([ofsx],[ofsy]) is the upper left corner
   of the rectangle (i.e., the one which has the smallest abscissa
   and ordinate). Note that [width] and/or [height] can be negative;
   in such cases, a symmetry along one (or both) median(s) of the
   rectangle will be applied when reproducing the drawing stored in
   [layer] on the [backend]'s drawing surface.

   Optional argument [autoscale] is by default fixed at [Uniform
   Unlimited], so there's by default no limitations on scaling, but
   if scaling, then it is done uniformly along the two axes.

   Optional argument [pos] specifies where the layer should take
   place if, for some restrictions, it is smaller than the rectangle
   specified.*)

(*Local Variables:*)
(*compile-command: "ocamlc -c layer.mli"*)
(*End:*)
