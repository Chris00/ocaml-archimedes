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
type t (*= {
  ofsx : float;
  ofsy : float;
  width : float;
  height : float;
  mutable current : (float * float) option;
  coord : Coordinate.t;
  mutable dash : float * float array;
  mutable lj : Backend.line_join;
  mutable lc : Backend.line_cap;
  mutable color : Color.t;
  mutable lw : float;
  stack : styles Stack.t;
  mutable font : string;
  mutable fsize : int;
  mutable fslant : Backend.slant;
  mutable fweight : Backend.weight;
  mutable pxmin : float;
  mutable pxmax : float;
  mutable pymin : float;
  mutable pymax : float;
  mutable xmin : float;
  mutable xmax : float;
  mutable ymin : float;
  mutable ymax : float;
  mutable upmargin : float;
  mutable downmargin : float;
  mutable leftmargin : float;
  mutable rightmargin : float;
  autoscale : scaling;
  orders : (Backend.t -> unit) Q.t;
}*)
(**The type for the layer*)

type error =
    No_current_point
  | Restore_without_saving
(**Possible errors when working with a layer.*)


exception Error of error
  (**Raised when there is a nonvalid task to perform -- see above.*)

val make : unit -> t
  (**Creates a new layer*)

val translate : t -> float -> float -> unit
  (**[translate layer x y] makes a translation of the [layer] in the
     direction ([x],[y]). All subsequent drawings will be expressed in
     the new coordinates, that is, those which have been translated by
     the vector ([x],[y]).*)

val scale : t -> float -> float -> unit
  (**[scale layer x y] makes a rescaling of the [layer] with factors
     [x] and [y], respectively on abscissas and ordinates. All
     subsequent drawings will be expressed in the new coordinates,
     that is, those which have been scaled by [x] and [y].*)

(*FIXME: useful?
  val transform : t -> float -> float -> float * float
(**[transform layer x y] returns the point x,y transformed by the coordinate
  changing on [layer].*)

  val transform_dist : t -> float -> float -> float * float
  val invert : t -> Coordinate.t
  val inv_transform : t -> float -> float -> float * float
  val inv_transform_dist : t -> float -> float -> float * float
*)

val apply : next:Coordinate.t -> t -> unit
  (**[apply next layer] composes the existing transformation in [layer]
     with [next]. The coordinate system in [layer] is modified by applying
     [next] to the existing transformation.*)

val get_coord : t -> Coordinate.t
  (**Returns the coordinate system currently affecting the layer.*)

val reset_to_id : t -> unit
  (**Resets the layer's coordinate system to the identity.*)

val set_color : t -> Color.t -> unit
  (**Sets the layer's current color to the specified [Color.t].*)

val set_line_width : t -> float -> unit
  (**Sets the layer's line width.*)

val set_line_cap : t -> Backend.line_cap -> unit
  (**Sets the layer's line cap mode.*)

val set_dash : t -> float * float array -> unit
  (**Sets the layer's dash mode.*)

val set_line_join : t -> Backend.line_join -> unit
  (**Sets the layer's line join mode.*)

val get_line_width : t -> float
  (**Returns the current line width.*)

val get_line_cap : t -> Backend.line_cap
  (**Returns the current line cap mode.*)

val get_dash : t -> float * float array
  (**Returns the current dash mode.*)

val get_line_join : t -> Backend.line_join
  (**Returns the current line join mode.*)

val move_to : t -> float -> float -> unit
  (**Moves the current point to ([x],[y]).*)

val line : t -> ?x:float -> ?y:float -> float -> float -> unit
  (**Makes a line between ([x],[y]) (or, if not given, the current
     point) and the point specified.*)

val line_to : t -> float -> float -> unit
  (**Alias for line, without optional argument.*)

val get_point : t -> float * float
  (**Returns the current point. If there's no current point, an [Error
     (No_current_point)] is raised.*)

val rel_move_to : t -> float -> float -> unit
  (**Relative move to.*)

val rel_line_to : t -> float -> float -> unit
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
  x1:float -> y1:float -> x2:float -> y2:float -> float -> float -> unit
  (**Alias for [curve] without optional arguments.*)

val rel_curve_to :
  t ->
  x1:float -> y1:float -> ?x2:float -> ?y2:float -> float -> float -> unit
  (**Relative Bezier curve.*)

val rectangle : t -> ?x:float -> ?y:float -> float -> float -> unit
  (**Draws a rectangle.*)

val save : t -> unit
  (**Saves the layer's current state.*)

val restore : t -> unit
  (**Restores the layer's state to a previous saved state. If there's
     no saved state, raises [Error(Restore_without_saving)]*)

val close_path : t -> unit
  (**Closes the current path.*)

val path_extents : t -> Backend.rectangle
  (**Returns the current path extents.*)

type coord_linewidth =
    Layer (**Expressed in layer coordinates*)
  | Backend (**Expressed in backend coordinates*)
      (**Type indicating how to interpret the line width and dash
         styles. A [Backend] interpretation makes the same stroking
         independently of the zoom applied when flushing. *)

val stroke : ?lw:coord_linewidth -> t -> unit
  (**Strokes the current path and start a new one. The [lw] argument
     indicates how to stroke. Default is [Backend]: make it invariant
     under deformation.*)

val fill : t -> unit
  (**Fills the current path and start a new one.*)

(*val clip : t -> unit
(**Clips along the current path and start a new one.*)*)

val stroke_preserve : ?lw:coord_linewidth -> t -> unit
  (**Same as [stroke], but preserves the current path.*)

val fill_preserve : t -> unit
  (**Same as [fill], but preserves the current path.*)

(*val clip_preserve : t -> unit
(**Same as [clip], but preserves the current path.*)*)

val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
  (** Establishes a new clip rectangle by intersecting the current
      clip rectangle.  This {i may clear} the current path. *)


val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit
  (** [select_font_face t slant weight family] selects a family
      and style of font from a simplified description as a family
      name, slant and weight.  Family names are bakend dependent.  *)
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

val flush : ?autoscale:scaling -> t -> ofsx:float -> ofsy:float ->
  width:float -> height:float -> Backend.t -> unit
  (**[flush layer ofsx ofsy width height backend] copies the resulting
     drawing in the [layer], to the [backend], in the rectangle
     specified by the quantities [ofsx],[ofsy] (some corner of the
     rectangle), [width] and [height]. If [width] and [height] are
     positive, then the point ([ofsx],[ofsy]) is the upper left corner
     of the rectangle (i.e., the one which has the smallest abscissa
     and ordinate). Note that [width] and/or [height] can be negative;
     in such cases, a symmetry along one (or both) median(s) of the
     rectangle will be applied when reproducing the drawing stored in
     [layer] on the [backend]'s drawing surface.

     Optional argument [autoscale] is by default fixed at [Uniform
     Unlimited], so there's by default no limitations on scaling, but
     if scaling, then it is done uniformly along the two axes.*)

val make_axes :
  t ->
  ?color_axes:Color.t ->
  ?color_labels:Color.t -> Axes.data -> Axes.mode -> unit

(*Local Variables:*)
(*compile-command: "ocamlc -c layer.mli"*)
(*End:*)
