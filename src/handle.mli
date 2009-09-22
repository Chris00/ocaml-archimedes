type t
val make : dirs:string list -> string -> float -> float -> t
val close : t -> unit
val immediate : t -> bool -> unit
  (**[immediate handle b] makes the handle do immediately all the
     orders if [b] is [true]; it makes the handle wait an [immediate
     handle true] or a [close handle] to do the orders if [b] is
     false. *)

val backend : t -> Backend.t

type viewport
  (**Viewports creation.*)
module Viewport :
sig
  val make :
    t -> xmin:float -> xmax:float -> ymin:float -> ymax:float -> viewport
    (**[make t ~xmin ~ymin ~ymin ~ymax] creates a new viewport,
       whose coordinate system makes the rectangle delimited by the
       given values (expressed in the current [t] coordinate system) as
       the new unit square.*)
  val sub :
    viewport -> xmin:float -> xmax:float -> ymin:float -> ymax:float -> viewport
    (**Same as [make] but the values are expressed in viewport's coordinates.*)
  val make_rect :
    t -> x:float -> y:float -> w:float -> h:float -> viewport
    (**[make_rect t x y w h] is equivalent to [make t x y (x+.w) (y+.h)].*)
  val sub_rect :
    viewport -> x:float -> y:float -> w:float -> h:float -> viewport
    (**[sub_rect vp x y w h] is equivalent to [sub vp x y (x+.w) (y+.h)].*)

  (**{2 Convenience functions to create viewports}*)
  val rows : t -> int -> viewport array
  val columns : t -> int -> viewport array
  val grid : t -> int -> int -> viewport array array
  val sub_rows : viewport -> int -> viewport array
  val sub_columns : viewport -> int -> viewport array
  val sub_matrix : viewport -> int -> int -> viewport array array
end

(**{2 Using viewports}*)
val use : viewport -> unit
val use_initial : t -> unit
val set_line_width : t -> float -> unit
val set_mark_size : t -> float -> unit
val set_font_size : t -> float -> unit
val set_rel_line_width : t -> float -> unit
val set_rel_mark_size : t -> float -> unit
val set_rel_font_size : t -> float -> unit
val set_global_line_width : t -> float -> unit
val set_global_mark_size : t -> float -> unit
val set_global_font_size : t -> float -> unit
val get_line_width : t -> float
val get_mark_size : t -> float

(**{2 Backend primitives}*)
val width : t -> float
val height : t -> float
val set_color : t -> Color.t -> unit
(* val set_line_width : t -> float -> unit *)
val set_line_cap : t -> Backend.line_cap -> unit
val set_dash : t -> float -> float array -> unit
val set_line_join : t -> Backend.line_join -> unit
val get_line_width : t -> float
val get_line_cap : t -> Backend.line_cap
val get_dash : t -> float array * float
val get_line_join : t -> Backend.line_join
val move_to : t -> x:float -> y:float -> unit
val line_to : t -> x:float -> y:float -> unit
val rel_move_to : t -> x:float -> y:float -> unit
val rel_line_to : t -> x:float -> y:float -> unit
val curve_to :
  t ->
  x1:float ->
  y1:float -> x2:float -> y2:float -> x3:float -> y3:float -> unit
val rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
val arc : t -> r:float -> a1:float -> a2:float -> unit
val close_path : t -> unit
val clear_path : t -> unit
(*val path_extents : t -> rectangle*)
val stroke_current : t -> unit
val stroke_current_preserve : t -> unit
val stroke : t -> unit
val stroke_preserve : t -> unit
val fill : t -> unit
val fill_preserve : t -> unit
val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
val save_vp : t -> unit
val restore_vp : t -> unit
val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit
val show_text :
  t ->
  rotate:float ->
  x:float -> y:float -> Backend.text_position -> string -> unit
  (*  val text_extents : t -> string -> rectangle*)
val render : t -> string -> unit
(* val mark_extents : t -> string -> rectangle *)

(**{2 Plotting}*)
val xyf :
  t ->
  ?color: Color.t ->
  ?nsamples:int ->
  ?min_step:float ->
  ?do_with:(t -> float * float -> unit) ->
  ?finish:(t -> unit) ->
  (float -> float * float) -> float -> float -> unit
val xy :
  t ->
  ?color: Color.t ->
  ?axes:([> Axes.axes ],[> Axes.tic]) Axes.t ->
  ?mark:string -> ?do_with:(t -> float -> float -> unit) ->
  Iterator.t -> unit

val make_xaxis :
  ([> Axes.tic] as 'a) -> ([> Axes.data] as 'b) -> Backend.text_position -> 'a ->
  ?get_labels:(bool -> 'b -> Axes.label_collection) ->
  ?get_position:(([> Axes.loc_tics] as 'c) ->
                   Axes.label_collection -> Axes.tic_position) ->
  ?tic_extents:('a -> Matrix.rectangle) ->
  'c -> 'a Axes.axis
val make_yaxis :
  ([> Axes.tic] as 'a) -> ([>Axes.data] as 'b) -> Backend.text_position -> 'a ->
  ?get_labels:(bool -> 'b -> Axes.label_collection) ->
  ?get_position:(([>Axes.loc_tics] as 'c) ->
                   Axes.label_collection -> Axes.tic_position) ->
  ?tic_extents:('a -> Matrix.rectangle) ->
  'c -> 'a Axes.axis

val axes : t -> ?color:Color.t ->
  ?type_axes_printer:(([> Axes.axes ] as 'a) -> Axes.ranges -> t -> unit) ->
  ?axes_meeting:('a -> Axes.ranges -> float * float) ->
  ?print_tic:(t -> ([> Axes.tic] as 'b) -> unit) ->
  ('a, 'b) Axes.t -> unit
  (** Says that axes are required and should be in the current viewport
     of [t], following the given parameters. Note that this function creates a new
     viewport and sets the handle's current viewport to it. But if the
     axes take too big margins (reducing the graph to nothing), the
     current viewport is maintained. In this latter case, the axes are
     not guaranteed to fit in the viewport. *)

val direct_axes : t -> ?color:Color.t  -> ([>Axes.axes] as 'a) ->
  ?type_axes_printer:('a -> Axes.ranges -> Backend.t -> unit) ->
  ?axes_meeting:('a -> Axes.ranges -> float * float) ->
  ?print_tic:(Backend.t -> ([> Axes.tic] as 'b) -> unit) ->
  'b Axes.axis -> 'b Axes.axis -> Axes.ranges -> unit

val current_vp: t -> viewport
  (** Returns the current viewport of the handle. *)


val xrange : t -> float -> float -> unit
val yrange : t -> float -> float -> unit

(** The computed ranges (after update) for the current viewport. *)
val xmin : t -> float
val xmax : t -> float
val ymin : t -> float
val ymax : t -> float

(**/**)
val update_coords : t -> float -> float -> unit
