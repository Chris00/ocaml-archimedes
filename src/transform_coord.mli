(**Drawing on a backend without explicitly modify its coordinate transformation.*)

type t
val make : ?dirs:string list -> string -> ?coord:Backend.matrix ->
  ?point:Pointstyle.t -> float -> float -> t
val use : Backend.t -> ?point:Pointstyle.t -> Backend.matrix -> t
val use_unit_square: Backend.t -> float -> float -> float -> float -> t
  (*FIXME:needed?*)
val get_handle : t -> Backend.t
val translate : t -> x:float -> y:float -> unit
val scale : t -> x:float -> y:float -> unit
val rotate: t -> angle:float -> unit
val set_matrix : t -> Backend.matrix -> unit
val get_matrix : t -> Backend.matrix
val set_point_style : t -> Pointstyle.t -> unit
val get_point_style : t -> Pointstyle.t

(**{2 Backend primitives}*)
val width : t -> float
val height : t -> float
  (*FIXME: needed, or done after querying the underlying backend?*)
val close : t -> unit
val set_color : t -> Color.t -> unit
val set_line_width : t -> float -> unit
val set_line_cap : t -> Backend.line_cap -> unit
val set_dash : t -> float -> float array -> unit
val set_matrix: t -> Backend.matrix -> unit
val set_line_join : t -> Backend.line_join -> unit
val get_line_width : t -> float
val get_line_cap : t -> Backend.line_cap
val get_dash : t -> float array * float
val get_line_join : t -> Backend.line_join
val get_matrix : t -> Backend.matrix
val move_to : t -> x:float -> y:float -> unit
val line_to : t -> x:float -> y:float -> unit
val rel_move_to : t -> x:float -> y:float -> unit
val rel_line_to : t -> x:float -> y:float -> unit
val curve_to :
  t ->
  x1:float ->
  y1:float -> x2:float -> y2:float -> x3:float -> y3:float -> unit
val rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
  (**Note that this [rectangle] does not always use [Backend.rectangle],
     because of possible shears in the coordinate transformation.*)

val close_path : t -> unit
val clear_path : t -> unit
val path_extents : t -> Backend.rectangle
val stroke : t -> unit
val stroke_preserve : t -> unit
val fill : t -> unit
val fill_preserve : t -> unit
  (*val clip : t -> unit
    val clip_preserve : t -> unit*)
val clip_rectangle : t  -> x:float -> y:float -> w:float -> h:float -> unit
val save : t -> unit
val restore : t -> unit
val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit
  (** [select_font_face t slant weight family] selects a family and
      style of font from a simplified description as a family name,
      slant and weight.  Family names are backend dependent.  *)
val set_font_size : t -> float -> unit
  (** Set the scaling of the font. *)
val show_text : t -> rotate:float -> x:float -> y:float ->
  Backend.text_position -> string -> unit
val text_extents : t -> string -> Backend.rectangle

val point: t -> float -> float -> unit

val points:t -> (float * float) list -> unit




(*Local Variables:*)
(*compile-command: "ocamlc -c transform_coord.mli"*)
(*End:*)
