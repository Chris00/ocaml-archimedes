type t
val make : ?dirs:string list -> string -> ?coord:Coordinate.t ->
  float -> float -> t
val use : Backend.t -> Coordinate.t -> t
val use_unit_square: Backend.t -> float -> float -> float -> float -> t
(*FIXME:needed?*)
val get_handle : t -> Backend.t
val translate : t -> float -> float -> unit
val scale : t -> float -> float -> unit
val change_coord: t -> Coordinate.t -> unit
val width : t -> float
val height : t -> float
(*FIXME: needed, or done after querying the underlying backend?*)
val close : t -> unit
val set_color : t -> Color.t -> unit
val set_line_width : t -> float -> unit
val set_line_cap : t -> Backend.line_cap -> unit
val set_dash : t -> float -> float array -> unit
val set_line_join : t -> Backend.line_join -> unit
val get_line_width : t -> float
val get_line_cap : t -> Backend.line_cap
val get_dash : t -> float array * float
val get_line_join : t -> Backend.line_join
val move_to : t -> float -> float -> unit
val line_to : t -> float -> float -> unit
val rel_move_to : t -> float -> float -> unit
val rel_line_to : t -> float -> float -> unit
val curve_to :
  t ->
  x1:float ->
  y1:float -> x2:float -> y2:float -> x3:float -> y3:float -> unit
val close_path : t -> unit
val path_extents : t -> Backend.rectangle
val stroke : t -> unit
val stroke_preserve : t -> unit
val fill : t -> unit
val fill_preserve : t -> unit
val clip : t -> unit
val clip_preserve : t -> unit
val save : t -> unit
val restore : t -> unit
val text : t -> size:float -> x:float -> y:float -> string -> unit
val text_extents : t -> size:float -> string -> Backend.text_extents



(*Local Variables:*)
(*compile-command: "ocamlc -c transform_coord.mli"*)
(*End:*)
