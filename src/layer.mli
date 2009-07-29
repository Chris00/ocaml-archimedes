type limitation =
    Unlimited
  | Limited_out of float
  | Limited_in of float
  | Limited of float * float
type scaling =
    Not_allowed
  | Uniform of limitation
  | Free of limitation * limitation
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

type error = No_current_point
exception Error of error
val make : ?autoscale:scaling -> unit -> t
val reinit_path_ext : t -> unit
val translate : t -> float -> float -> unit
val scale : t -> float -> float -> unit
val transform : t -> float -> float -> float * float
val transform_dist : t -> float -> float -> float * float
val invert : t -> Coordinate.t
val inv_transform : t -> float -> float -> float * float
val inv_transform_dist : t -> float -> float -> float * float
val apply : next:Coordinate.t -> t -> unit
val get_coord : t -> Coordinate.t
val reset_to_id : t -> unit
val set_color : t -> Color.t -> unit
val set_line_width : t -> float -> unit
val set_line_cap : t -> Backend.line_cap -> unit
val set_dash : t -> float * float array -> unit
val set_line_join : t -> Backend.line_join -> unit
val get_line_width : t -> float
val get_line_cap : t -> Backend.line_cap
val get_dash : t -> float * float array
val get_line_join : t -> Backend.line_join
val move_to : t -> float -> float -> unit
val line : t -> ?x:float -> ?y:float -> float -> float -> unit
val line_to : t -> float -> float -> unit
val get_point : t -> float * float
val rel_move_to : t -> float -> float -> unit
val rel_line_to : t -> float -> float -> unit
val curve :
  t ->
  ?x0:float ->
  ?y0:float ->
  x1:float -> y1:float -> ?x2:float -> ?y2:float -> float -> float -> unit
val curve_to :
  t ->
  x1:float -> y1:float -> x2:float -> y2:float -> float -> float -> unit
val rel_curve :
  t ->
  x1:float -> y1:float -> ?x2:float -> ?y2:float -> float -> float -> unit
val rectangle : t -> ?x:float -> ?y:float -> float -> float -> unit
val save : t -> unit
val restore : t -> unit
val close_path : t -> unit
type rectangle =
  Backend.rectangle = {
  x : float;
  y : float;
  w : float;
  h : float;
}
val path_extents : t -> rectangle
val stroke : t -> unit
val fill : t -> unit
val clip : t -> unit
val stroke_preserve : t -> unit
val fill_preserve : t -> unit
val clip_preserve : t -> unit
val text : t -> size:float -> x:float -> y:float -> string -> unit
val flush : t -> ofsx:float -> ofsy:float -> width:float -> height:float ->
  Backend.t -> unit
(*Local Variables:*)
(*compile-command: "ocamlc -c layer.mli"*)
(*End:*)
