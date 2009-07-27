

type slant = Upright | Italic

type weight = Normal | Bold

type text_position =
  | CC  (** centrer horizontally and vertically *)
  | LC  (** align left horizontally and center vertically *)
  | RC  (** align right horizontally and center vertically *)
  | CT  (** center horizontally and align top vertically *)
  | CB
  | LT
  | LB
  | RT
  | RB

exception Non_existent

val available: unit -> string list
val load: ?exts:string * string * string -> ?dep:string  -> ?unsafe:bool ->
  string -> unit


type handle
    (** Handle of a backend or a coordinate system. *)

val finish : handle -> unit
val set_pointstyle : handle -> pointstyle -> unit
val set_pattern : handle -> pattern -> unit
val set_color : handle -> Color.t -> unit
val set_thickness : handle -> thickness -> unit
val set_endstyle : handle -> endstyle -> unit
val set_linestyle : handle -> linestyle -> unit
val set_anglestyle : handle -> anglestyle -> unit
val get_pointstyle: handle -> pointstyle
val get_pattern: handle -> pattern
val get_thickness: handle -> thickness
val get_endstyle: handle -> endstyle
val get_linestyle: handle -> linestyle
val get_anglestyle: handle -> anglestyle

val string_of_error : handle -> backend_error -> string
  (*val point : handle -> float -> float -> unit*)
val move : handle -> float -> float -> unit
val get_point: handle -> float*float
val line : handle -> ?x:float -> ?y:float -> float -> float -> unit
val rel_move : handle -> float -> float -> unit
val rel_line : handle -> float -> float -> unit
val bezier :
  handle ->
  ?x0:float ->
  ?y0:float ->
  cx1:float ->
  cy1:float -> ?cx2:float -> ?cy2:float -> x3:float -> y3:float -> unit
val rel_bezier :
  handle ->
  cx1:float ->
  cy1:float -> ?cx2:float -> ?cy2:float -> x3:float -> y3:float -> unit
val ellipse_arc :
  handle ->
  ?x:float -> ?y:float -> a:float -> b:float -> float -> float -> unit
val ellipse : handle -> ?x:float -> ?y:float -> a:float -> b:float -> unit
val circle_arc :
  handle -> ?x:float -> ?y:float -> r:float -> float -> float -> unit
val circle : handle -> ?x:float -> ?y:float -> r:float -> unit
val rectangle : handle -> ?x:float -> ?y:float -> float -> float -> unit
val rectangle_box : handle -> rectangle -> unit
val save : handle -> unit
val restore : handle -> unit
val translate : handle -> float -> float -> unit
val scale : handle -> float -> float -> unit
val rotate : handle -> float -> unit
val transform : handle -> float -> float -> float * float
val transform_dist : handle -> float -> float -> float * float
val invert : handle -> coord
val inv_transform : handle -> float -> float -> float * float
val inv_transform_dist : handle -> float -> float -> float * float
val apply : next:coord -> handle -> unit
val get_coord : handle -> coord
val reset_to_id : handle -> unit
val close_path : handle -> unit
val path_box : handle -> rectangle
val clip : ?keeppath:bool -> handle -> unit
val fill : ?keeppath:bool -> handle -> unit
val draw : ?keeppath:bool -> handle -> unit
val text : handle -> ?size:float -> x:float -> y:float -> string -> unit
val put_image :
  handle -> x:float -> y:float -> ?scale:float -> string -> unit


(** {2 Registering new modules} *)

val register :
  ?force:bool ->
  create:(float -> float -> 'a) ->
  finish:('a -> unit) ->
  set_pointstyle:('a -> pointstyle -> unit) ->
  set_pattern:('a -> pattern -> unit) ->
  set_color:('a -> Color.t -> unit) ->
  set_thickness:('a -> thickness -> unit) ->
  set_endstyle:('a -> endstyle -> unit) ->
  set_linestyle:('a -> linestyle -> unit) ->
  set_anglestyle:('a -> anglestyle -> unit) ->
  get_pointstyle: ('a -> pointstyle) ->
  get_pattern: ('a -> pattern) ->
  get_thickness: ('a -> thickness) ->
  get_endstyle: ('a -> endstyle) ->
  get_linestyle: ('a -> linestyle) ->
  get_anglestyle: ('a -> anglestyle) ->

  string_of_error:(backend_error -> string) ->
  move:('a -> float -> float -> unit) ->
  get_point:('a -> float*float) ->
  line:('a -> ?x:float -> ?y:float -> float -> float -> unit) ->
  rel_move:('a -> float -> float -> unit) ->
  rel_line:('a -> float -> float -> unit) ->
  bezier:('a ->
            ?x0:float ->
           ?y0:float ->
           cx1:float ->
           cy1:float ->
           ?cx2:float -> ?cy2:float -> x3:float -> y3:float -> unit) ->
  rel_bezier:('a ->
                cx1:float ->
               cy1:float ->
               ?cx2:float -> ?cy2:float -> x3:float -> y3:float -> unit) ->
  ellipse_arc: 'a ellipse_arc ->
  ?ellipse:('a -> ?x:float -> ?y:float -> a:float -> b:float -> unit) ->
  ?circle_arc:('a ->
                 ?x:float -> ?y:float -> r:float -> float -> float -> unit) ->
  ?circle:('a -> ?x:float -> ?y:float -> r:float -> unit) ->
  rectangle:('a -> ?x:float -> ?y:float -> float -> float -> unit) ->
  rectangle_box:('a -> rectangle -> unit) ->
  save:('a -> unit) ->
  restore:('a -> unit) ->
  translate : ('a -> float -> float -> unit) ->
  scale : ('a -> float -> float -> unit) ->
  rotate : ('a -> float -> unit) ->
  transform : ('a -> float -> float -> float * float) ->
  transform_dist : ('a -> float -> float -> float * float) ->
  invert : ('a -> coord) ->
  inv_transform : ('a -> float -> float -> float * float) ->
  inv_transform_dist : ('a -> float -> float -> float * float) ->
  apply : (next:coord -> 'a -> unit) ->
  get_coord :('a -> coord) ->
  reset_to_id : ('a -> unit) ->
  close_path:('a -> unit) ->
  path_box:('a -> rectangle) ->
  clip:(?keeppath:bool -> 'a -> unit) ->
  fill:(?keeppath:bool -> 'a -> unit) ->
  draw:(?keeppath:bool -> 'a -> unit) ->
  select_font_face:('a -> fontslant -> fontweight -> fontname -> unit) ->
  text:('a -> ?size:float -> x:float -> y:float -> string -> unit) ->
  put_image:('a -> x:float -> y:float -> ?scale:float -> string -> unit) ->
  string
  -> unit

(* Are [save] and [restore] needed?  Is [clip] needed ?  How to
   implement it for graphics ?  Do we want to rotate text ?
*)
