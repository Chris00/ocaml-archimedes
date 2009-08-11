(**Drawing on a backend with facilities to manipulate coordinate transformations.*)

type t
(**The main type (handle).*)

type error =
  | Not_Found of string (**Raised when no coordinate transformation has
                          the supplied name*)
  | Closed(**Raised if the handle has been already closed.*)
(**The type for errors.*)

exception Error of error
  (**Exception raised when an error has occured.*)

val make : ?dirs:string list -> string ->
  ?coord:Coordinate.t -> float -> float -> t
  (**[make backend w h] creates a handle whose underlying backend is
     created using the options given in [backend] (see also
     {!Backend.make} for more information). A coordinate
     transformation [coord] can be supplied; it will be the main
     transformation, reachable under the name "" (empty string). If it
     is not provided, then the main transformation is set to identity
     matrix. *)

val use : ?coord:Coordinate.t -> Backend.t -> t * Coordinate.ctm
  (**[use backend] makes a [t] with a preexistent backend; it also
     returns the current transformation matrix of [backend]. When calling
     it, the transformation matrix is modified and set to [coord] if
     supplied, or to the identity matrix.*)

val use_unit_square :
  Backend.t -> float -> float -> float -> float -> t * Coordinate.ctm
  (**[use_unit_square backend x1 y1 x2 y2] makes a [t] whose initial
     coordinate is defined as follows: the point ([x1], [y1]) will be
     expressed (0,0) in the new coordinates; the point ([x2],[y1])
     will be (1,0), and ([x1],[y2]) will be (0,1). So the new unit
     square has corners the three previous points, and ([x2],[y2]).*)

val use_normalized : Backend.t -> t * Coordinate.ctm
  (**[use_normalized handle] defines a [t] whose initial coordinate
     transformation is such that the device is normalized (as the square
     [0,1] x [0,1].*)

val get_handle : ?initial:Backend.matrix -> t -> Backend.t
  (**Retrieves the handle of a [t], applying the [initial] matrix if
     given before returning it.*)

val close : t -> unit
  (**Closes the given [t], killing all references to the stored
     coordinate transformations and closing the underlying backend. A
     closed [t] cannot be used further; if it is, it will raise an
     [Error Closed].*)

val translate : t -> ?name:string -> x:float -> y:float -> unit
  (**Translates the coordinate transformation whose name is [name] (or
     the current one if not given) by the vector ([x],[y]). See
     {!Coordinate.translate} for more information.*)

val scale : t -> ?name:string -> x:float -> y:float -> unit
  (**Applies a dilatation to the coordinate transformation whose name is [name] (or
     the current one if not given) by multiplying the X axis by [x] and the Y axis by [y]. See
     {!Coordinate.scale} for more information.*)

val rotate : t -> ?name:string -> angle:float -> unit
  (**Applies a rotation of angle [angle] to the coordinate
     transformation whose name is [name] (or the current one if not
     given).*)

val add_translate : t -> string -> ?from:string -> x:float -> y:float -> unit
  (**[add_translate t name ~from x y] creates a new coordinate
     transformation with name [name], depending on the transformation
     [~from] (or the current one if not given), that consists of applying
     this transformation, then translating the origin of the result to
     the point [(x,y)].*)

val add_scale : t -> string -> ?from:string -> x:float -> y:float -> unit
  (**[add_scale t name ~from x y] creates a new coordinate
     transformation with name [name], depending on the transformation
     [~from] (or the current one if not given), that consists of applying
     this transformation, then scaling the result with the coefficients [x] and [y].*)

val add_rotate : t -> string -> ?from:string -> angle:float -> unit
  (**[add_rotate t name ~from angle] creates a new coordinate
     transformation with name [name], depending on the transformation
     [~from] (or the current one if not given), that consists of
     applying this transformation, then rotating the axes of the
     result by [angle] radians (assuming the axes of the system
     referenced by [from] are orthonormal).*)

val add_transform : t -> string -> ?from:string -> Backend.Matrix.t -> unit
  (**[add_transform t name ~from matrix] creates a new coordinate
     transformation with name [name], depending on the transformation
     [~from] (or the current one if not given), that consists of applying
     this transformation, then applying the transformation stored in [matrix].*)

val set_coordinate : t -> string -> unit
  (**Sets the coordinate system in [t] to the system registered under
     [name]. Raises [Error (Not_Found name)] if there's no coordinate
     transformation registered under [name].*)

val get_coordinate : t -> Coordinate.t
  (**Returns (a copy of) the current transformation coordinate.*)

(**{2 Backend primitives}*)
val set_color : t -> Color.t -> unit
val set_line_width : t -> float -> unit
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
  (**Note that this [rectangle] does not always use [Backend.rectangle],
     because of possible shears in the coordinate transformation.*)

val close_path : t -> unit
val clear_path : t -> unit
val path_extents : t -> Backend.rectangle
val stroke : t -> unit
val stroke_preserve : t -> unit
val fill : t -> unit
val fill_preserve : t -> unit
val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
val save : t -> unit
val restore : t -> unit
val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit
val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit
  (** [select_font_face t slant weight family] selects a family and
      style of font from a simplified description as a family name,
      slant and weight.  Family names are backend dependent.  *)
val set_font_size : t -> float -> unit
  (** Set the size of the font. *)

val text_extents : t -> string -> Backend.rectangle
val point : t -> Pointstyle.t -> float -> float -> unit
val points : t -> Pointstyle.t -> (float * float) list -> unit


(*Local Variables:*)
(*compile-command: "ocamlc -c transform_coord.mli"*)
(*End:*)
