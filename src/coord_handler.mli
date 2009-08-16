(**Drawing on a backend with facilities to manipulate coordinate transformations.*)

type name =
  | Device
  | Normalized
  | Marks
  | N of string
      (**Available names of coordinates. You can only create coordinates
         whose name are of the form N "..."; the others are built-in coordinate
         systems. More precisely,

         [Device] is the device coordinate system: all coordinates
         transformations will depend upon it;

         [Initial] is just used to store the initial coordinate
         trnasformation of a backend;

         [Normalized] is the transformation coordinate under which all
         points are expressed by percents;

         and [Marks] is the coordinate transformation used to render point marks.

         The other ones are user-defined coordinate systems.*)

type t
  (**The main type (handle).*)

type error =
  | Not_Found of string (**Raised when no coordinate transformation has
                           the supplied name*)
  | Closed(**Raised if the handle has been already closed.*)
  | Trying_modify_built_in
  | No_saved_states

      (**The type for errors.*)

exception Error of error
  (**Exception raised when an error has occured.*)

val string_of_error: error -> string

val make : ?dirs:string list -> string ->
  float -> float -> t
  (**[make backend w h] creates a handle whose underlying backend is
     created using the options given in [backend] (see also
     {!Backend.make} for more information). Default coordinate systems are created *)

val use : Backend.t -> t
  (**[use backend] makes a [t] with a preexistent backend; it also
     returns the current transformation matrix of [backend]. When calling
     it, the transformation matrix is modified and set to [coord] if
     supplied, or to the identity matrix.*)

val use_unit_square :
  Backend.t -> name:string -> float -> float -> float -> float -> t
  (**[use_unit_square backend name x1 y1 x2 y2] makes a [t] whose initial
     coordinate is defined as follows: the point ([x1], [y1]) will be
     expressed (0,0) in the new coordinates; the point ([x2],[y1])
     will be (1,0), and ([x1],[y2]) will be (0,1). So the new unit
     square has corners the three previous points, and ([x2],[y2]).
     The transformation is then stored under [name].*)

val use_normalized : Backend.t -> t
  (**[use_normalized handle] defines a [t] whose initial coordinate
     transformation is such that the device is normalized (as the square
     [0,1] x [0,1].*)

val get_handle : t -> Backend.t
  (**Returns the backend of a [t], after resetting the initial
     transformation on it, if any.*)

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
  (**Applies a dilatation to the coordinate transformation whose name
     is [name] (or the current one if not given) by multiplying the X
     axis by [x] and the Y axis by [y]. See {!Coordinate.scale} for
     more information.*)

val rotate : t -> ?name:string -> angle:float -> unit
  (**Applies a rotation of angle [angle] to the coordinate
     transformation whose name is [name] (or the current one if not
     given).*)

val scale_marks : t -> x:float -> y:float -> unit
  (**Applies a dilatation to the coordinate transformation currently
     affecting the marks by multiplying the X axis by [x] and the Y
     axis by [y]. See {!Coordinate.scale} for more information.*)

val rotate_marks : t -> angle:float -> unit
  (**Applies a rotation of angle [angle] to the coordinate
     transformation currently affecting the marks.*)

val add_translate : t -> string -> ?from:name -> x:float -> y:float -> unit
  (**[add_translate t name ~from x y] creates a new coordinate
     transformation with name [name], depending on the transformation
     [~from] (or the current one if not given), that consists of applying
     this transformation, then translating the origin of the result to
     the point [(x,y)].*)

val add_scale : t -> string -> ?from:name -> x:float -> y:float -> unit
  (**[add_scale t name ~from x y] creates a new coordinate
     transformation with name [name], depending on the transformation
     [~from] (or the current one if not given), that consists of applying
     this transformation, then scaling the result with the coefficients [x] and [y].*)

val add_rotate : t -> string -> ?from:name -> angle:float -> unit
  (**[add_rotate t name ~from angle] creates a new coordinate
     transformation with name [name], depending on the transformation
     [~from] (or the current one if not given), that consists of
     applying this transformation, then rotating the axes of the
     result by [angle] radians (assuming the axes of the system
     referenced by [from] are orthonormal).*)

val add_transform : t -> string -> ?from:name -> Backend.Matrix.t -> unit
  (**[add_transform t name ~from matrix] creates a new coordinate
     transformation with name [name], depending on the transformation
     [~from] (or the current one if not given), that consists of applying
     this transformation, then applying the transformation stored in [matrix].*)

val set_coordinate : t -> string -> unit
  (**Sets the coordinate system in [t] to the system registered under
     [name]. Raises [Error (Not_Found name)] if there's no coordinate
     transformation registered under [name].*)

val print_coordinate : t -> string
  (**Returns the name of the current transformation coordinate the
     handle obey.*)

val print_matrix : t -> unit
  (*val get_coordinate : t -> Coordinate.t
  (**Returns (a copy of) the current transformation coordinate.*)*)

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
val stroke_init : t -> unit
val stroke_init_preserve : t -> unit
val fill : t -> unit
val fill_preserve : t -> unit
val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
val save : t -> unit
val restore : t -> unit
val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit
  (** [select_font_face t slant weight family] selects a family and
      style of font from a simplified description as a family name,
      slant and weight.  Family names are backend dependent.  *)

val set_font_size : t -> float -> unit
  (** Set the size of the font. *)

val show_text : t -> rotate:float -> x:float -> y:float ->
  Backend.text_position -> string -> unit


val text_extents : t -> string -> Backend.rectangle
val render : t -> Pointstyle.name ->  Backend.rectangle

(*Local Variables:*)
(*compile-command: "ocamlc -c transform_coord.mli"*)
(*End:*)
