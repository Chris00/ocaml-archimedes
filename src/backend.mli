(** Module providing a uniform interface and managing the dynamic
    loading of the backends.  This modules is only useful to create
    new backends and should not be used for plotting data. *)

type line_cap =
| BUTT  (** start(stop) the line exactly at the start(end) point *)
| ROUND (** use a round ending, the center of the circle is the end point *)
| SQUARE (** use squared ending, the center of the square is the end point *)

type line_join =
| JOIN_MITER (** use a sharp (angled) corner *)
| JOIN_ROUND (** use a rounded join, the center of the circle is the
                 joint point *)
| JOIN_BEVEL (** use a cut-off join, the join is cut off at half the line
                 width from the joint point *)

type text_position =
| CC  (** centrer horizontally and vertically *)
| LC  (** align left horizontally and center vertically *)
| RC  (** align right horizontally and center vertically *)
| CT  (** center horizontally and align top vertically *)
| CB  (** center horizontally and align bottom vertically *)
| LT  (** align left horizontally and top vertically *)
| LB  (** align left horizontally and bottom vertically *)
| RT  (** align right horizontally and top vertically *)
| RB  (** align right horizontally and bottom vertically *)


(** Specifies variants of a font face based on their slant. *)
type slant = Upright | Italic

(** Specifies variants of a font face based on their weight. *)
type weight = Normal | Bold

(** The interface that backends must provide to be registered. *)
module type T =
sig
  type t
  (** Handle to a backend. *)

  val set_color : t -> Color.t -> unit
  (** [set_color bk c] sets the color of the backend [bk] to [c]. *)
  val set_line_width : t -> float -> unit
  (** [set_line_width bk w] sets the line width of the backend [bk] to
      [w].  The line width is expressed in the natural backend
      coordinates (i.e. when the CTM is the identity). *)
  val set_line_cap : t -> line_cap -> unit
  (** [set_line_cap bk c] sets the line cap for the backend [bk] to [c]. *)
  val set_dash : t -> float -> float array -> unit
  (** [set_dash bk ofs pattern] *)
  val set_line_join : t -> line_join -> unit
  (** [set_line_join bk j] sets the line join for the backend [bk]
      to [j]. *)

  val get_line_width: t -> float
  val get_line_cap: t -> line_cap
  val get_dash: t -> float array * float
  val get_line_join: t -> line_join

  val move_to : t -> x:float -> y:float -> unit
  (** Begin a new sub-path.  After this call the current point will be
      [(x, y)]. *)
  val line_to : t -> x:float -> y:float -> unit
  (** [line_to bk x y] Adds a line to the path from the current point
      to position [(x, y)] in the current backend coordinates.  After
      this call the current point will be [(x, y)].

      If there is no current point before the call to [line_to] this
      function will behave as {!move_to}[ bk x y]. *)

  val rel_move_to : t -> x:float -> y:float -> unit
  val rel_line_to : t -> x:float -> y:float -> unit

  val curve_to : t ->
    x1:float -> y1:float ->
    x2:float -> y2:float ->
    x3:float -> y3:float -> unit
  (** [curve_to bk x1 y1 x2 y2 x3 y3] adds an Bezier curve to the
      path, starting at the current point, ending at point
      [(x3,y3)], with control points [(x1,y1)] and [(x2,y2)]. *)

  val rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
  (** [rectangle bk x y w h] adds to the current path of [bk] a
      rectangle whose lower left corner is at [(x,y)] and width
      and height are respectively [w] and [h]. *)

  val arc : t -> r:float -> a1:float -> a2:float -> unit
  (** [arc bk r a1 a2] add an arc to the current path starting from
      the current point with a radius [r], starting at angle [a1]
      and going clockwise to angle [a2]. *)

  val close_path : t -> unit
  (** Adds a line segment to the path from the current point to
      the beginning of the current sub-path (the most recent point
      passed to {!Archimedes.Backend.T.move_to}) and closes this
      sub-path. *)
  val clear_path : t -> unit
  (** Clears the current path. After this call there will be no
      path.  Nothing is guaranteed about the current point (it may
      not be preserved). *)
  val path_extents : t -> Matrix.rectangle

  val stroke : t -> unit
  (** [stroke bk] draw the curve described by the current path
      according to the current line width and color.  *)
  val stroke_preserve : t -> unit
  (** Same as {!stroke} but make sure the current path is unmodified. *)
  val fill : t -> unit
  (** [fill bk] draw the curve described by the current path according
      to the current line width and color.  The current path may be
      modified.  This is affected by the CTM.  *)
  val fill_preserve : t -> unit
  (** Same as {!fill} but make sure the current path is unmodified. *)

  val stroke_path_preserve : t -> Path.t -> unit
  (** [stroke_path bk p] stroke the abstract path [p], where its
      coordinates are interpreted in the current transformation
      matrix.  Of course, the current clipping, line width and color
      are be obeyed.  This function may modify the current path in
      [bk].

      For backend developers: the internal representation of the path
      is available in [Archimedes_internals.Path]. *)
  val fill_path_preserve : t -> Path.t -> unit
  (** [fill_path_preserve] is similar to [stroke_path_preserve] except
      that it fills the path. *)

  val show : t -> unit
  (** Some backends may not show immediately the action of {!stroke},
      {!fill}, {!stroke_path_preserve},... immediately (usually
      because it is expensive but also to avoid flicker during
      animations).  [show bk] forces the backend to update.  *)

  val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
  (** Establishes a new clip rectangle by intersecting the current
      clip rectangle.  This {i may clear} the current path.  Calling
      {clip_rectangle} can only make the clip region smaller, never
      larger.  For [clip_rectangle] to have only a local effect, put
      it in a {!save} / {!restore} group.

      [clip_rectangle] is garantee to respect the CTM only if the
      components [xy] and [yx] of the matrix are both [0.]. *)

  val save : t -> unit
  (** Save the current state of the backend.  Note that
      save/restore must not affect the current path. *)
  val restore : t -> unit
  (** Restore the saved state of the backend. *)

  val translate : t -> x:float -> y:float -> unit
  (** [translate cr tx ty] modifies the current transformation
      matrix by translating the user-space origin by ([tx],[ty]). *)
  val scale : t -> x:float -> y:float -> unit
  (** [scale sx sy] modifies the current transformation matrix by
      scaling the X and Y user-space axes by [sx] and [sy]
      respectively. *)
  val rotate : t -> angle:float -> unit
  (** Modifies the current transformation matrix by rotating the
      user-space axes by [angle] radians. *)
  val set_matrix : t -> Matrix.t -> unit
  (** Set the current transformation matrix which is the matrix
      transorming user to device coordinates. *)
  val get_matrix : t -> Matrix.t
  (** Return the current transformation matrix.  Modifying this
      matrix does not affect the matrix held in [t]. *)
  val flipy : t -> bool
  (** [true] iff this kind of device has its Y axis pointing
      downwards.
      FIXME: really needed ?  Beware that on some devices, the font
      display happens in the current coordinates. *)

  val select_font_face : t -> slant -> weight -> string -> unit
  (** [select_font_face t slant weight family] selects a family
      and style of font from a simplified description as a family
      name, slant and weight.  Family names are bakend dependent.
      Raise an exception if the face is not supported. *)
  val set_font_size : t -> float -> unit
  (** Set the scaling of the font. *)
  val text_extents : t -> string -> Matrix.rectangle
  (** Returns a rectangle whose width and height specify
      respectively the length and the height of the text. The x and
      y values give the lower bottom point of the rectangle as if
      the text was placed at the origin.*)
  val show_text : t -> rotate:float -> x:float -> y:float ->
    text_position -> string -> unit
      (** [show_text t angle x y pos txt] displays [txt] at the point
          ([x],[y]) as indicated by [pos].  The point ([x],[y]) is in
          the current coordinate system but the current transformation
          matrix will NOT be applied to the text itself.  [angle]
          indicates by how many radians the text must be rotated
          w.r.t. the x-axis (in the current coordinate system, assuming
          it is orthonormal) -- not all device support rotations of
          angles [<> 0.] (in device coordinates).  This is an immediate
          operation: no [stroke] nor [fill] are required (nor will have
          any effect).  *)
end

type error =
| Corrupted_dependency of string
| Non_loadable_dependency of string * Dynlink.error
| Nonexistent of string  (** Cannot find the backend in the directories *)
| Not_loadable of string * Dynlink.error
    (** Cannot load the backend because of the dynlink error. *)
| Not_registering of string (** Not applying the {!Backend.Register}
                                functor. *)

exception Error of error * string
(** Exception raised when a backend cannot be loaded. *)

include T

val make : ?dirs:string list -> string list -> float -> float -> t
(** [make backend width height] creates a new backend of the
    given dimensions.  The units of the dimensions are backend
    dependent.

    The first element of [backend] is the name (case insensitive) of
    the underlying engine.  It may be followed by one or several
    options.  For example, ["Graphics"] for the graphics backend or
    ["Cairo"; "PNG"; filename] for the Cairo backend, using a PNG
    surface to be saved to [filename].  The empty list selects the
    graphics backend. *)

val close : t -> unit
(** Close the handle.  For some backends, the output will not be
    complete until this function is called. *)

val height : t -> float
  (** Returns the width of the backend canvas. *)

val width : t -> float
  (** Returns the height of the backend canvas. *)


val registered: unit -> string list
  (** Return the list of registered (i.e. loaded) backends. *)

val available : dirs:string list -> string list
(** Return the list of available backends in the given directories. *)


(************************************************************************)
(** {2 Registering new modules} *)

module type Capabilities =
sig
  include T

  val name : string
    (** Name under which to register the backend. *)

  val make : options:string list -> float -> float -> t
    (** [create options width height] must creates a new handle of
        size [width]×[height] (in units proper to the module) on which
        the subsequent drawing functions operate.  [options] allows to
        pass options to the backend (this is backend specific). *)

  val close : options:string list -> t -> unit
  (** Close the handle.  This function will be given the options
      specified at backend creation so it can react appropriately if
      some final work need to be done for some of them. *)
end

module Register(B: Capabilities) : sig end
(** The {i side effect} of this functor application is to register
    the functions of the backend [B] under the name [B.name].

    A backend [B] must be declared in a file archimedes_[B.name]
    (compiled to a .cmo and/or .cmxs library) and the functor
    application must be executed as part of the initialisation code.
    We recommend the use of [let module U = Register(B) in ()] to
    perform the registration.  *)
