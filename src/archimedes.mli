(** A 2D plotting library with various backends. *)


(** Representation of colors.*)
module Color :
sig
  type t
    (**The type for colors*)

  val make : ?a:float -> float -> float -> float -> t
    (**[color ~a r g b] creates the color with transparency [~a], red
       component [r], green component [g] and blue component [b]. All values
       must be between [0.] and [1.]; raises [Invalid_argument] otherwise.*)

  val r : t -> float
    (**Returns the red component of a color.*)

  val g : t -> float
    (**Returns the green component of a color.*)

  val b : t -> float
    (**Returns the blue component of a color.*)

  val a : t -> float
    (**Returns the transparency (alpha) component of a color.*)

  val get_rgb : t -> float * float * float
    (**Equivalent to ([r t],[g t],[b t]).*)

  val get_rgba : t -> float * float * float * float
    (**Equivalent to ([r t],[g t],[b t], [a t]).*)

  val black : t
  val red : t
  val green : t
  val blue : t
  val yellow : t
  val magenta : t
  val cyan : t
  val white : t
  (**Predefined colors.*)

  (** {2 Merging colors} *)

  (** Different ways of merging colors.  See
      http://cairographics.org/operators/ for more explanations.*)
  type operator =
    | Over (** Transparency and color components are mixed in such a way
               that it corresponds to putting the second color over the first*)
    | Source (** First color completely ignored. *)
    | Clear (** Inhibits all colors *)
    | In (** RGB components as the second color, A component product of
             the two A components. So, a transparent color result if the
             first one was transparent.*)
    | Out (** RGB components as the second color, A component product of
              the second A component with (1 - A) first component. So, a
              transparent color result if the first one was opaque.*)
    | Atop (** Transparency of the first color is the final transparency;
               mixes RGB components.*)
    | Dest (** Second color completely ignored. (<-> SOURCE)*)
    | Dest_Over (** Transparency and color components are mixed in such a
                    way that it corresponds to putting the first color over the
                    second. (<-> OVER)*)
    | Dest_In (** RGB components as the first color, A component product of
                  the two A components. So, a transparent color result if the
                  second one was transparent. (<-> IN)*)
    | Dest_Out (** RGB components as the first color, A component product
                   of the first A component with (1 - A) second
                   component. So, a transparent color result if the
                   second one was opaque. (<-> OUT)*)
    | Dest_Atop (** Transparency of the second color is the final transparency;
                    mixes RGB components. (<-> ATOP)*)
    | Xor (** Same mix of color than OVER, but transparency will be more
              important.*)
    | Add (** RGB components: ponderated sum of RGB components, with
              transparency. Resulting A is the sum of transparencies
              (bounded to 1. if necessary).*)
    | Saturate (** Same as ADD, but the sum for RGB components shrinks
                   the ponderation the first color components (coeff:
                   min (first A, 1 - second A)) *)


  val add : ?op:operator -> t -> t -> t
    (**Adds the first color to the second color, according to the
       operator [op] (default : [Over]).*)
end

(************************************************************************)
(** {2 Plotting functions} *)

(** Axes maker and convenient ways to create axes. *)
module Axes :
sig

  (** A data structure holding ranges.*)
  type ranges = { x1:float; x2:float; y1:float; y2:float}

  type 'a axis
    (** This type stores all information about an axis: major, minor
        tics, their positioning and how to position labels relative to
        tics. *)

  type ('a, 'b) t
    (** This type stores information about a pair of axes. *)

  (** Different axes modes. They all specify which point has to be
      taken into account for the intersection of the axes. This point
      determine the position of tics. *)
  type axes =
      [ `None of bool * bool
          (**No axis will be printed. The bools have to be interpreted
             this way:

             -for the first one, [true] means: minimal abscissa;
             [false]: maximal abscissa.

             -for the second one, same meaning, but on ordinates.*)
      | `Rectangle of bool * bool
          (**A rectangle, whose corners are taken so that it fits the
             zone. The bools have the same meaning as for [`None].*)
      | `Two_lines of float * float
          (**Abscissas axes are represented in a horizontal line,
             ordinates in a vertical line. The pair of floats is
             precisely the intersection of these two lines.*)
      | `Two_lines_rel of float * float
          (**Same as [Two_lines] except that the two floats are
             relative; that is, if [(t,u)] is an argument pair, and
             [xmin, ..., ymax] are the bounds, then the intersection
             is computed as [(xmin +. t *.(xmax -. xmin), ymin +. u
             *. (ymax -. ymin) )].*)
      ]

  (** Type of data to put as labels on major tics. *)
  type data = [
  | `Text_label of string array * float
      (** Labels will be text labels, rotated by the second argument. *)
  | `Number
      (** Use abscissas or ordinates as labels. *)
  | `Expnumber
      (** Labels of the form [10^x] with [x] abscissa or ordinate. *)
  ]

  type tic = [ `P of string ]
      (**Type for tics.*)

  type loc_tics =
      [ `Fixed_rel of (float * bool) list
        (**List of pairs [(x, major)] with [x] a number between 0 and
           1, specifying the relative position of the tic and [major]
           indicating whether the tic is major.*)
      | `Fixed_abs of ((float * bool) list)
      | `Linear_variable of int array
        (**The [i]th element of the array specifies the number of
           minor tics between the [i]th major tic and the [i+1]th
           one (starting count at 0). All tics are placed linearly;
           that is, if the length of the axis is [len], then the
           [i]th tic (among all tics, starting to count at 0) is
           placed at distance [i /. len].*)
      | `Linear of int * int
        (**[`Linear(majors, minors)]: Fixed number of major tics,
           and number of minor tics between two consecutive major
           tics. They are all placed linearly.*)
      | `Logarithmic of int * int
        (**Same as [`Linear] except that the minor tics are placed
           in a logarithmic scale.*)
      | `Auto_linear
      ]


end

(************************************************************************)
(** {2 Affine transformations} *)

(** Module implementing affine transformations and various operations
    on them. *)
module Matrix :
sig

  (** Holds an affine transformation, such as a scale, rotation, shear,
      or a combination of those. The transformation of a point (x, y) is
      given by:
      {[
      x_new = xx *. x +. xy *. y +. x0;
      y_new = yx *. x +. yy *. y +. y0;      ]} *)
  type t = { mutable xx: float; mutable yx: float;
             mutable xy: float; mutable yy: float;
             mutable x0: float; mutable y0: float; }

  exception Not_invertible

  val make_identity : unit -> t
    (** [make_identity()] returns the identity transformation. *)

  val make_translate : x:float -> y:float -> t
    (** [make_translate tx ty] returns a transformation that translates
        by [tx] and [ty] in the X and Y dimensions, respectively. *)

  val make_scale : x:float -> y:float -> t
    (** [make_scale sx sy] returns a transformation that scales by [sx]
        and [sy] in the X and Y dimensions, respectively. *)

  val make_rotate : angle:float -> t
    (** [make_rotate radians] returns a transformation that rotates
        by [radians]. *)

  val set_to_identity : t -> unit
    (** Sets the current transformation to the identity transformation. *)

  val copy: t -> t
    (** [copy matrix] returns a copy of [matrix]. *)

  val blit : t -> t -> unit
    (** [blit m1 m2] copies the content of [m1] into [m2]. *)

  val translate : t -> x:float -> y:float -> unit
    (** [translate m tx ty] applies a translation by [tx], [ty] to the
        transformation in [m].  The effect of the new transformation
        is to {i first} translate the coordinates by [tx] and [ty],
        then apply the original transformation to the coordinates. *)

  val scale : t -> x:float -> y:float -> unit
    (** [scale m sx sy] applies scaling by [sx], [sy] to the
        transformation in [m].  The effect of the new transformation
        is to {i first} scale the coordinates by [sx] and [sy], then
        apply the original transformation to the coordinates. *)

  val rotate : t -> angle:float -> unit
    (** [rotate m radians] applies rotation by [radians] to the
        transformation in [m].  The effect of the new transformation
        is to {i first} rotate the coordinates by [radians], then
        apply the original transformation to the coordinates. *)

  val invert : t -> unit
    (** [invert m] changes [matrix] to be the inverse of it's original
        value.  Not all transformation matrices have inverses; if the
        matrix collapses points together (it is degenerate), then it
        has no inverse and this function will raise
        {!Matrix.Not_invertible}. *)

  val det : t -> float
    (** [det m] returns the determinant of the linear part of [m].  It
        is the (signed) area that gets the unit square after
        transformation.  *)

  val mul : t -> t -> t
    (** [multiply b a] multiplies the affine transformations in [a]
        and [b] together and return the result.  The effect of the
        resulting transformation is to {i first} apply the
        transformation in [a] to the coordinates and then apply the
        transformation in [b] to the coordinates.

        BEWARE that the order of the arguments is different from
        e.g. [Cairo.Matrix.multiply]. *)

  val mul_in : t -> t -> t -> unit
    (** [mul_in c b a] computes [mul b a] and put the result in [c]. *)

  val transform_point : t -> x:float -> y:float -> float * float
    (** [transform_point m x y] transforms the point ([x], [y]) by [m]. *)

  val transform_distance : t -> dx:float -> dy:float -> float * float
    (** [transform_distance m dx dy] transforms the distance vector
        ([dx],[dy]) by [m].  This is similar to
        {!Matrix.transform_point} except that the translation
        components of the transformation are ignored.  The calculation
        of the returned vector is as follows:
        {[
        dx2 = dx1 * xx + dy1 * xy;
        dy2 = dx1 * yx + dy1 * yy;
        ]}
        Affine transformations are position invariant, so the same
        vector always transforms to the same vector.  If (x1,y1)
        transforms to (x2,y2) then (x1+dx1,y1+dy1) will transform to
        (x2+dx2,y2+dy2) for all values of dx1 and dy1.  *)

  val inv_transform_point : t -> x:float -> y:float -> float * float
    (** Makes the inverse transformation of a point. *)

  val inv_transform_distance : t -> dx:float -> dy:float -> float * float
    (** Makes the inverse transformation of a distance. *)

  val has_shear: t -> bool
    (** Tests whether the transformation has shears.  This is also the
        case if the transformation does a rotation.  *)

  (** A data structure for holding a rectangle. *)
  type rectangle = {
    x:float;   (** X coordinate of the left side of the rectangle *)
    y:float;   (** Y coordinate of the the top side of the rectangle  *)
    w:float;   (** width of the rectangle *)
    h:float;   (** height of the rectangle  *)
  }

  val transform_rectangle: ?dist_basepoint:bool -> t -> rectangle -> rectangle
    (** Transformation of rectangles. This returns the smallest
        rectangle containing the transformation of the rectangle argument
        by the matrix. The optional argument [dist_basepoint] has the
        following meaning:

        - Not specified: transform the base point as a point.
        - Specified as [true]: transform the base point as a distance.
        - Specified as [false]: no transformation of the base point.*)
end

(************************************************************************)
(** {2 Registering backends and extending the library} *)

(** Module managing the dynamic loading of the backends.  This modules
    is only useful to create new backends and should not be used for
    plotting data. *)
module Backend :
sig
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

  type slant = Upright | Italic
    (** Specifies variants of a font face based on their slant. *)

  type weight = Normal | Bold
    (** Specifies variants of a font face based on their weight. *)

  (** To be able to register a given backend, it must provide an
      implementation for all these functions. *)
  module type T =
  sig
    type t
      (** Handle to a backend. *)

    val set_color : t -> Color.t -> unit
    val set_line_width : t -> float -> unit
    val set_line_cap : t -> line_cap -> unit
    val set_dash : t -> float -> float array -> unit
    val set_line_join : t -> line_join -> unit

    val get_line_width: t -> float
    val get_line_cap: t -> line_cap
    val get_dash: t -> float array * float
    val get_line_join: t -> line_join

    val move_to : t -> x:float -> y:float -> unit
    val line_to : t -> x:float -> y:float -> unit
    val rel_move_to : t -> x:float -> y:float -> unit
    val rel_line_to : t -> x:float -> y:float -> unit

    val curve_to : t ->
      x1:float -> y1:float ->
      x2:float -> y2:float ->
      x3:float -> y3:float -> unit

    val rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit

    val arc : t -> r:float -> a1:float -> a2:float -> unit

    val close_path : t -> unit
      (** Adds a line segment to the path from the current point to
          the beginning of the current sub-path (the most recent point
          passed to {!Archimedes.Backend.T.move_to}) and closes this
          sub-path. *)
    val clear_path : t -> unit
      (** Clears the current path. After this call there will be no path.
          Nothing is guaranteed about the current point. *)
    val path_extents : t -> Matrix.rectangle

    val stroke : t -> unit
    val stroke_preserve : t -> unit
    val fill : t -> unit
    val fill_preserve : t -> unit

    val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
      (** Establishes a new clip rectangle by intersecting the current
          clip rectangle.  This {i may clear} the current path. *)

    val save : t -> unit
    val restore : t -> unit

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
          matrix should not affect the matrix held in [t]. *)
    val backend_to_device : t -> Matrix.t
      (** The returned matrix is the one which transforms the backend
          coordinates (those for which the origin is at the lower left
          corner of the surface, with unit square 1px x 1px) to the
          device coordinates (that is, the original coordinates which
          naturally come with the surface). *)

    val select_font_face : t -> slant -> weight -> string -> unit
      (** [select_font_face t slant weight family] selects a family
          and style of font from a simplified description as a family
          name, slant and weight.  Family names are bakend dependent.  *)
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
    | Non_loadable_dependency of string
    | Nonexistent of string  (** Cannot find the backend in the directories *)
    | Not_loadable of string * Dynlink.error
        (** Cannot load the backend because of the dynlink error. *)
    | Not_registering of string (** Not applying the {!Backend.Register}
                                    functor. *)

  val string_of_error : error -> string

  exception Error of error

  include T

  val make : ?dirs:string list -> string -> float -> float -> t
    (** [make backend width height] creates a new backend of the given
        dimensions.

        [backend] is the name of the underlying engine, followed by one
        or several options separated by spaces.  For example, "Graphics"
        for the graphics backend or "Cairo PNG filename" for the Cairo
        backend, using a PNG surface to be saved in [filename]. *)

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
          size [width]�[height] (in units proper to the module) on which
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
end

(** Affine systems of coordinates relative to other coordinate systems
    with automatic updates. *)
module Coordinate :
sig
  type t
    (** Mutable affine coordinate system. *)

  type ctm
    (** Current transformation matrix of the backend (to be able to
        restore it through {!Coordinate.restore}. *)

  val use : Backend.t -> t -> ctm
    (** After a call to [use b c], all backend operations will be
        performed in the corrdinates [c].  It returns the current
        coordinate system so one can restore it with
        {!Coordinate.restore}. *)

  val restore : Backend.t -> ctm -> unit
    (** [restore b c] restore the coordinate transformation matrix [ctm]
        for the backend [b]. *)


  (** {2 Transforming coordinates} *)

  val to_device : t -> x:float -> y:float -> float * float
    (**[to_device coord x y] returns the location of the point [(x,y)]
       in device coordinates.*)

  val to_device_distance : t -> dx:float -> dy:float -> float * float
    (**[to_device coord dx dy] returns the distance of [(dx,dy)]
       in device coordinates.*)

  val to_coord : t -> x:float -> y:float -> float * float
    (**[to_coord coord x y] converts the (device) point [(x,y)] into
       the corresponding point, expressed in [coord] coordinates.*)

  val to_coord_distance : t -> dx:float -> dy:float -> float * float
    (**[to_coord coord x y] converts the (device) distance [(dx,dy)] into
       the corresponding distance, expressed in [coord] coordinates.*)

  (** {2 Creating new coordinate systems} *)

  val make_identity : unit -> t
    (** Make a system of coordinates which, when used, amounts to plot
        in the "raw" device coordinates. *)

  val make_translate : t -> x:float -> y:float -> t
    (** [make_translate coord x y] defines a new coordinate system that
        consists in moving the origin of [coord] to the point [(x,y)]
        (understood as coordinates in the system [coord]).  If [coord]
        is modified, the new system will be updated as well. *)

  val make_scale : t -> x:float -> y:float -> t
    (** [make_scale coord x y] defines a new coordinate system that
        consists in dilating axis X and Y of [coord] by a factor of [x]
        and [y] respectively.  If [coord] is modified, the new system
        will be updated as well. *)

  val make_rotate : t -> angle:float -> t
    (** [make_rotate coord a] defines a new coordinate system that
        consists in rotating the axis X and Y of [coord] by [a] radians
        (assuming the axis of the system [coord] are orthonormal).  If
        [coord] is modified, the new system will be updated as well.  *)

  val make_from_transform : t -> Matrix.t -> t
    (** [make_from_transform coord tm] defines a new coordinate system
        that consists first in applying [tm] and then the tranformation
        in [coord].  In other words, [tm] is the affine transformation
        from the desired coordinate system to [coord].  If [coord] is
        modified, the new system will be updated as well. *)

  val copy : t -> t
    (** Returns a completely independent copy of the current coordinate
        system. *)


  (** {2 Modifying this coordinate system} *)

  val translate : t -> x:float -> y:float -> unit
    (** [translate coord x y] modifies the coordinate system [coord]
        translating its origin to the point [(x,y)] (understood as
        coordinates in the system [coord]). *)

  val scale : t -> x:float -> y:float -> unit
    (** [scale coord x y] modifies the coordinate system [coord]
        dilating its axis X and Y by a factor of [x] and [y]
        respectively. *)

  val rotate : t -> angle:float -> unit
    (** [rotate coord a] modifies the coordinate system [coord] rotating
        its axis X and Y by [a] radians (assuming the axis of the system
        [coord] are orthonormal). *)

  val transform : t -> Matrix.t -> unit
    (** [transform coord tm] modifies the coordinate system [coord]
        changing the transformation matrix to its parent (the one it was
        created from) to [tm]. *)


  (** {2 Monitoring coordinate systems for updates} *)

  type monitor
    (** Handle to monitor the updates to a coordinate system. *)

  val monitor : t -> monitor
    (** [monitor coord] creates a new monitor for changes to [coord]
        (initially not set). *)

  val reset : monitor -> unit
    (** [reset m] reset the monitor.  See {!Coordinate.changed}. *)

  val changed : monitor -> bool
    (** [changed m] tell whether the coordinate system [m] is attached
        to was updated (possibly because of one of the coordinate systems
        it (transitively) depends on was mofidied) since the last
        [reset]. *)
end

(** Module handling point styles and marks. *)
module Pointstyle :
sig
  exception Error of string
    (**Raised for undefined point styles.*)

  type name = string
    (** Point styles are identified by strings. *)

  val add : name:name -> (Backend.t -> unit) -> Matrix.rectangle -> unit
    (**[add name f extents] adds to the existing point styles, a new
       point style, referenced under the name [name]. This point style is
       made using the function [f]; the extents it takes is given by
       [extents]. The behaviour of adding a new point style whose name is
       already used by another is the same as the core [Map.S.add] (that
       is, the previous binding disappears).*)
end

