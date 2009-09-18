
(** Plotting various datatypes. *)
module type T = sig

  type t
    (** Handle to an Archimedes' plotting surface. *)

  type line_cap =
    | BUTT  (** start(stop) the line exactly at the start(end) point *)
    | ROUND (** use a round ending, the center of the circle is the endpoint *)
    | SQUARE (** use squared ending, the center of the square is the endpoint *)


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

  (************************************************************************)
  (** Functions common to all datatype submodules. *)
  module type COMMON = sig

    val make : ?dirs:string list -> string -> float -> float -> t
      (** [make backend w h] creates a new plotting surface of width
          [w] and height [h] for the specified backend.  The units for
          [w] and [h] are backend dependent. *)

    val close : t -> unit
      (** [close p] closes the plotting handle [p]. *)


    val set_line_width : t -> float -> unit
    val set_mark_size : t -> float -> unit
    val set_color : t -> Color.t -> unit

    (** {3 Plotting} *)

    val f : t -> ?color: Color.t -> ?nsamples: int -> ?mark:string ->
      (float -> float) -> float -> float -> unit

    val xyf : t -> ?color: Color.t -> ?nsamples: int -> ?mark:string ->
      (float -> float * float) -> float -> float -> unit


    (** {3 Text} *)

    val set_font_size : t -> float -> unit

    val text : t -> ?rotate:float ->
      x:float -> y:float -> ?pos:text_position -> string -> unit
      (** [text p x y txt] draw the text [txt] at the position of
          coordinates [x], [y].

          @param rotate the angle (in the current coordinates) of
          rotation of the text.  Default: [0.].
          @param pos the position of the text relative to the point
          [(x,y)].  Default: [CC]. *)


    (** {3 Viewports} *)

    type viewport
      (** A viewport is a rectangle on the backend surface where plots
          will take place. *)

    module Viewport : sig
      val make : t -> xmin:float -> xmax:float -> ymin:float -> ymax:float
        -> viewport

      val columns : t -> int -> viewport array
        (** [columns n] creates [n] viewports by subdividing the
            surface in [n] columns of equal width. *)
      val rows : t -> int -> viewport array
        (** [rows n] creates [n] viewports by subdividing the surface
            in [n] rows of equal height. *)
      val grid : t -> int -> int -> viewport array array
        (** [grid c r] creates [c * r] viewports by subdividing the
            surface in [c] columns and [r] rows. *)
    end

    val viewport : viewport -> unit
      (** [viewport vp] uses the viewport [vp] (for Archimedes' handle
          given to {!Archimedes.Plot.Viewport.make}). *)
  end

  module Array : sig
    include COMMON
  end

  module List : sig
    include COMMON

  end

  module Fortran : sig
    include COMMON

  end

  module C : sig
    include COMMON

  end

  module Generic : sig
    include COMMON

    val x : t -> ?color: Color.t -> ?mark:string -> ?n0:int ->
      iter:((float -> unit) -> 'a -> unit) -> 'a -> unit

    val xy : t -> ?color: Color.t -> ?mark:string ->
      iter:((float -> float -> unit) -> 'a -> unit) -> 'a -> unit

  end
end
