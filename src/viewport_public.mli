(* File: viewport.mli

   Copyright (C) 2009-2011

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     Pierre Hauweele <antegallya@gmail.com>
     Noemie Meunier <noemie_6462@hotmail.com>
     Fabian Pijcke <fabian.pijcke@gmail.com>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

module type T = sig
  module rec Axes : sig
    type sign = Positive | Negative

    type axis = {
      mutable x0: float;     mutable auto_x0: bool;
      mutable xend: float;   mutable auto_xend: bool;
      mutable log: bool;
      mutable orientation: sign;
      mutable viewports: Viewport.t list
    }

    type t = {
      mutable x: axis;
      mutable y: axis;
    }
  end
  and Viewport : sig
    type t

    type coord_name = Device | Graph | Data | Orthonormal

    val get_coord_from_name : t -> coord_name -> Coordinate.t
      (** [get_coord_from_name viewport coord_name] returns one of the
          coordinate systems of the viewport *)

    (** {2 Create new viewports} *)
    val init : ?lines:float -> ?text:float -> ?marks:float ->
      ?w:float -> ?h:float -> dirs:string list -> string -> t
      (** [init backend_name] initializes the whole Archimedes stuff,
          returning a main viewport using the backend given

          @param lines the width of the lines (default: 1. corresponds to
          fullfilling a biggest square of the viewport with 500 lines)

          @param text the size of the text in (default: 12. corresponds to
          fullfilling a biggest square of the viewport with about 42 lines of
          text)

          @param marks the size of the marks in pixels (default:
          1. corresponds to fullfilling a biggest square the viewport with 100
          "lines of marks")

          @param w the width of the main viewport (in backend's unit)

          @param h the height of the main viewport (in backend's unit)

          @param dirs a list of output files, useful for backends supporting
          several output formats (e.g. ["mytest.eps", "tex/mytest.tex"])
      *)
    val make : ?axes_sys:bool -> ?lines:float -> ?text:float -> ?marks:float ->
      t -> coord_name -> float -> float -> float -> float ->
      (t -> float -> float -> unit) -> t
      (** [make parent coord_name xmin xmax ymin ymax] creates and returns a
          viewport on top of [parent] with top left corner (xmin, ymin) and
          bottom right corner (xmax, ymax) using parent's [coord_name]
          coordinate system.

          @param axes_sys should we draw the axis system ?

          @param lines see {!init}

          @param text see {!init}

          @param marks see {!init}
      *)

    val get_backend : t -> Backend.t

    val sync : ?x:bool -> ?y:bool -> t -> t -> unit
      (** [sync vp vp_base] synchronize the viewport vp with vp_base. The
          optionals ?x and ?y allows one to synchronize only the x axis or
          only the y axis

          @param x default true, set to false if you want to synchronize
          only the y-axis

          @param y default true, set to false if you want to synchronize
          only the x-axis
      *)

    val layout_grid : ?syncs:(bool * bool * bool * bool) -> ?axes_sys:bool ->
      t -> int -> int -> t array
      (** [layout_grid parent n_cols n_rows] creates [n_cols] * [n_rows]
          viewports layouted in a grid and returns them in an array of
          viewports

          @param syncs (cx, cy, rx, ry) should we synchronize the x axis
          along the columns ? The y axis alon the columns ? The x axis
          along the rows ? The y axis along the rows ?

          @param axes_sys see {!make}
      *)
    val layout_rows : ?syncs:(bool * bool) -> ?axes_sys:bool -> t ->
      int -> t array
      (** [layout_rows parent n_rows] creates [n_rows] viewports layouted in a
          column and returns them in an array of viewpors

          @param syncs the axes to synchronize (x, y)

          @param axes_sys see {!make}
      *)
    val layout_columns : ?syncs:(bool * bool) -> ?axes_sys:bool -> t ->
      int -> t array
      (** [layout_cols parent n_cols] creates [n_cols] viewports layouted in a
          row and returns them in an array of viewports

          @param syncs the axes to synchronize (x, y)

          @param axes_sys see {!make}
      *)
      (* not in public interface *)
      (*val fixed_left : ?axes_sys:bool -> float -> t -> viewport * viewport
        val fixed_right : ?axes_sys:bool -> float -> t -> viewport * viewport
        val fixed_top : ?axes_sys:bool -> float -> t -> viewport * viewport
        val fixed_bottom : ?axes_sys:bool -> float -> t -> viewport * viewport*)
    val layout_borders : ?north:float -> ?south:float -> ?west:float ->
      ?east:float -> ?axes_sys:bool -> t -> t * t * t * t * t
      (** [layout_borders parent] returns a 5-uple of viewports where the 4
          first viewports are fixed in size towards the center while the fifth
          one is extensible. The viewports are north, south, west, east, center
          and are placed conformally to their names.

          @param north the size of the north's viewport; if zero (default),
          this viewport is unused (the north viewport will be the same as
          the center one)

          @param south the size of the south's viewport; same behaviour as
          north if zero

          @param west the size of the west's viewport; same behaviour as
          north if zero

          @param east the size of the east's viewport; same behaviour as
          north if zero

          @axes_sys see {!make}
      *)

    val ortho_from : t -> coord_name -> float * float -> float * float
    val data_from : t -> coord_name -> float * float -> float * float

    val set_line_width : t -> float -> unit
    val set_font_size : t -> float -> unit
    val set_mark_size : t -> float -> unit
    val set_rel_line_width : t -> float -> unit
    val set_rel_font_size : t -> float -> unit
    val set_rel_mark_size : t -> float -> unit
    val get_color : t -> Color.t
    val get_line_width : t -> float
    val get_font_size : t -> float
    val get_mark_size : t -> float

    val lower_left_corner  : t -> float * float
      (** The device's coordinates of the viewport's lower left corner *)
    val upper_right_corner : t -> float * float
      (** The device's coordinates of the viewport's upper right corner *)
    val dimensions : t -> float * float
      (** The device's width and height of the viewport *)

    (* set_global_param set param of backend and then of all viewports *)
    val set_color : t -> Color.t -> unit
    val set_global_line_cap : t -> Backend.line_cap -> unit
    val set_global_dash : t -> float -> float array -> unit
    val set_global_line_join : t -> Backend.line_join -> unit
      (*  val get_global_line_width : t -> float*)
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
    val stroke_preserve : ?path:Path.t -> t -> coord_name -> unit
      (** strokes the path (default: viewport's path) on the specified
          coordinate system, doesn't clear the viewport's path if no path
          given *)
    val stroke : ?path:Path.t -> t -> coord_name -> unit
      (** strokes the path (default: viewport's path) on the specified
          coordinate system, does clear the viewport's path if no path given *)
    val fill_preserve : ?path:Path.t -> t -> coord_name -> unit
    val fill : ?path:Path.t -> t -> coord_name -> unit
    val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
      (*    val save_vp : t -> unit
            val restore_vp : t -> unit*)
    val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit
    val show_text :
      t -> coord_name ->
      ?rotate:float ->
      x:float -> y:float -> Backend.text_position -> string -> unit
      (*  val text_extents : t -> string -> rectangle*)
    val mark : t -> x:float -> y:float -> string -> unit
      (* val mark_extents : t -> string -> rectangle *)

    (* TODO Complete *)
    (*  val fx
        val xy
        val box_axes
        val centered_axes*)

    val xrange : t -> float -> float -> unit
    val yrange : t -> float -> float -> unit

    val xmin : t -> float
    val xmax : t -> float
    val ymin : t -> float
    val ymax : t -> float

    val xlog : t -> bool
    val ylog : t -> bool
    val set_xlog : t -> bool -> unit
    val set_ylog : t -> bool -> unit

    val set_line_width_direct : t -> float -> unit -> unit
    val set_font_size_direct : t -> float -> unit -> unit
    val set_mark_size_direct : t -> float -> unit -> unit
    val set_rel_line_width_direct : t -> float -> unit -> unit
    val set_rel_font_size_direct : t -> float -> unit -> unit
    val set_rel_mark_size_direct : t -> float -> unit -> unit
    val set_color_direct : t -> Color.t -> unit -> unit
    val set_line_cap_direct : t -> Backend.line_cap -> unit -> unit
    val set_dash_direct : t -> float -> float array -> unit -> unit
    val set_line_join_direct : t -> Backend.line_join -> unit -> unit
    val stroke_direct : ?path:Path.t -> t -> coord_name -> unit -> unit
    val fill_direct : ?path:Path.t -> t -> coord_name -> unit -> unit
    val clip_rectangle_direct : t -> x:float -> y:float -> w:float ->
      h:float -> unit -> unit
    val select_font_face_direct : t -> Backend.slant -> Backend.weight ->
      string -> unit -> unit
    val show_text_direct : t -> coord_name -> ?rotate:float ->
      x:float -> y:float -> Backend.text_position -> string -> unit -> unit
    val mark_direct : t -> x:float -> y:float -> string -> unit -> unit
    val save_direct : t -> unit -> unit
    val restore_direct : t -> unit -> unit

    val close : t -> unit

    val add_instruction : (unit -> unit) -> t -> unit
    val do_instructions : t -> unit

    val auto_fit : t -> float -> float -> float -> float -> unit
      (** [auto_fit vp x0 y0 x1 y1] ensures that the rectangle delimited by
          (x0, y0) and (x1, y1) is included into the axes' ranges *)

    val save : t -> unit
    val restore : t -> unit
  end
end
