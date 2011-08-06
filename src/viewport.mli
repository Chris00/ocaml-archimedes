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


(** Area on which graphs can be made. *)

type t
(** Viewport handle. *)

type coord_name = Device | Graph | Data | Orthonormal

val get_coord_from_name : t -> coord_name -> Coordinate.t
(** [get_coord_from_name viewport coord_name] returns one of the
    coordinate systems of the viewport *)

(** {2 Create new viewports} *)

val make : ?lines:float -> ?text:float -> ?marks:float ->
  t -> coord_name -> float -> float -> float -> float ->
  (t -> float -> float -> unit) -> t
(** [make parent coord_name xmin xmax ymin ymax] creates and returns a
    viewport on top of [parent] with top left corner (xmin, ymin) and
    bottom right corner (xmax, ymax) using parent's [coord_name]
    coordinate system.

    @param lines see {!init}

    @param text see {!init}

    @param marks see {!init}
*)

val get_backend : t -> Backend.t
(** [get_backend vp] returns the backend associated to [vp], if vp is
    built over another viewport, the same backend is used. *)

val desync_ratio : t -> unit
(** [desync_ratio vp] make [vp] single. The ratio used will be the one
    used before desync. *)

val sync_ratio : t -> t -> unit
(** [sync_ratio vp vp_base] synchronizes [vp]'s ratio with the
    [vp_base]'s one. *)

val desync_range : ?x:bool -> ?y:bool -> t -> unit
(** [desync_range vp] make [vp] single. The range used will be the one
    used before desync.

    @param x desync the x axis (default: true)

    @param y desync the y axis (default: true)
*)

val sync_range : ?x:bool -> ?y:bool -> t -> t -> unit
(** [sync_range vp vp_base] synchronizes [vp]'s ranges (according
    to ?x and ?y params) with the ranges of [vp_base]. The range
    consists of a xmin and a xmax values, which defines the bounds of
    the viewport in Coordinate data.

    @param x sync the x axis (default: true)

    @param y sync the y axis (default: true)
*)

val desync_unit_size : ?x:bool -> ?y:bool -> t -> unit
(** [desync_unit_size vp] make [vp] single. The unit size used will be
    the one used before desync.

    @param x desync the x axis (default: true)

    @param y desync the y axis (default: true)
*)

val sync_unit_size : ?x:bool -> ?y:bool -> t -> t -> unit
(** [sync_unit_size vp vp_base] synchronizes [vp]'s unit sizes
    (according to ?x and ?y params) with the sizes of [vp_base].

    @param x sync the x axis (default: true)

    @param y sync the y axis (default: true)
*)

val sync : ?x:bool -> ?y:bool -> t -> t -> unit

val grid : ?syncs:(bool * bool * bool * bool) -> t -> int -> int -> t array array
(** [grid parent nx ny] returns [vp] an array of [nx] * [ny]
    sub-viewports of [parent]  arranged in a grid of [nx] columns and
    [ny] rows.  The bottom left viewport is [vp.(0).(0)], the one to
    its right (resp. abobve) is [vp.(1).(0)] (resp. [vp.(0).(1)]).

    @param syncs (cx, cy, rx, ry) where [cx] (resp. [cy]) says whether
    to synchronize the X-axis (resp. the [Y-axis]) along the columns
    and [rx] (resp. [ry]) says whether to synchronize the X-axis
    (resp. the Y-axis) along the rows.  Default: all [false].
*)

val rows : ?syncs:(bool * bool) -> t -> int -> t array
(** [rows parent ny] returns [vp] an array of [ny] viewports arranged
    in a column, the bottom one being [vp.(0)].

    @param syncs the axes to synchronize (x, y).  Default: both [false].
*)

val columns : ?syncs:(bool * bool) -> t -> int -> t array
(** [colimns parent nx] creates [n_cols] viewports layouted in a
    row and returns them in an array of viewports

    @param syncs the axes to synchronize (x, y)
*)
(* not in public interface *)
(*val fixed_left : ?axes_sys:bool -> float -> t -> viewport * viewport
  val fixed_right : ?axes_sys:bool -> float -> t -> viewport * viewport
  val fixed_top : ?axes_sys:bool -> float -> t -> viewport * viewport
  val fixed_bottom : ?axes_sys:bool -> float -> t -> viewport * viewport*)
val layout_borders : ?north:float -> ?south:float -> ?west:float ->
  ?east:float -> t -> t * t * t * t * t
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
val curve_to : t ->
  x1:float -> y1:float -> x2:float -> y2:float -> x3:float -> y3:float -> unit
val rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
val arc : t -> r:float -> a1:float -> a2:float -> unit
val close_path : t -> unit
val clear_path : t -> unit
(*val path_extents : t -> rectangle*)
val stroke_preserve : ?path:Path.t -> ?fit:bool -> t -> coord_name -> unit
(** strokes the path (default: viewport's path) on the specified
    coordinate system, doesn't clear the viewport's path if no path
    given *)
val stroke : ?path:Path.t -> ?fit:bool -> t -> coord_name -> unit
(** strokes the path (default: viewport's path) on the specified
    coordinate system, does clear the viewport's path if no path given *)
val fill_preserve : ?path:Path.t -> ?fit:bool -> t -> coord_name -> unit
val fill : ?path:Path.t -> ?fit:bool -> t -> coord_name -> unit
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

val axes_ratio : t -> float -> unit
(** [axes_ratio vp ratio] forces axes to keep [ratio] ([w / h]). *)
val xrange : t -> float -> float -> unit
val yrange : t -> float -> float -> unit
val xlabel : t -> string -> unit
val ylabel : t -> string -> unit
val title : t -> string -> unit

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


val add_instruction : (unit -> unit) -> t -> unit
val do_instructions : t -> unit

val auto_fit : t -> float -> float -> float -> float -> unit
(** [auto_fit vp x0 y0 x1 y1] ensures that the rectangle delimited by
    (x0, y0) and (x1, y1) is included into the axes' ranges *)

val fit : t -> Matrix.rectangle -> unit
(** [fit vp r] ensures that the rectangle [r] is included into the
    axes ranges. *)

val save : t -> unit
val restore : t -> unit

(**/**)

val init : ?lines:float -> ?text:float -> ?marks:float ->
  ?bg:Color.t -> ?w:float -> ?h:float -> ?dirs:string list -> string list -> t
(** See archimedes_footer.mli *)

val close : t -> unit
(** See archimedes_footer.mli *)
