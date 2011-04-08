(* File: viewport.mli

   Copyright (C) 2009-2015

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

module rec Sizes : sig
  type size =
    | Absolute of float
    | Rel_not_updated of float
    | Rel_updated of float * float

  type t = {
    parent: t;
    children: t list;

    mutable line_width: size;
    mutable text_size: size;
    mutable mark_size: size
  }

  val make_root : size -> size -> size -> t
  val make : t -> size -> size -> size -> t
  val make_rel : t -> float -> float -> float -> t
  val make_abs : t -> float -> float -> float -> t
end
and Axes : sig
  type labels =
    | Text of string array * float
    | Number
    | Expnumber of float
    | Expnumber_named of float * string
    | Custom of (float -> string)

  type tics =
    | Fixed of float list
    | Fixed_norm of float list
    | Equidistants of int * int
    | Auto

  type sign = Positive | Negative

  type offset =
    | Relative of float
    | Absolute of float

  type graph_axis = {
    tics: tics;
    offset: float;
    tics_position: sign
  }

  type axis = {
    mutable x0: float;     mutable auto_x0: bool;
    mutable xend: float;   mutable auto_xend: bool;
    mutable log: bool;
    mutable orientation: sign;
    mutable graph_axes: graph_axis list
  }

  type t = {
    x: axis;
    y: axis;

    mutable viewports: Viewport.t list
  }

  val default_axis: unit -> axis
  val default_axes_system: Viewport.t list -> t
end
and Viewport : sig
  type t = {
    backend: Backend.t;
    mutable coord_device: Coordinate.t;
    mutable coord_graph: Coordinate.t;
    mutable coord_orthonormal: Coordinate.t;
    mutable coord_data: Coordinate.t;

    mutable axes_system: Axes.t;
    mutable sizes: Sizes.t;
    mutable current_point: float * float;
    mutable instructions: (unit -> unit) Queue.t;
    mutable immediate_drawing: bool
  }

  type coord_name = Device | Graph | Data | Orthonormal

  val get_coord_from_name : viewport -> coord_name -> Coordinate.t
  val init : ?lines:float -> ?text:float -> ?marks:float -> ?w:int -> ?h:int ->
    dirs:string list -> string -> viewport
  val make : ?lines:float -> ?text:float -> ?marks:float -> viewport ->
    coord_name -> float -> float -> float -> float -> viewport

  val rows : t -> int -> viewport array
  val columns : t -> int -> viewport array
  val grid : t -> int -> int -> viewport array array

  val set_line_width : t -> float -> unit
  val set_mark_size : t -> float -> unit
  val set_font_size : t -> float -> unit
  val set_rel_line_width : t -> float -> unit
  val set_rel_mark_size : t -> float -> unit
  val set_rel_font_size : t -> float -> unit
  val get_line_width : t -> float
  val get_mark_size : t -> float
  val get_font_size : t -> float

  val width : t -> float
  val height : t -> float
  val set_color : t -> Color.t -> unit
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
  val arc : t -> r:float -> a1:float -> a2:float -> unit
  val close_path : t -> unit
  val clear_path : t -> unit
    (*val path_extents : t -> rectangle*)
  val stroke_current : t -> unit
  val stroke_current_preserve : t -> unit
  val stroke : t -> unit
  val stroke_preserve : t -> unit
  val fill : t -> unit
  val fill_preserve : t -> unit
  val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
  val save_vp : t -> unit
  val restore_vp : t -> unit
  val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit
  val show_text :
    t ->
    rotate:float ->
    x:float -> y:float -> Backend.text_position -> string -> unit
    (*  val text_extents : t -> string -> rectangle*)
  val render : t -> string -> unit
    (* val mark_extents : t -> string -> rectangle *)

  (* TODO Complete *)
  val fx
  val xy
  val add_xaxis
  val add_yaxis
  val box_axes
  val centered_axes

  val xrange : t -> float -> float -> unit
  val yrange : t -> float -> float -> unit

  val xmin : t -> float
  val xmax : t -> float
  val ymin : t -> float
  val ymax : t -> float


  val close : viewport -> unit

  val do_instructions : viewport -> unit
end
