(* File: viewport.ml

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

module rec Axes : sig
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
= struct
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
    offset: offset;
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

  let default_axis () =
    { x0 = -1.; xend = 1.; auto_x0 = true; auto_xend = true;
      log = false; orientation = Positive; graph_axes = [] }

  let default_axes_system viewports =
    { x = default_axis ();
      y = default_axis ();
      viewports = viewports }
end
and Viewport : sig
  type t
  type coord_name = Device | Graph | Data | Orthonormal
  val get_coord_from_name : viewport -> coord_name -> Coordinate.t
  val init : ?lines:float -> ?text:float -> ?marks:float -> ?w:int -> ?h:int ->
    dirs:string list -> string -> viewport
  val make : ?lines:float -> ?text:float -> ?marks:float -> viewport ->
    coord_name -> float -> float -> float -> float -> viewport
end
= struct
  type t = {
    backend: Backend.t;

    (* (A,B,C,E) indicate "1" for a particular (see below) coordinate system
       A---------device--------+
       | B--------graph------+ |
       | C         Data      | |
       | +-C-----------------B |
       +------E----------------A
    *)
    mutable coord_device: Coordinate.t; (* AA *)
    mutable coord_graph: Coordinate.t; (* BB *)
    mutable coord_orthonormal: Coordinate.t; (* AE *)
    mutable coord_data: Coordinate.t; (* CC *)

    (* Axes system associated to the viewport *)
    mutable axes_system: Axes.t;
    (* For sizing texts, tics, etc. *)
    mutable sizes: Sizes.t;
    (* The last point drawn *)
    mutable current_point: float * float;
    (* An instruction is a "thing" to plot on the device, we memorize
       their order to replot in case of necessity *)
    mutable instructions: (unit -> unit) Queue.t;

    (* Draw immediately or wait for closing ? *)
    mutable immediate_drawing: bool
  }

  type coord_name = Device | Graph | Data | Orthonormal

  let def_lw, def_ts, def_ms = 0.002, 0.024, 0.01

  (* TODO: doc *)
  let usr_lw, usr_ts, usr_ms = 500., 500., 100.

  let init ?(lines=def_lw) ?(text=def_ts) ?(marks=def_ms) ?(w=640) ?(h=480) ~dirs backend_name =
    let backend = Backend.make ~dirs backend_name w h in
    let coord_root = Coordinate.make_root (Backend.get_matrix backend) in
    let size0 = min w h in
    let coord_device = Coordinate.make_identity coord_root in
    let coord_graph = Coordinate.make_scale (Coordinate.make_translate coord_device 0.1 0.1) 0.8 0.8 in
    let rec axes_system = Axes.default_axes_system [viewport]
    and viewport = {
      backend = backend;
      coord_device = coord_device; coord_graph = coord_graph;
      coord_orthonormal = Coordinate.make_scale coord_device (size0 /. w) (size0 /. h);
      (* We don't care; will be updated as soon as points are added or we change axes. *)
      coord_data = Coordinate.make_identity coord_graph;
      axes_system = axes_system;
      sizes = Sizes.make (Sizes.make_root size0 size0 1.) lines text marks;
      current_point = (0., 0.);
      instructions = Queue.create ();
      immediate_drawing = false;
    } in
    viewport

  let get_coord_from_name vp = function
    | Device -> vp.coord_device
    | Graph -> vp.coord_graph
    | Data -> vp.coord_data
    | Orthonormal -> vp.coord_orthonormal

  let make ?(axes_sys=false) ?(lines=def_lw) ?(text=def_ts) ?(marks=def_ms) vp coord_name xmin xmax ymin ymax =
    let w, h, size0 =
      let xmax', ymax' = Coordinate.to_device coord xmax ymax
      and xmin', ymin' = Coordinate.to_device coord xmin ymin in
      let w = xmax' -. xmin' and h = ymax' -. ymin' in
      w, h, min w h
    in
    let coord_parent = get_coord_from_name vp coord_name in
    let coord_device = Coordinate.make_translation
      (Coordinate.make_scale coord_parent (xmax -. xmin) (ymax -. ymin)) xmin ymin in
    let coord_graph = Coordinate.make_scale (Coordinate.make_translate coord_device 0.1 0.1) 0.8 0.8 in
    let rec viewport = {
      backend = vp.backend;
      coord_device = coord_device; coord_graph = coord_graph;
      coord_orthonormal = Coordinate.make_scale coord_device (size0 /. w) (size0 /. h);
      (* We don't care; will be updated as soon as points are added or we change axes. *)
      coord_data = Coordinate.make_identity coord_graph;
      axes_system =
        if axes_sys then vp.axes_system
        else axes_system = Axes.default_axes_system [viewport];
      sizes = Sizes.make_rel vp.sizes lines text marks;
      current_point = (0., 0.);
      instructions = [];
      immediate_drawing = false;
    } in
    viewport

  let do_instructions vp = ()

(*  let close vp =
    let parent = vp.coord_device.Coordinate.parent in
    parent.children <- List.filter (( <> ) vp) parent.children
    (* TODO do we need to do instructions ? *)
    if parent == parent.Coordinate.parent then begin
      do_instructions vp;
      Backend.close vp.backend*)

  let rows ?(axes_sys=false) vp n =
    let step = 1. /. (float n) in
    let f i =
      let y_min = float i *. step in
      make ~axes_sys:axes_sys vp Device 0. 1. y_min (y_min +. step) in
    Array.init n f

  let columns ?(axes_sys=false) vp n =
    let step = 1. /. (float n) in
    let f i =
      let x_min = float i *. step in
      make ~axes_sys:axes_sys vp Device x_min (x_min +. step) 0. 1. in
    Array.init n f


  let grid ?(axes_sys=false) vp n m =
    let stepx = 1. /. (float n)
    and stepy = 1. /. (float m) in
    let f i j =
      let x_min, y_min = float i *. step, float j *. step in
      make ~axes_sys:axes_sys vp Device x_min (x_min +. step)
        y_min (y_min +. step) in
    let make_row i = Array.init m (f i) in
    Array.init n make_row

(* ..........................................................................*)
  let set_line_width vp lw =
    let size =
      if lw <= 0. then def_lw *. vp.coord_orthonormal
      else lw /. usr_lw *. vp.coord_orthonormal in
    Size.set_abs_lw vp.size size

  let set_font_size vp ts =
    let size =
      if ts <= 0. then def_ts *. vp.coord_orthonormal
      else ts /. usr_ts *. vp.coord_orthonormal in
    Size.set_abs_ts vp.size size

  let set_mark_size vp ms =
    let size =
      if ms <= 0. then def_marks *. vp.coord_orthonormal
      else ms /. usr_ms *. vp.coord_orthonormal in
    Size.set_abs_ms vp.size size

  let set_rel_line_width vp lw =
    Size.set_rel_line_width vp.size (if lw <= 0. then 1. else lw)

  (* FIXME: Fix names text, font? *)
  let set_rel_font_size vp ts =
  Size.set_text_size vp.size (if ts <= Graphics0. then 1. else ts)

  let set_rel_mark_size vp ms =
  Size.set_rel_mark_size vp.size (if ms <= 0. then 1. else ms)

  let get_line_width vp =
    (Size.get_line_width vp.size) *. usr_lw /. vp.coord_orthonormal
  let get_font_size vp =
   (Size.get_text_size vp.size) *. usr_ts /. vp.coord_orthonormal
  let get_mark_size vp =
   (Size.get_mark_size vp.size) *. usr_ms /. vp.coord_orthonormal


(* ......................................................................... *)

  let lower_left_corner vp = Coordinate.to_device vp.coord_device (0., 0.)

  let upper_right_corner vp = Coordinate.to_device vp.coord_device (1., 1.)

  let dimensions vp =
    let x0, y0 = lower_left_corner vp
    and xend, yend = upper_right_corner vp in
    (xend -. x0, yend -. y0)

  let set_global_color vp color = Backend.set_color vp.backend



end

