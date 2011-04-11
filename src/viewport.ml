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
= struct
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

  let make_root lw ts ms =
    let rec root =
      { parent = root;
        children = [];
        line_width = lw; text_size = ts; mark_size = ms }
    in
    root

  let make parent lw ts ms =
    let sizes =
      { parent = parent;
        children = [];
        line_width = lw;
        text_size = ts;
        mark_size = ms } in
    parent.children <- sizes :: parent.children;
    sizes

  let make_rel parent lw ts ms =
    make parent (Rel_not_updated lw) (Rel_not_updated ts) (Rel_not_updated ms)

  let make_abs parent lw ts ms =
    make parent (Absolute lw) (Absolute ts) (Absolute ms)
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

  let default_axis () =
    { x0 = -1.; xend = 1.; auto_x0 = true; auto_xend = true;
      log = false; orientation = Positive; graph_axes = [] }

  let default_axes_system viewports =
    { x = default_axis ();
      y = default_axis ();
      viewports = viewports }
end
and Viewport : sig
  type t = {
    backend: Backend.t;
    mutable coord_device: Coordinate.t; (* AA *)
    mutable coord_graph: Coordinate.t; (* BB *)
    mutable coord_orthonormal: Coordinate.t; (* AE *)
    mutable coord_data: Coordinate.t; (* CC *)

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
end
= struct
  type t = {
    backend: Backend.t;

    parent: t;
    mutable children: t list;

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
    mutable immediate_drawing: bool;

    (* Viewport' position; in _NORMALIZED_ values *)
    mutable x: float;
    mutable y: float;
    mutable w: float;
    mutable h: float;

    redim: float -> float -> unit;
  }

  type coord_name = Device | Graph | Data | Orthonormal

  let init ?(lines=0.002) ?(text=0.024) ?(marks=0.01) ?(w=640) ?(h=480) ~dirs backend_name =
    let backend = Backend.make ~dirs backend_name w h in
    let coord_root = Coordinate.make_root (Backend.get_matrix backend) in
    let size0 = min w h in
    let coord_device = Coordinate.make_identity coord_root in
    let coord_graph = Coordinate.make_scale (Coordinate.make_translate coord_device 0.1 0.1) 0.8 0.8 in
    let rec axes_system = Axes.default_axes_system [viewport]
    and viewport = {
      backend = backend;
      parent = viewport;
      children = [];
      coord_device = coord_device; coord_graph = coord_graph;
      coord_orthonormal = Coordinate.make_scale coord_device (size0 /. w) (size0 /. h);
      (* We don't care; will be updated as soon as points are added or we change axes. *)
      coord_data = Coordinate.make_identity coord_graph;
      axes_system = axes_system;
      sizes = Sizes.make (Sizes.make_root size0 size0 1.) lines text marks;
      current_point = (0., 0.);
      instructions = Queue.create ();
      immediate_drawing = false;
      x = 0.; y = 0.; w = 1.; h = 1.;
      redim = (fun _ _ -> ());
    } in
    viewport

  let get_coord_from_name vp = function
    | Device -> vp.coord_device
    | Graph -> vp.coord_graph
    | Data -> vp.coord_data
    | Orthonormal -> vp.coord_orthonormal

  let make ?(lines=0.002) ?(text=0.024) ?(marks=0.01) vp coord_name xmin xmax ymin ymax =
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
      axes_system = Axes.default_axes_system [viewport];
      sizes = Sizes.make_rel vp.sizes lines text marks;
      current_point = (0., 0.);
      instructions = [];
      immediate_drawing = false;
    } in
    viewport

  let is_nan_or_inf (x:float) = x <> x || 1. /. x = 0.

  let ensure_point_visible vp x y =
    if vp.axes_system.x.auto_x0 then
      

  let do_instructions vp = ()

  let close vp =
    let parent = vp.coord_device.Coordinate.parent in
    parent.children <- List.filter (( <> ) vp) parent.children
    (* TODO do we need to do instructions ? *)
    if parent == parent.Coordinate.parent then begin
      do_instructions vp;
      Backend.close vp.backend
    end
end
