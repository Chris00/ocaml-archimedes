(* File: viewport.ml

   Copyright (C) 2009-2015

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     Pierre Hauweele <antegallya@gmail.com>
     Noémie Meunier <noemie_6462@hotmail.com>
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

module rec Sizes =
struct
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
	line_width = lw; text_size = ts; mark_size = ms } in
      parent.children <- sizes :: parent.children;
      sizes
end
and Axes =
struct
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

  type sign = Positive | Negative;

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
end
and Viewport = struct
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

  type Coord_name = Device | Graph | Data | Orthonormal

  let init ?(lines=0.002) ?(text=0.024) ?(marks=0.01) ?(w=640) ?(h=480) ~dirs backend_name =
    let backend = Backend.make ~dirs backend_name w h in
    let coord_root = Coordinate.make_root (Backend.get_matrix backend) in
    let size0 = min w h in
    let axis = {
      Axis.x0 = -1.; Axis.auto_x0 = true;
      Axis.xend = 1.; Axis.auto_xend = true;
      Axis.log = false;
      Axis.orientation = Axis.Positive;
      Axis.graph_axes = []
    } in
    let rec axes_system = {
      x = axis;
      y = {axis with Axis.x0 = axis.Axis.x0};
      viewports = [viewport]
    }
    and viewport = {
      backend = backend;
      coord_device = Coordinate.make_identity coord_root;
      coord_graph = Coordinate.make_scale (Coordinate.make_translate coord_root 0.1 0.1) 0.8 0.8;
      coord_orthonormal = Coordinate.make_scale coord_root (size0 /. w) (size0 /. h);
      (* We don't care; will be updated as soon as points are added or we change axes. *)
      coord_data = Coordinate.make_identity coord_root;
      axes_system = axes_system;
      sizes = Sizes.make (Sizes.make_root size0 size0 1.) lines text marks;
      current_point = (0., 0.);
      instructions = Queue.create ();
      immediate_drawing = false;
    } in
      viewport

  (* TODO *)
  let make ?(lines=0.002) ?(text=0.024) ?(marks=0.01) vp coord_name xmin xmax ymin ymax =
    let size0 =
      let xmax', ymax' = Coordinate.to_device coord xmax ymax in
      let xmin', ymin' = Coordinate.to_device coord xmin ymin in
	min (xmax' -. xmin') (ymax' -. ymin')
    in
    let axis = {
      Axis.x0 = -1.; Axis.auto_x0 = true;
      Axis.xend = 1.; Axis.auto_xend = true;
      Axis.log = false;
      Axis.orientation = Axis.Positive;
      Axis.graph_axes = []
    } in
    let rec axes_system = {
      x = axis;
      y = {axis with Axis.x0 = axis.Axis.x0};
      viewports = [viewport]
    }
    and viewport = {
      backend = backend;
      coord_device = coord_root;
      coord_graph = coord_root;
      coord_orthonormal = coord_root;
      coord_data = coord_root;
      axes_system = axes_system;
      sizes = {
        Sizes.line_width = Sizes.Absolute size0;
        Sizes.text_size = Sizes.Absolute size0;
        Sizes.mark_size = Sizes.Absolute 1. };
      current_point = (0., 0.);
      instructions = [];
      immediate_drawing = false;
    } in
    let root_sizes = {
      Sizes.line_width = Sizes.Rel_updated (lines, lines *. size0);
      Sizes.text_size = Sizes.Rel_updated (text, text *. size0);
      Sizes.mark_size = Sizes.Rel_updated (marks, marks) } in
      {real_root with sizes = root_sizes; parent = real_root}
end
