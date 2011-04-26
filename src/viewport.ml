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
  val make : ?axes_sys:bool -> ?lines:float -> ?text:float -> ?marks:float -> viewport ->
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
    mutable immediate_drawing: bool;

    (* Redimension function: when the parent viewport grows of x and y,
       what should subsequent coordinates become ? This function must set
       them correctly *)
    redim: float -> float -> unit;
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
      redim = (fun _ _ -> ());
    } in
    viewport

  let get_coord_from_name vp = function
    | Device -> vp.coord_device
    | Graph -> vp.coord_graph
    | Data -> vp.coord_data
    | Orthonormal -> vp.coord_orthonormal

  let make ?(axes_sys=false) ?(lines=def_lw) ?(text=def_ts) ?(marks=def_ms) vp coord_name xmin xmax ymin ymax redim =
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
      redim = redim;
    } in
    viewport

  let is_nan_or_inf (x:float) = x <> x || 1. /. x = 0.

  let initial_scale = 1.

  let auto_fit vp x y =
    let xaxis = vp.axes_system.x
    and yaxis = vp.axes_system.y in
    let updated = ref false in
    if xaxis.auto_x0 then
      if is_nan_or_inf xaxis.x0 || x < xaxis.x0 then
        (xaxis.x0 <- x; updated := true);
    if xaxis.auto_xend then
      if is_nan_or_inf xaxis.xend || x > xaxis.xend then
        (xaxis.xend <- x; updated := true);
    if yaxis.auto_x0 then
      if is_nan_or_inf yaxis.x0 || y < yaxis.x0 then
        (yaxis.x0 <- y; updated := true);
    if yaxis.auto_xend then
      if is_nan_or_inf yaxis.xend || y > yaxis.xend then
        (yaxis.xend <- y; updated := true);
    if !updated then begin
      let scalx, tr_x =
        if xaxis.x0 = xaxis.xend then
          initial_scale, -. xaxis.x0 -. 0.5 *. initial_scale
        else 1. /. (xaxis.xend -. xaxis.x0), -. xaxis.x0
      and scaly, tr_y =
        if yaxis.x0 = yaxis.xend then
          initial_scale, -. yaxis.x0 -. 0.5 *. initial_scale
        else 1. /. (yaxis.xend -. yaxis.x0), -. yaxis.x0
      in
    (* TODO Finish that *)
    end

  let do_instructions vp = ()

(*  let close vp =
    let parent = vp.coord_device.Coordinate.parent in
    parent.children <- List.filter (( <> ) vp) parent.children
    (* TODO do we need to do instructions ? *)
    if parent == parent.Coordinate.parent then begin
      do_instructions vp;
      Backend.close vp.backend*)

  (* Grille uniforme; redim: identit� *)
  let layout_grid ?(axes_sys=false) vp rows cols =
    let redim _ _ = () in
    let xstep = 1. /. (float cols) and ystep = 1. /. (float rows) in
    let init_viewport i =
      let xmin = float (i / cols) *. xstep
      and ymin = float (i mod cols) *. ystep in
      let xmax = xmin +. xstep
      and ymax = ymin +. ystep in
      make ~axes_sys vp Device xmin xmax ymin ymax redim
    in
    Array.init (rows * cols) init_viewport in

  let layout_rows ?(axes_sys=false) vp n = grid ~axes_sys vp n 1
  let layout_columns ?(axes_sys=false) vp n = grid ~axes_sys vp 1 n

  let fixed_left ?(axes_sys=false) initial_proportion vp =
    let rec redim_fixed xfactor yfactor = begin
      let coord = vp_fixed.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.;
    end
    and vp_fixed =
      make ~axes_sys vp Device 0. initial_proportion 0. 1. redim_fixed in
    let rec redim xfactor yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.;
      let fixed_right = fst (Coordinate.to_parent vp_fixed ~x:1. ~y:0.) in
      let vp_left = fst (Coordinate.to_parent vp ~x:0. ~y:0.) in
      Coordinate.translate coord (vp_left -. fixed_right) 0.
    end
    and vp = make ~axes_sys vp Device initial_proportion 1. 0. 1. redim in
    (vp_fixed, vp)

  let fixed_right ?(axes_sys=false) initial_proportion vp =
    let rec redim_fixed xfactor _ = begin
      let coord = vp_fixed.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.;
      Coordinate.translate
	coord (-. (fst (Coordinate.to_parent coord ~x:1. ~y:0.))) 0.
    end
    and vp_fixed =
      make ~axes_sys vp Device initial_proportion 1. 0. 1. redim_fixed in
    let rec redim xfactor _ = begin
      let coord = vp.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.
    end
    and vp = make ~axes_sys vp Device 0. initial_proportion 0. 1. redim in
    (vp_fixed, vp)

  let fixed_top ?(axes_sys=false) initial_proportion vp =
    let rec redim_fixed _ yfactor = begin
      let coord = vp_fixed.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor);
    end
    and vp_fixed =
      make ~axes_sys vp Device 0. 1. 0. initial_proportion redim_fixed
    in
    let rec redim _ yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord 1. (1. ./ yfactor);
      let fixed_bottom = snd (Coordinate.to_parent vp_fixed ~x:0. ~y:1.) in
      let vp_top = snd (Coordinate.to_parent vp ~x:0. ~y:0.) in
      Coordinate.translate coord (vp_top -. fixed_bottom) 0.
    end
    and vp = make ~axes_sys vp Device 0. initial_proportion 0. 1. redim in
    (vp_fixed, vp)

  let fixed_bottom ?(axes_sys=false) initial_proportion vp =
    let rec redim_fixed _ yfactor = begin
      let coord = vp_fixed.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor);
      Coordinate.translate
	coord 0. (-. (snd (Coordinate.to_parent coord ~x:0. ~y:1.)))
    end
    and vp_fixed =
      make ~axes_sys vp Device 0. 1. initial_proportion 1. redim_fixed in
    let rec redim _ yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor)
    end
    and vp = make ~axes_sys vp Device 0. 1. 0. initial_proportion redim in
    (vp_fixed, vp)

  (* Des layouts sur les bords, des tailles d�sir�es *)
  let layout_borders ?(north=0.) ?(south=0.) ?(west=0.) ?(east=0.)
      ?(axes_sys=false) vp =
    let east, vp =
      if east > 0. then fixed_right ~axes_sys (1. -. east) vp else vp, vp
    in
    let west, vp =
      if west > 0. then fixed_left ~axes_sys west vp else vp, vp
    in
    let south, vp =
      if south > 0. then fixed_bottom ~axes_sys (1. -. south) vp else vp, vp
    in
    let north, center =
      if north > 0. then fixed_top ~axes_sys north vp else vp, vp
    in
    (north, south, west, east, center)

(* ..........................................................................*)


  let set_line_width vp lw =
    let size =
      if lw <= 0. then def_lw *. vp.coord_orthonormal
      else lw /. usr_lw *. vp.coord_orthonormal in
    Sizes.set_abs_lw vp.size size

  let set_font_size vp ts =
    let size =
      if ts <= 0. then def_ts *. vp.coord_orthonormal
      else ts /. usr_ts *. vp.coord_orthonormal in
    Sizes.set_abs_ts vp.size size

  let set_mark_size vp ms =
    let size =
      if ms <= 0. then def_marks *. vp.coord_orthonormal
      else ms /. usr_ms in
    Sizes.set_abs_ms vp.size size

  let set_rel_line_width vp lw =
    Sizes.set_rel_line_width vp.size (if lw <= 0. then 1. else lw)

  (* FIXME: Fix names text, font? *)
  let set_rel_font_size vp ts =
  Sizes.set_text_size vp.size (if ts <= Graphics0. then 1. else ts)

  let set_rel_mark_size vp ms =
  Sizes.set_rel_mark_size vp.size (if ms <= 0. then 1. else ms)

  let get_line_width vp =
    (Sizes.get_line_width vp.size) *. usr_lw /. vp.coord_orthonormal
  let get_font_size vp =
   (Sizes.get_text_size vp.size) *. usr_ts /. vp.coord_orthonormal
  let get_mark_size vp =
   (Sizes.get_mark_size vp.size) *. usr_ms


(* ......................................................................... *)

  let lower_left_corner vp = Coordinate.to_device vp.coord_device (0., 0.)

  let upper_right_corner vp = Coordinate.to_device vp.coord_device (1., 1.)

  let dimensions vp =
    let x0, y0 = lower_left_corner vp
    and xend, yend = upper_right_corner vp in
    (xend -. x0, yend -. y0)

  let set_global_color vp c =
    add_order vp (fun () -> Backend.set_color vp.backend c);
    Backend.set_color vp.backend c

  let set_global_line_cap vp lc =
    add_order vp (fun () -> Backend.set_line_cap vp.backend lc);
    Backend.set_line_cap vp.backend lc

  let set_global_dash vp x y =
    add_order vp (fun () -> Backend.set_dash vp.backend x y);
    Backend.set_dash vp.backend x y

  let set_global_line_join vp join =
    add_order vp (fun () -> Backend.set_line_join vp.backend join);
    Backend.set_line_join vp.backend join

  let get_line_cap vp = Backend.get_line_cap vp.backend
  let get_dash vp = Backend.get_dash vp.backend
  let get_line_join vp = Backend.get_line_join vp.backend


  let arc t ~r ~a1 ~a2 =
    (*FIXME: better bounds for the arc can be found.*)
    let x, y = get_current_pt t in
    let x' = x -. r *. cos a1
    and y' = y -. r *. sin a1 in
    update_coords t (x'+.r) (y'+.r);
    update_coords t (x'-.r) (y'-.r);
    add_order (fun _ -> Backend.arc t.backend r a1 a2) t

  let close_path t =
    add_order (fun _ -> Backend.close_path t.backend) t

  let clear_path t =
    add_order (fun _ -> Backend.clear_path t.backend) t
  (*let path_extents t = Backend.path_extents t.backend*)

  (*Stroke when using current coordinates.*)
  let stroke_current t =
    add_order (fun _ -> Backend.stroke t.backend) t
  let stroke_current_preserve t =
    add_order (fun _ -> Backend.stroke_preserve t.backend) t

  let stroke t =
    let lw = Sizes.get_lw t.vp.scalings in
    let f _ =
      let ctm = Coordinate.use t.backend t.normalized in
      Backend.set_line_width t.backend lw;
      Backend.stroke t.backend;
      Coordinate.restore t.backend ctm
    in
    add_order f t

  (* IM HERE -->> *)
  let stroke_preserve vp =
    let lw = Sizes.get_lw vp.scalings in
    let f () =
      let ctm = Coordinate.use vp.backend vp.normalized in
      Backend.set_line_width vp.backend lw;
      Backend.stroke_preserve vp.backend;
      Coordinate.restore vp.backend ctm
    in
    add_order f vp

  let fill vp =
    add_order (fun () -> Backend.fill vp.backend) vp

  let fill_preserve t =
    add_order (fun () -> Backend.fill_preserve vp.backend) vp

  let clip_rectangle vp ~x ~y ~w ~h =
    add_order (fun () -> Backend.clip_rectangle vp.backend x y w h) vp

  (* TODO: Check what is it used for ? *)
  let save_vp t =
    let f vp =
      Backend.save t.backend;
      let sizes =
        Sizes.get_lw vp.scalings,
        Sizes.get_ts vp.scalings,
        Sizes.get_marks vp.scalings
      in
      Stack.push sizes vp.scalings_hist
    in
    add_order f t

  (* TODO: Check what is it used for ? *)
  let restore_vp t =
    let f vp =
      try
        let lw, ts, marks = Stack.pop vp.scalings_hist in
        Sizes.set_abs_lw vp.scalings lw;
        Sizes.set_abs_ts vp.scalings ts;
        Sizes.set_abs_marks vp.scalings marks;
        Backend.restore t.backend
      with Stack.Empty -> ()
    in
    add_order f t

  let select_font_face vp slant weight family =
    let f () = Backend.select_font_face vp.backend slant weight family
      add_order f vp

  (* TODO: val show_text. *)

  (* TODO: Check how do we specify the position where we draw the
     mark ? *)
  let render_mark vp name =
    let mark_size = Sizes.get_marks vp.sizes in
    let f () =
      let ctm = Coordinate.use vp.backend vp.coord_orthonormal in
      (* FIXME: We should either translate the coor to the current point and
         change Pointstyle to use [move_to] instead of [rel_move_to], or we
         should update the current point. *)
      Backend.scale vp.backend marks marks;
      Pointstyle.render name vp.backend;
      Coordinate.restore vp.backend ctm;
    in
    (* FIXME : what are extents ? *)
    (* FIXME: extents are expressed in "marks-normalized" coords. We need
       to have it in user coords in order to determine the extents. *)
     (*
     let extents = Pointstyle.extents name in
     let marks' = marks *. t.square_side in
     let x',y' = get_current_pt t in
     Printf.printf "initial marks: %f %f %f %f %f" marks t.square_side marks' x' y';
     let axpmw x w = x +. w *. marks' in update_coords t (axpmw x'
       extents.Matrix.x) (axpmw y' extents.Matrix.y);
     update_coords t (axpmw x' (extents.Matrix.x +. extents.Matrix.w)) (axpmw y' (extents.Matrix.y
                                                                                  +.extents.Matrix.h));
     *)
    add_order f vp
end

