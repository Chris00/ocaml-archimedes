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
  type t
  type coord_name = Device | Graph | Data | Orthonormal
  val get_coord_from_name : t -> coord_name -> Coordinate.t
  val init : ?lines:float -> ?text:float -> ?marks:float -> ?w:float ->
    ?h:float -> dirs:string list -> string -> t
  val make : ?axes_sys:bool -> ?lines:float -> ?text:float -> ?marks:float ->
    t -> coord_name -> float -> float -> float -> float ->
    (t -> float -> float -> unit) -> t

  val layout_grid : ?axes_sys:bool -> t -> int -> int -> t array
  val layout_rows : ?axes_sys:bool -> t -> int -> t array
  val layout_columns : ?axes_sys:bool -> t -> int -> t array
  val fixed_left : ?axes_sys:bool -> float -> t -> t * t
  val fixed_right : ?axes_sys:bool -> float -> t -> t * t
  val fixed_top : ?axes_sys:bool -> float -> t -> t * t
  val fixed_bottom : ?axes_sys:bool -> float -> t -> t * t
  val layout_borders : ?north:float -> ?south:float -> ?west:float ->
    ?east:float -> ?axes_sys:bool -> t -> t * t * t * t * t

  val set_line_width : t -> float -> unit
  val set_font_size : t -> float -> unit
  val set_mark_size : t -> float -> unit
  val set_rel_line_width : t -> float -> unit
  val set_rel_font_size : t -> float -> unit
  val set_rel_mark_size : t -> float -> unit
  val get_line_width : t -> float
  val get_font_size : t -> float
  val get_mark_size : t -> float

  val lower_left_corner : t -> float * float
  val upper_right_corner : t -> float * float
  val dimensions : t -> float * float (* returns (w, h) *)
    (* set_global_param set param of backend and then of all viewports *)
  val set_global_color : t -> Color.t -> unit
  val set_global_line_cap : t -> Backend.line_cap -> unit
  val set_global_dash : t -> float -> float array -> unit
  val set_global_line_join : t -> Backend.line_join -> unit
    (*  val set_global_line_width : t -> float -> unit*)
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
  val stroke : ?path:Path.t -> t -> coord_name -> unit
  val fill : t -> unit
  val fill_preserve : t -> unit
  val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
    (*  val save_vp : t -> unit
        val restore_vp : t -> unit*)
  val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit
    (*  val show_text :
        t ->
        rotate:float ->
        x:float -> y:float -> Backend.text_position -> string -> unit
    (*  val text_extents : t -> string -> rectangle*)*)
  val render_mark : t -> string -> unit
    (* val mark_extents : t -> string -> rectangle *)
  val xrange : t -> float -> float -> unit
  val yrange : t -> float -> float -> unit
  val xmin : t -> float
  val xmax : t -> float
  val ymin : t -> float
  val ymax : t -> float
  val close : t -> unit
  val do_instructions : t -> unit
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

    mutable square_side: float;

    path: Path.t;

    (* Axes system associated to the viewport *)
    mutable axes_system: Axes.t;
    (* For sizing texts, tics, etc. *)
    mutable sizes: Sizes.t;
    (* An instruction is a "thing" to plot on the device, we memorize
       their order to replot in case of necessity *)
    mutable instructions: (unit -> unit) Queue.t;

    (* Draw immediately or wait for closing ? *)
    mutable immediate_drawing: bool;

    (* Redimension function: when the parent viewport grows of x and y,
       what should subsequent coordinates become ? This function must set
       them correctly *)
    redim: t -> float -> float -> unit;
  }

  type coord_name = Device | Graph | Data | Orthonormal

  let def_lw, def_ts, def_ms = 0.002, 0.024, 0.01

  (* Multiplier to get "user-friendly" values (e.g. 12pt instead of 0.024) *)
  let usr_lw, usr_ts, usr_ms = 500., 500., 100.

  let init ?(lines=def_lw *. usr_lw) ?(text=def_ts *. usr_ts)
      ?(marks=def_ms *. usr_ms) ?(w=640.) ?(h=480.) ~dirs backend_name =
    let backend = Backend.make ~dirs backend_name w h in
    let coord_root = Coordinate.make_root (Backend.get_matrix backend) in
    let size0 = min w h in
    let coord_device = Coordinate.make_identity coord_root in
    let coord_graph = Coordinate.make_scale
      (Coordinate.make_translate coord_device 0.1 0.1) 0.8 0.8 in
    let rec axes_system = Axes.default_axes_system [viewport]
    and viewport = {
      backend = backend;
      parent = viewport;
      children = [];
      coord_device = coord_device; coord_graph = coord_graph;
      coord_orthonormal =
      Coordinate.make_scale coord_device (size0 /. w) (size0 /. h);
      (* We don't care; will be updated as soon as points are added or we
	 change axes. *)
      coord_data = Coordinate.make_identity coord_graph;
      path = Path.make ();
      axes_system = axes_system;
      sizes = Sizes.make_rel (Sizes.make_root size0 1. 1. 1.) lines text marks;
      instructions = Queue.create ();
      immediate_drawing = false;
      redim = (fun _ _ _ -> ());
      square_side = size0;
    } in
    viewport

  let get_coord_from_name vp = function
    | Device -> vp.coord_device
    | Graph -> vp.coord_graph
    | Data -> vp.coord_data
    | Orthonormal -> vp.coord_orthonormal

  let make ?(axes_sys=false) ?(lines=def_lw) ?(text=def_ts) ?(marks=def_ms)
      vp coord_name xmin xmax ymin ymax redim =
   let coord_parent = get_coord_from_name vp coord_name in
   let w, h, size0 =
      let xmax', ymax' = Coordinate.to_device coord_parent xmax ymax
      and xmin', ymin' = Coordinate.to_device coord_parent xmin ymin in
      let w = xmax' -. xmin' and h = ymax' -. ymin' in
      w, h, min w h
    in
    let coord_parent = get_coord_from_name vp coord_name in
    let coord_device =
      Coordinate.make_translate
        (Coordinate.make_scale coord_parent (xmax -. xmin) (ymax -. ymin))
        xmin ymin
    in
    let coord_graph =
      Coordinate.make_scale
        (Coordinate.make_translate coord_device 0.1 0.1) 0.8 0.8 in
    let rec viewport = {
      backend = vp.backend;
      parent = vp;
      children = [];
      coord_device = coord_device; coord_graph = coord_graph;
      coord_orthonormal =
      Coordinate.make_scale coord_device (size0 /. w) (size0 /. h);
      (* We don't care; will be updated as soon as points are added or we
	 change axes. *)
      coord_data = Coordinate.make_identity coord_graph;
      path = Path.make ();
      axes_system =
        if axes_sys then vp.axes_system
        else Axes.default_axes_system [viewport];
      sizes = Sizes.make_rel vp.sizes lines text marks;
      instructions = Queue.create ();
      immediate_drawing = false;
      redim = redim;
      square_side = size0;
    } in
    vp.children <- viewport :: vp.children;
    viewport

  let is_nan_or_inf (x:float) = x <> x || 1. /. x = 0.

  let initial_scale = 1.

  let blank vp =
    let ctm = Coordinate.use vp.backend vp.coord_device in
    Backend.save vp.backend;
    Backend.clear_path vp.backend;
    Backend.set_color vp.backend Color.white;
    Backend.rectangle vp.backend 0. 0. 1. 1.;
    Backend.fill vp.backend;
    Backend.restore vp.backend;
    Coordinate.restore vp.backend ctm

  let add_instruction f vp = Queue.push f vp.instructions

  let rec do_instructions vp =
    blank vp;
    Queue.iter (fun f -> f ()) vp.instructions;
    List.iter do_instructions vp.children

  let auto_fit vp x0 y0 x1 y1 =
    let xaxis = vp.axes_system.Axes.x
    and yaxis = vp.axes_system.Axes.y in
    let x0' = min x0 x1 (* TODO can we skip those tests ? (x|y)(0|1)' *)
    and x1' = max x0 x1
    and y0' = min y0 y1
    and y1' = max y0 y1 in
    let updated = ref false in
    if xaxis.Axes.auto_x0 then
      if is_nan_or_inf xaxis.Axes.x0 || x0' < xaxis.Axes.x0 then
        (xaxis.Axes.x0 <- x0'; updated := true);
    if xaxis.Axes.auto_xend then
      if is_nan_or_inf xaxis.Axes.xend || x1' > xaxis.Axes.xend then
        (xaxis.Axes.xend <- x1'; updated := true);
    if yaxis.Axes.auto_x0 then
      if is_nan_or_inf yaxis.Axes.x0 || y0' < yaxis.Axes.x0 then
        (yaxis.Axes.x0 <- y0'; updated := true);
    if yaxis.Axes.auto_xend then
      if is_nan_or_inf yaxis.Axes.xend || y1' > yaxis.Axes.xend then
        (yaxis.Axes.xend <- y1'; updated := true);
    if !updated then begin
      let scalx, tr_x =
        if xaxis.Axes.x0 = xaxis.Axes.xend then
          initial_scale, -. xaxis.Axes.x0 -. 0.5 *. initial_scale
        else 1. /. (xaxis.Axes.xend -. xaxis.Axes.x0), -. xaxis.Axes.x0
      and scaly, tr_y =
        if yaxis.Axes.x0 = yaxis.Axes.xend then
          initial_scale, -. yaxis.Axes.x0 -. 0.5 *. initial_scale
        else 1. /. (yaxis.Axes.xend -. yaxis.Axes.x0), -. yaxis.Axes.x0
      in
      let data_transform = Matrix.make_scale scalx scaly in
      Matrix.translate data_transform tr_x tr_y;
      Coordinate.transform vp.coord_data data_transform;
      if vp.immediate_drawing then do_instructions vp
    end

  let close vp =
    let parent = vp.parent in
    parent.children <- List.filter (( <> ) vp) parent.children;
    if parent == parent.parent then begin
      do_instructions vp;
      Backend.close vp.backend
    end

  (* Grille uniforme; redim: identit� *)
  let layout_grid ?(axes_sys=false) vp rows cols =
    let redim _ _ _ = () in
    let xstep = 1. /. (float cols) and ystep = 1. /. (float rows) in
    let init_viewport i =
      let xmin = float (i / cols) *. xstep
      and ymin = float (i mod cols) *. ystep in
      let xmax = xmin +. xstep
      and ymax = ymin +. ystep in
      make ~axes_sys vp Device xmin xmax ymin ymax redim
    in
    Array.init (rows * cols) init_viewport

  let layout_rows ?(axes_sys=false) vp n =
    layout_grid ~axes_sys vp n 1
  let layout_columns ?(axes_sys=false) vp n =
    layout_grid ~axes_sys vp 1 n

  let fixed_left ?(axes_sys=false) initial_proportion vp =
    let redim_fixed vp xfactor _ = begin
      let coord = vp.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.;
    end in
    let vp_fixed =
      make ~axes_sys vp Device 0. initial_proportion 0. 1. redim_fixed in
    let redim vp xfactor _ = begin
      let coord = vp.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.;
      let fixed_right, _ =
        Coordinate.to_parent vp_fixed.coord_device ~x:1. ~y:0. in
      let vp_left, _ = Coordinate.to_parent vp.coord_device ~x:0. ~y:0. in
      Coordinate.translate coord (vp_left -. fixed_right) 0.
    end in
    let vp' = make ~axes_sys vp Device initial_proportion 1. 0. 1. redim in
    (vp_fixed, vp')

  let fixed_right ?(axes_sys=false) initial_proportion vp =
    let redim_fixed vp xfactor _ = begin
      let coord = vp.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.;
      Coordinate.translate
        coord (-. (fst (Coordinate.to_parent coord ~x:1. ~y:0.))) 0.
    end in
    let vp_fixed =
      make ~axes_sys vp Device initial_proportion 1. 0. 1. redim_fixed in
    let redim vp xfactor _ = begin
      let coord = vp.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.
    end in
    let vp' = make ~axes_sys vp Device 0. initial_proportion 0. 1. redim in
    (vp_fixed, vp')

  let fixed_top ?(axes_sys=false) initial_proportion vp =
    let redim_fixed vp _ yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor);
    end in
    let vp_fixed =
      make ~axes_sys vp Device 0. 1. 0. initial_proportion redim_fixed
    in
    let rec redim vp _ yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor);
      let _, fixed_bottom =
        Coordinate.to_parent vp_fixed.coord_device ~x:0. ~y:1. in
      let _, vp_top = Coordinate.to_parent vp.coord_device ~x:0. ~y:0. in
      Coordinate.translate coord (vp_top -. fixed_bottom) 0.
    end in
    let vp' = make ~axes_sys vp Device 0. initial_proportion 0. 1. redim in
    (vp_fixed, vp')

  let fixed_bottom ?(axes_sys=false) initial_proportion vp =
    let redim_fixed vp _ yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor);
      Coordinate.translate
        coord 0. (-. (snd (Coordinate.to_parent coord ~x:0. ~y:1.)))
    end in
    let vp_fixed =
      make ~axes_sys vp Device 0. 1. initial_proportion 1. redim_fixed in
    let rec redim vp _ yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor)
    end in
    let vp' = make ~axes_sys vp Device 0. 1. 0. initial_proportion redim in
    (vp_fixed, vp')

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


(* Data that depends directly on viewports
 ***********************************************************************)

  let set_line_width vp lw =
    let size =
      if lw <= 0. then def_lw *. vp.square_side
      else lw /. usr_lw *. vp.square_side in
    Sizes.set_abs_lw vp.sizes size

  let set_font_size vp ts =
    let size =
      if ts <= 0. then def_ts *. vp.square_side
      else ts /. usr_ts *. vp.square_side in
    Sizes.set_abs_ts vp.sizes size

  let set_mark_size vp ms =
    let size =
      if ms <= 0. then def_ms *. vp.square_side
      else ms /. usr_ms in
    Sizes.set_abs_ms vp.sizes size

  let set_rel_line_width vp lw =
    Sizes.set_rel_lw vp.sizes (if lw <= 0. then 1. else lw)

  (* FIXME: Fix names text, font? *)
  let set_rel_font_size vp ts =
    Sizes.set_rel_ts vp.sizes (if ts <= 0. then 1. else ts)

  let set_rel_mark_size vp ms =
    Sizes.set_rel_ms vp.sizes (if ms <= 0. then 1. else ms)

  let get_line_width vp =
    (Sizes.get_lw vp.sizes) *. usr_lw /. vp.square_side
  let get_font_size vp =
   (Sizes.get_ts vp.sizes) *. usr_ts /. vp.square_side
  let get_mark_size vp =
   (Sizes.get_marks vp.sizes) *. usr_ms


(* ......................................................................... *)

  let lower_left_corner vp = Coordinate.to_device vp.coord_device ~x:0. ~y:0.

  let upper_right_corner vp = Coordinate.to_device vp.coord_device ~x:1. ~y:1.

  let dimensions vp = Coordinate.to_device_distance vp.coord_device ~dx:1. ~dy:1.

  let set_global_color vp c =
    add_instruction (fun () -> Backend.set_color vp.backend c) vp;
    Backend.set_color vp.backend c

  let set_global_line_cap vp lc =
    add_instruction (fun () -> Backend.set_line_cap vp.backend lc) vp;
    Backend.set_line_cap vp.backend lc

  let set_global_dash vp x y =
    add_instruction (fun () -> Backend.set_dash vp.backend x y) vp;
    Backend.set_dash vp.backend x y

  let set_global_line_join vp join =
    add_instruction (fun () -> Backend.set_line_join vp.backend join) vp;
    Backend.set_line_join vp.backend join

  let get_line_cap vp = Backend.get_line_cap vp.backend
  let get_dash vp = Backend.get_dash vp.backend
  let get_line_join vp = Backend.get_line_join vp.backend

  let move_to vp ~x ~y =
    add_instruction (fun () -> Path.move_to vp.path ~x ~y) vp

  let line_to vp ~x ~y =
    add_instruction (fun () -> Path.line_to vp.path ~x ~y) vp

  let rel_move_to vp ~x ~y =
    add_instruction (fun () -> Path.rel_move_to vp.path ~x ~y) vp

  let rel_line_to vp ~x ~y =
    add_instruction (fun () -> Path.rel_line_to vp.path ~x ~y) vp

  let curve_to vp ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    add_instruction (fun () -> Path.curve_to vp.path ~x1 ~y1 ~x2 ~y2 ~x3 ~y3) vp

  let rectangle vp ~x ~y ~w ~h =
    add_instruction (fun () -> Path.rectangle vp.path ~x ~y ~w ~h) vp

  let arc vp ~r ~a1 ~a2 =
    add_instruction (fun () -> Path.arc vp.path ~r ~a1 ~a2) vp

  let close_path vp =
    add_instruction (fun () -> Path.close vp.path) vp

  let clear_path vp =
    add_instruction (fun () -> Path.clear vp.path) vp

  let path_extents vp = Path.extents vp.path

  let stroke_preserve ?path vp coord_name =
    let coord = get_coord_from_name vp coord_name in
    let path = match path with
      | None -> vp.path
      | Some p -> p
    in
    let e = Path.extents path in
    let x0 = e.Matrix.x
    and y0 = e.Matrix.y in
    let x1 = x0 +. e.Matrix.w
    and y1 = y0 +. e.Matrix.h in
    auto_fit vp x0 y0 x1 y1;
    let f () =
      let ctm = Coordinate.use vp.backend coord in
      Path.stroke_on_backend path vp.backend;
      Coordinate.restore vp.backend ctm
    in
    add_instruction f vp

  let stroke ?path vp coord_name =
    stroke_preserve ?path vp coord_name;
    if path = None then add_instruction (fun () -> Path.clear vp.path) vp

  let fill vp =
    add_instruction (fun () -> Backend.fill vp.backend) vp

  let fill_preserve vp =
    add_instruction (fun () -> Backend.fill_preserve vp.backend) vp

  let clip_rectangle vp ~x ~y ~w ~h =
    add_instruction (fun () -> Backend.clip_rectangle vp.backend x y w h) vp

(* (* TODO: Check what is it used for ? (drop it ?) *)
  let save_vp vp =
    let f () =
      Backend.save vp.backend;
      let sizes =
        Sizes.get_lw vp.scalings,
        Sizes.get_ts vp.scalings,
        Sizes.get_marks vp.scalings
      in
      Stack.push sizes vp.scalings_hist
    in
    add_instruction f vp

  (* TODO: Check what is it used for ? (drop it ?) *)
  let restore_vp vp =
    let f () =
      try
        let lw, ts, marks = Stack.pop vp.scalings_hist in
        Sizes.set_abs_lw vp.scalings lw;
        Sizes.set_abs_ts vp.scalings ts;
        Sizes.set_abs_marks vp.scalings marks;
        Backend.restore vp.backend
      with Stack.Empty -> ()
    in
    add_instruction f vp *)

  let select_font_face vp slant weight family =
    let f () = Backend.select_font_face vp.backend slant weight family in
      add_instruction f vp

  (* TODO: val show_text. *)

  let render_mark vp name =
    let mark_size = Sizes.get_marks vp.sizes in
    let f () =
      let ctm = Coordinate.use vp.backend vp.coord_orthonormal in
      Backend.scale vp.backend mark_size mark_size;
      Pointstyle.render name vp.backend;
      Coordinate.restore vp.backend ctm;
    in
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
    add_instruction f vp

  (* TODO: maintain extents for current path instead of calling auto_fit
     after each call to functions modifying path (e.g arc and rectangle)*)

  let xmin vp = vp.axes_system.Axes.x.Axes.x0
  let xmax vp = vp.axes_system.Axes.x.Axes.xend
  let ymin vp = vp.axes_system.Axes.y.Axes.x0
  let ymax vp = vp.axes_system.Axes.y.Axes.xend

  let xrange vp x0 xend =
    vp.axes_system.Axes.x.Axes.x0 <- x0;
    vp.axes_system.Axes.x.Axes.xend <- xend

  let yrange vp y0 yend =
    vp.axes_system.Axes.y.Axes.x0 <- y0;
    vp.axes_system.Axes.y.Axes.xend <- yend
end
