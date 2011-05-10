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

let is_nan_or_inf (x:float) = x <> x || 1. /. x = 0.

module rec Axes : sig
  type sign = Positive | Negative

  type offset =
    | Relative of float
    | Absolute of float

  type graph_axis = {
    tics: Tics.t;
    offset: offset;
    major_tics: string * float;
    minor_tics: string * float;
    mutable tics_values: Tics.tic list
  }

  type axis = {
    mutable x0: float;     mutable auto_x0: bool;
    mutable xend: float;   mutable auto_xend: bool;
    mutable log: bool;
    mutable orientation: sign;
    mutable graph_axes: graph_axis list;
    mutable viewports: Viewport.t list
  }

  type t = {
    x: axis;
    y: axis;
  }

  val default_axis: unit -> axis
  val default_axes_system: unit -> t

  val add_axis: (string * float) -> (string * float) -> Tics.t -> offset ->
    sign -> axis -> unit
  val draw_axes: Viewport.t -> unit
end
= struct
  module V = Viewport
  module B = Backend

  type sign = Positive | Negative

  (* FIXME: offset might benefit from better variant names such as Data |
     Graph. And, it doesn't support other coord_names. Maybe we should use
     polymorphic variant types shared between Axes and Viewport instead of
     redefining local ones as Axes.Data and Axes.Graph. *)
  type offset =
    | Relative of float
    | Absolute of float

  type graph_axis = {
    tics: Tics.t;
    offset: offset;
    major_tics: string * float;
    minor_tics: string * float;
    mutable tics_values: Tics.tic list
  }

  type axis = {
    mutable x0: float;     mutable auto_x0: bool;
    mutable xend: float;   mutable auto_xend: bool;
    mutable log: bool;
    mutable orientation: sign;
    mutable graph_axes: graph_axis list;
    mutable viewports: Viewport.t list
  }

  type t = {
    x: axis;
    y: axis;
  }

  let default_axis () =
    { x0 = 0.; xend = 1.; auto_x0 = true; auto_xend = true;
      log = false; orientation = Positive; graph_axes = []; viewports = [] }

  let default_axes_system () =
    { x = default_axis ();
      y = default_axis () }

  let add_axis major_tics minor_tics tics offset sign axis =
    let graph_axis = {
      tics=tics;
      offset=offset;
      major_tics=major_tics;
      minor_tics=minor_tics;
      tics_values=Tics.tics axis.Axes.x0 axis.Axes.xend tics
    } in
    axis.graph_axes <- graph_axis :: axis.graph_axes

  (* returns the offset and wether the labels are over or under the axis *)
  let axis_offset start range = function
    | Absolute y -> start +. range *. y, if y < 0.5 then -1. else 1.
    | Relative y -> y, if y < start +. range /. 2. then -1. else 1.

  let tic vp x y (tic_type, tic_size) =
    V.set_rel_mark_size_direct vp tic_size ();
    V.mark_direct vp x y tic_type ()

  let draw_tic tic graph_axis text = function
    | Tics.Major (None, v) -> tic v graph_axis.Axes.major_tics
    | Tics.Major (Some label, v) -> tic v graph_axis.Axes.major_tics;
        text v label
    | Tics.Minor v -> tic v graph_axis.Axes.minor_tics

  let draw_x_axis vp graph_axis =
    (* TODO add Backend.ARROW *)
    (*V.set_line_cap vp Backend.ARROW;*)
    let yrange = V.ymax vp -. V.ymin vp in
    let offset, pos = axis_offset (V.ymin vp) yrange graph_axis.offset in
    let path = Path.make_at (V.xmin vp) offset in
    Path.line_to path (V.xmax vp) offset;
    V.stroke_direct path vp V.Data ();
    let y = offset +. yrange *. 0.0375 *. pos in
    let tic x = tic vp x offset in
    let text x lbl = V.show_text_direct vp V.Data ~x ~y B.CC lbl () in
    List.iter (draw_tic tic graph_axis text) graph_axis.tics_values
    (*V.set_line_cap vp Backend.BUTT;*)

  let draw_y_axis vp graph_axis =
    let xrange = V.xmax vp -. V.xmin vp in
    let offset, pos = axis_offset (V.xmin vp) xrange graph_axis.offset in
    let path = Path.make_at offset (V.ymin vp) in
    Path.line_to path offset (V.ymax vp);
    V.stroke_direct path vp V.Data ();
    let x = offset +. xrange *. 0.0375 *. pos in
    let tic y = tic vp offset y in
    let text y lbl = V.show_text_direct vp V.Data ~x ~y B.CC lbl () in
    List.iter (draw_tic tic graph_axis text) graph_axis.tics_values

  let draw_axes vp =
    List.iter (draw_x_axis vp) (vp.Viewport.axes_system.x.graph_axes);
    List.iter (draw_y_axis vp) (vp.Viewport.axes_system.y.graph_axes)
end
and Viewport : sig
  type t = {
    backend: Backend.t;
    parent: t;
    mutable children: t list;
    mutable coord_device: Coordinate.t;
    mutable coord_graph: Coordinate.t;
    mutable coord_orthonormal: Coordinate.t;
    mutable coord_data: Coordinate.t;
    mutable square_side: float;
    path: Path.t;
    mutable axes_system: Axes.t;
    mutable sizes: Sizes.t;
    mutable mark_size: float;
    mutable instructions: (unit -> unit) Queue.t;
    mutable immediate_drawing: bool;
    redim: t -> float -> float -> unit;
  }
  type coord_name = Device | Graph | Data | Orthonormal
  val get_coord_from_name : t -> coord_name -> Coordinate.t
  val init : ?lines:float -> ?text:float -> ?marks:float -> ?w:float ->
    ?h:float -> dirs:string list -> string -> t
  val make : ?axes_sys:bool -> ?lines:float -> ?text:float -> ?marks:float ->
    t -> coord_name -> float -> float -> float -> float ->
    (t -> float -> float -> unit) -> t

  val get_backend : t -> Backend.t

  val sync : ?x:bool -> ?y:bool -> t -> t -> unit

  val layout_grid : ?syncs:(bool * bool * bool * bool) -> ?axes_sys:bool ->
    t -> int -> int -> t array
  val layout_rows : ?syncs:(bool * bool) -> ?axes_sys:bool -> t ->
    int -> t array
  val layout_columns : ?syncs:(bool * bool) -> ?axes_sys:bool -> t ->
    int -> t array
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

  val set_rel_mark_size_direct : t -> float -> unit -> unit

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
  val stroke_direct : Path.t -> t -> coord_name -> unit -> unit
  val stroke_preserve : ?path:Path.t -> t -> coord_name -> unit
  val stroke : ?path:Path.t -> t -> coord_name -> unit
  val fill : t -> unit
  val fill_preserve : t -> unit
  val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
    (*  val save_vp : t -> unit
        val restore_vp : t -> unit*)
  val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit
  val show_text_direct :
    t -> coord_name ->
    ?rotate:float ->
    x:float -> y:float -> Backend.text_position -> string -> unit -> unit
  val show_text :
    t -> coord_name ->
    ?rotate:float ->
    x:float -> y:float -> Backend.text_position -> string -> unit
    (*  val text_extents : t -> string -> rectangle*)
  val ortho_from : t -> coord_name -> float * float -> float * float
  val data_from : t -> coord_name -> float * float -> float * float
  val mark : t -> x:float -> y:float -> string -> unit
    (* val mark_extents : t -> string -> rectangle *)

  val mark_direct : t -> x:float -> y:float -> string -> unit -> unit
  val xrange : t -> float -> float -> unit
  val yrange : t -> float -> float -> unit
  val xmin : t -> float
  val xmax : t -> float
  val ymin : t -> float
  val ymax : t -> float
  val close : t -> unit
  val do_instructions : t -> unit

  val add_x_axis: ?major:(string * float) -> ?minor:(string * float) ->
    ?tics:Tics.t -> ?offset:Axes.offset -> ?sign:Axes.sign -> t -> unit
  val add_y_axis: ?major:(string * float) -> ?minor:(string * float) ->
    ?tics:Tics.t -> ?offset:Axes.offset -> ?sign:Axes.sign -> t -> unit
  val draw_axes: t -> unit

  val box: t -> unit
  val cross: t -> unit
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
    mutable mark_size: float;
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

  (* Multiplier to get "user-friendly" values (e.g. 12pt instead of 0.024) *)
  let usr_lw, usr_ts, usr_ms = 500., 500., 100.
  let def_lw, def_ts, def_ms = 0.002, 0.024, 0.01

  let get_coord_from_name vp = function
    | Device -> vp.coord_device
    | Graph -> vp.coord_graph
    | Data -> vp.coord_data
    | Orthonormal -> vp.coord_orthonormal

(* Initialization functions
 ***********************************************************************)

  let init ?(lines=def_lw *. usr_lw) ?(text=def_ts *. usr_ts)
      ?(marks=def_ms *. usr_ms) ?(w=640.) ?(h=480.) ~dirs backend_name =
    let backend = Backend.make ~dirs backend_name w h in
    let coord_root =
      if Backend.flipy backend then
        let flip =
          { Matrix.xx = 1.; Matrix.yx = 0.;
            Matrix.xy = 0.;  Matrix.yy = -1. ; Matrix.x0 = 0.; Matrix.y0 = h }
        in
        Coordinate.make_root (Matrix.mul flip (Backend.get_matrix backend))
      else
        Coordinate.make_root (Backend.get_matrix backend)
    in
    let size0 = min w h in
    let coord_device = Coordinate.make_scale coord_root w h in
    let coord_graph =
      Coordinate.make_translate coord_device 0.1 0.1 in
    Coordinate.scale coord_graph 0.8 0.8;
    let rec viewport = {
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
      axes_system = Axes.default_axes_system ();
      sizes = Sizes.make_rel (Sizes.make_root size0 1. 1. 1.) lines text marks;
      mark_size = def_ms;
      instructions = Queue.create ();
      immediate_drawing = false;
      redim = (fun _ _ _ -> ());
      square_side = size0;
    } in
    viewport.axes_system.Axes.x.Axes.viewports <- [viewport];
    viewport.axes_system.Axes.y.Axes.viewports <- [viewport];
    viewport

  let make ?(axes_sys=false) ?(lines=def_lw) ?(text=def_ts) ?(marks=def_ms)
      vp coord_name xmin xmax ymin ymax redim =
   let coord_parent = get_coord_from_name vp coord_name in
   let w, h, size0 =
     (* We can't be sure of the y orientation, so we them in absolute *)
     let xmax', y2' = Coordinate.to_device coord_parent xmax ymax
     and xmin', y1' = Coordinate.to_device coord_parent xmin ymin in
     let w = xmax' -. xmin' and h = abs_float (y2' -. y1') in
     w, h, min w h
    in
    let coord_parent = get_coord_from_name vp coord_name in
    let coord_device =
      Coordinate.make_scale
        (Coordinate.make_translate coord_parent xmin ymin)
        (xmax -. xmin) (ymax -. ymin)
    in
    let coord_graph =
      Coordinate.make_scale
        (Coordinate.make_translate coord_device 0.1 0.1) 0.8 0.8 in
    let viewport = {
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
        else Axes.default_axes_system ();
      sizes = Sizes.make_rel vp.sizes lines text marks;
      mark_size = def_ms;
      instructions = Queue.create ();
      immediate_drawing = false;
      redim = redim;
      square_side = size0;
    } in
    if not axes_sys then begin
      viewport.axes_system.Axes.x.Axes.viewports <- [viewport];
      viewport.axes_system.Axes.y.Axes.viewports <- [viewport]
    end;
    vp.children <- viewport :: vp.children;
    viewport

(* Basic functions
 ***********************************************************************)

  let get_backend vp = vp.backend

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

  (* Note: if a vp is synchronized with one of its children, this children
     will be redrawn two times. *)
  let rec do_instructions vp =
    blank vp;
    Queue.iter (fun f -> f ()) vp.instructions;
    List.iter do_instructions vp.children

  let close vp =
    let parent = vp.parent in
    parent.children <- List.filter (fun x -> not (x == vp)) parent.children;
    if vp == parent then begin
      do_instructions vp;
      Backend.close vp.backend
    end

(* Axes update functions
 ***********************************************************************)

  let xmin vp = vp.axes_system.Axes.x.Axes.x0
  let xmax vp = vp.axes_system.Axes.x.Axes.xend
  let ymin vp = vp.axes_system.Axes.y.Axes.x0
  let ymax vp = vp.axes_system.Axes.y.Axes.xend

  let update_coordinate_system vp =
    let x0, xend, y0, yend = xmin vp, xmax vp, ymin vp, ymax vp in
    let m = Matrix.make_scale
      (1. /. (xend -. x0)) (1. /. (yend -. y0))
    in
    Matrix.translate m (-. x0) (-. y0);
    Coordinate.transform vp.coord_data m

  let update_tics axis =
    let f graph_axis =
      let x0, xend = axis.Axes.x0, axis.Axes.xend in
      graph_axis.Axes.tics_values <- Tics.tics x0 xend graph_axis.Axes.tics
    in
    List.iter f axis.Axes.graph_axes

  (* Utility function for (x|y)range *)
  let update_axis axis vp x0 xend =
    axis.Axes.auto_x0 <- is_nan_or_inf x0;
    if not (is_nan_or_inf x0) then axis.Axes.x0 <- x0;
    axis.Axes.auto_xend <- is_nan_or_inf xend;
    if not (is_nan_or_inf xend) then axis.Axes.xend <- xend;
    update_coordinate_system vp;
    if vp.immediate_drawing
    then List.iter do_instructions axis.Axes.viewports

  let xrange vp = update_axis vp.axes_system.Axes.x vp
  let yrange vp = update_axis vp.axes_system.Axes.y vp

  let auto_fit vp x0 y0 x1 y1 =
    let xaxis = vp.axes_system.Axes.x
    and yaxis = vp.axes_system.Axes.y in
    (* TODO can we skip those tests ? (x|y)(0|1)' *)
    let x0', x1', y0', y1' = min x0 x1, max x0 x1, min y0 y1, max y0 y1 in
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
      update_tics xaxis;
      update_tics yaxis;
      update_coordinate_system vp;
      if vp.immediate_drawing then begin
        let l1 = vp.axes_system.Axes.x.Axes.viewports in
        let l2 = vp.axes_system.Axes.y.Axes.viewports in
        (* We want to merge the 2 lists without duplicates *)
        let l2' = List.filter (fun x -> not (List.exists (( == ) x) l1)) l2 in
        List.iter do_instructions (List.rev_append l1 l2')
          (* TODO Is there a way to optimize that bunch of code ? *)
      end
    end

(* Synchronization
 ***********************************************************************)

  let sync ?(x=true) ?(y=true) vp vp_base =
    let xvp = if x then vp_base else vp
    and yvp = if y then vp_base else vp in
    let xaxis = xvp.axes_system.Axes.x
    and yaxis = yvp.axes_system.Axes.y in
    vp.axes_system <- {Axes.x=xaxis; y=yaxis}

(* Layouts
 ***********************************************************************)

  (* Uniform grid; redim: identity *)
  let layout_grid ?(syncs=(false, false, false, false)) ?(axes_sys=false)
      vp rows cols =
    let cols_sync_x, cols_sync_y, rows_sync_x, rows_sync_y = syncs in
    let redim _ _ _ = () in
    let xstep = 1. /. (float cols) and ystep = 1. /. (float rows) in
    let ret = Array.make (rows * cols) vp in
    let init_viewport i =
      let x = i / cols and y = i mod cols in
      let xmin = float x *. xstep
      and ymin = float y *. ystep in
      let xmax = xmin +. xstep
      and ymax = ymin +. ystep in
      ret.(i) <- make ~axes_sys vp Device xmin xmax ymin ymax redim;
      if x > 0 then begin
        if cols_sync_x then sync ~y:false ret.(i) ret.(x)
        else if cols_sync_y then sync ~x:false ret.(i) ret.(x)
      end;
      if y > 0 then begin
        if rows_sync_x then sync ~y:false ret.(i) ret.(y * cols)
        else if rows_sync_y then sync ~x:false ret.(i) ret.(y * cols)
      end
    in
    for i = 0 to rows * cols - 1 do init_viewport i done;
    ret

  let layout_rows ?(syncs=false, false) ?(axes_sys=false) vp n =
    let sync_x, sync_y = syncs in
    layout_grid ~syncs:(false, false, sync_x, sync_y) ~axes_sys vp n 1

  let layout_columns ?(syncs=false, false) ?(axes_sys=false) vp n =
    let sync_x, sync_y = syncs in
    layout_grid ~syncs:(sync_x, sync_y, false, false) ~axes_sys vp 1 n

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

  (* Border layouts, of desired sizes *)
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
    add_instruction (fun () -> Backend.set_line_width vp.backend lw) vp

  let set_font_size vp ts =
    add_instruction (fun () -> Backend.set_font_size vp.backend ts) vp

  let set_mark_size vp ms =
    add_instruction (fun () -> vp.mark_size <- ms) vp

  let set_rel_line_width vp lw =
    let f () =
      Backend.set_line_width vp.backend (lw /. usr_lw *. vp.square_side)
    in
    add_instruction f vp

  (* FIXME: Fix names text, font? *)
  let set_rel_font_size vp ts =
    let f () =
      Backend.set_font_size vp.backend (ts /. usr_ts *. vp.square_side)
    in
    add_instruction f vp

  let set_rel_mark_size_direct vp ms () =
    vp.mark_size <- (ms /. usr_ms *. vp.square_side)

  let set_rel_mark_size vp ms =
    add_instruction (set_rel_mark_size_direct vp ms) vp

  let get_line_width vp =
    (Sizes.get_lw vp.sizes) *. usr_lw
  let get_font_size vp =
    (Sizes.get_ts vp.sizes) *. usr_ts
  let get_mark_size vp =
    (Sizes.get_marks vp.sizes) *. usr_ms


(* ......................................................................... *)

  let lower_left_corner vp = Coordinate.to_device vp.coord_device ~x:0. ~y:0.

  let upper_right_corner vp = Coordinate.to_device vp.coord_device ~x:1. ~y:1.

  let dimensions vp = Coordinate.to_device_distance vp.coord_device ~dx:1. ~dy:1.

  let set_global_color vp c =
    add_instruction (fun () -> Backend.set_color vp.backend c) vp

  let set_global_line_cap vp lc =
    add_instruction (fun () -> Backend.set_line_cap vp.backend lc) vp

  let set_global_dash vp x y =
    add_instruction (fun () -> Backend.set_dash vp.backend x y) vp

  let set_global_line_join vp join =
    add_instruction (fun () -> Backend.set_line_join vp.backend join) vp

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

  let stroke_direct path vp coord_name () =
    let coord = get_coord_from_name vp coord_name in
    let ctm = Coordinate.use vp.backend coord in
    Path.stroke_on_backend path vp.backend;
    Coordinate.restore vp.backend ctm

  let stroke_preserve ?path vp coord_name =
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
    add_instruction (stroke_direct path vp coord_name) vp

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

  let to_parent coord (x, y) = Coordinate.to_parent coord ~x ~y
  let from_parent coord (x, y) = Coordinate.from_parent coord ~x ~y

  let rec ortho_from vp coord_name pos = match coord_name with
    | Device -> from_parent vp.coord_orthonormal pos
    | Graph -> ortho_from vp Device (to_parent vp.coord_graph pos)
    | Data -> ortho_from vp Graph (to_parent vp.coord_data pos)
    | Orthonormal -> pos

  let rec data_from vp coord_name pos = match coord_name with
    | Device -> data_from vp Graph (from_parent vp.coord_graph pos)
    | Graph -> from_parent vp.coord_data pos
    | Data -> pos
    | Orthonormal -> data_from vp Device (to_parent vp.coord_orthonormal pos)

  let show_text_direct vp coord_name ?(rotate=0.) ~x ~y pos text () =
    let ctm = Coordinate.use vp.backend vp.coord_orthonormal in
    let x, y = ortho_from vp coord_name (x, y) in
    Backend.show_text vp.backend ~rotate ~x ~y pos text;
    Coordinate.restore vp.backend ctm

  let show_text vp coord_name ?(rotate=0.) ~x ~y pos text =
    (* auto_fit if Data *)
    if coord_name = Data then begin
      let ctm = Coordinate.use vp.backend vp.coord_orthonormal in
      let rect = Backend.text_extents vp.backend text in
      Coordinate.restore vp.backend ctm;
      let w, h = rect.Matrix.w, rect.Matrix.h in
      let module B = Backend in
      let tr_x = match pos with
        | B.CC | B.CT | B.CB -> w /. 2.
        | B.LC | B.LT | B.LB -> w
        | B.RC | B.RT | B.RB -> 0.
      and tr_y = match pos with
        | B.CC | B.LC | B.RC -> h /. 2.
        | B.CT | B.LT | B.RT -> h
        | B.CB | B.LB | B.RB -> 0.
      in
      let mat = Matrix.make_translate (-. tr_x) (-. tr_y) in
      Matrix.rotate mat rotate;
      Matrix.translate mat (tr_x -. x) (tr_y -. y);
      let ext = Matrix.transform_rectangle mat rect in
      let x, y = ext.Matrix.x, ext.Matrix.y
      and w, h = ext.Matrix.w, ext.Matrix.h in
      let x0, y0 = data_from vp Orthonormal (x, y)
      and xend, yend = data_from vp Orthonormal (x +. w, y +. h) in
      auto_fit vp x0 y0 xend yend
    end;
    add_instruction (show_text_direct vp coord_name ~rotate ~x ~y pos text) vp

  let mark_direct vp ~x ~y name () =
    let ms = vp.mark_size /. vp.square_side in
    let x, y = ortho_from vp Data (x, y) in
    let coord = Coordinate.make_translate vp.coord_orthonormal
      (x -. ms /. 2.) (y -. ms /. 2.) in
    Coordinate.scale coord ms ms;
    let ctm = Coordinate.use vp.backend coord in
    Pointstyle.render name vp.backend;
    Coordinate.restore vp.backend ctm

  let mark vp ~x ~y name =
    auto_fit vp x y x y; (* TODO we want all the mark to be included *)
    add_instruction (mark_direct vp ~x ~y name) vp

  let add_x_axis ?(major=("tic_up",5.)) ?(minor=("tic_up",2.))
      ?(tics=Tics.Auto (Tics.Number 5)) ?(offset=Axes.Absolute 0.)
      ?(sign=Axes.Positive) vp =
    Axes.add_axis major minor tics offset sign (vp.axes_system.Axes.x)

  let add_y_axis ?(major=("tic_right",5.)) ?(minor=("tic_right",2.))
      ?(tics=Tics.Auto (Tics.Number 5)) ?(offset=Axes.Absolute 0.)
      ?(sign=Axes.Positive) vp =
    Axes.add_axis major minor tics offset sign (vp.axes_system.Axes.y)

  let draw_axes vp =
    add_instruction (fun () -> Axes.draw_axes vp) vp

  let box vp =
    add_x_axis ~offset:(Axes.Absolute 0.) vp;
    add_x_axis ~offset:(Axes.Absolute 1.) ~major:("tic_down", 5.)
      ~minor:("tic_down", 2.) ~sign:Axes.Negative vp;
    add_y_axis ~offset:(Axes.Absolute 0.) vp;
    add_y_axis ~offset:(Axes.Absolute 1.) ~major:("tic_left", 5.)
      ~minor:("tic_left", 2.) ~sign:Axes.Negative vp;
    draw_axes vp

  let cross vp =
    add_x_axis ~offset:(Axes.Absolute 0.5) ~major:("|", 2.)
      ~minor:("|", 1.) vp;
    add_y_axis ~offset:(Axes.Absolute 0.5) ~major:("-", 2.)
      ~minor:("-", 1.) vp;
    draw_axes vp

end
