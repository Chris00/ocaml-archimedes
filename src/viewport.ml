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

  type axis = {
    (* Range used for graphic purpose. *)
    (* TODO: check if it is better to use a variant type for auto_x0. *)
    mutable gx0: float;     mutable auto_x0: bool;
    mutable gxend: float;   mutable auto_xend: bool;
    (* Real extents of data. *)
    mutable x0: float;
    mutable xend: float;
    mutable log: bool;
    mutable orientation: sign;
    mutable viewports: Viewport.t list
  }

  type t = {
    mutable x: axis;
    mutable y: axis;
  }

  val default_axis: unit -> axis
  val default_axes_system: unit -> t

  val fit_axis : axis -> unit
  val resize_axis : axis -> float -> unit

  val add_viewport : axis -> Viewport.t -> unit

end
= struct
  module V = Viewport
  module B = Backend

  type sign = Positive | Negative

  type axis = {
    mutable gx0: float;     mutable auto_x0: bool;
    mutable gxend: float;   mutable auto_xend: bool;
    mutable x0: float;
    mutable xend: float;
    mutable log: bool;
    mutable orientation: sign;
    mutable viewports: Viewport.t list
  }

  type t = {
    mutable x: axis;
    mutable y: axis;
  }

  let default_axis () =
    { gx0 = 0.; gxend = 1.; auto_x0 = true; auto_xend = true;
      x0 = infinity; xend = -.infinity;
      log = false; orientation = Positive; viewports = [] }

  let default_axes_system () =
    { x = default_axis ();
      y = default_axis () }

  let fit_axis axis =
    match axis.auto_x0, axis.auto_xend with
    | false, false -> ()
    | false, true ->
      (* TODO: test if using the magnitude of the values for interval
         epsilon is good looking or not. *)
      axis.gxend <- max axis.xend (axis.gx0 +. 0.01);
    | true, false ->
      axis.gx0 <- min axis.x0 (axis.gxend -. 0.01);
    | true, true ->
      assert (axis.x0 = infinity || axis.x0 > -.infinity);
      assert (axis.xend = -.infinity || axis.xend < infinity);
      if axis.x0 = infinity && axis.xend = -.infinity then begin
        axis.gx0 <- 0.;
        axis.gxend <- 1.
      end else begin
        axis.gx0 <- min axis.x0 (axis.xend -. 0.01);
        axis.gxend <- max axis.xend (axis.x0 +. 0.01)
      end

  let resize_axis axis size =
    match axis.auto_x0, axis.auto_xend with
    | false, false | true, true ->
      axis.gx0 <- (axis.gx0 +. axis.gxend -. size) *. 0.5;
      axis.gxend <- (axis.gx0 +. axis.gxend +. size) *. 0.5
    | false, true ->
      axis.gxend <- axis.gx0 +. size
    | true, false ->
      axis.gx0 <- axis.gxend -. size

  let add_viewport axis vp =
    axis.viewports <- vp :: axis.viewports

end
and Viewport : sig
  type save_data

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
    mutable font_size: float;
    mutable color: Color.t;
    mutable instructions: (unit -> unit) Queue.t;
    mutable immediate_drawing: bool;
    redim: t -> float -> float -> unit;
    mutable saves: save_data list
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

  val lower_left_corner : t -> float * float
  val upper_right_corner : t -> float * float
  val dimensions : t -> float * float (* returns (w, h) *)
    (* set_global_param set param of backend and then of all viewports *)
  val set_color : t -> Color.t -> unit
  val set_global_line_cap : t -> Backend.line_cap -> unit
  val set_global_dash : t -> float -> float array -> unit
  val set_global_line_join : t -> Backend.line_join -> unit
    (*  val set_global_line_width : t -> float -> unit*)
  val get_color : t -> Color.t
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
  val fill_preserve : ?path:Path.t -> t -> coord_name -> unit
  val fill : ?path:Path.t -> t -> coord_name -> unit
  val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
    (*  val save_vp : t -> unit
        val restore_vp : t -> unit*)
  val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit
  val show_text :
    t -> coord_name ->
    ?rotate:float ->
    x:float -> y:float -> Backend.text_position -> string -> unit
    (*  val text_extents : t -> string -> rectangle*)
  val ortho_from : t -> coord_name -> float * float -> float * float
  val data_from : t -> coord_name -> float * float -> float * float
  val mark : t -> x:float -> y:float -> string -> unit
    (* val mark_extents : t -> string -> rectangle *)

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
  val path_direct : t -> x:float -> y:float -> Path.t -> unit -> unit
  val save_direct : t -> unit -> unit
  val restore_direct : t -> unit -> unit

  val xrange : t -> float -> float -> unit
  val yrange : t -> float -> float -> unit

  val resize_xaxis : t -> float -> unit
  val resize_yaxis : t -> float -> unit
  val orthonormalize_axes : t -> unit

  val set_xlog : t -> bool -> unit
  val set_ylog : t -> bool -> unit

  val xmin : t -> float
  val xmax : t -> float
  val ymin : t -> float
  val ymax : t -> float
  val xlog : t -> bool
  val ylog : t -> bool
  val close : t -> unit

  val add_instruction : (unit -> unit) -> t -> unit
  val do_instructions : t -> unit

  val auto_fit : t -> float -> float -> float -> float -> unit

  val save : t -> unit
  val restore : t -> unit
end
= struct
  type save_data = {
    lw: float; fs: float; ms: float;
    c: Color.t;
    line_cap: Backend.line_cap;
    dash: float array * float;
    line_join: Backend.line_join;
  }

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
    mutable font_size: float;
    (* The current color, shared by all the "root" descending *)
    mutable color: Color.t;
    (* An instruction is a "thing" to plot on the device, we memorize
       their order to replot in case of necessity *)
    mutable instructions: (unit -> unit) Queue.t;

    (* Draw immediately or wait for closing ? *)
    mutable immediate_drawing: bool;

    (* Redimension function: when the parent viewport grows of x and y,
       what should subsequent coordinates become ? This function must set
       them correctly *)
    redim: t -> float -> float -> unit;

    (* internal *)
    mutable saves: save_data list;
  }

  type coord_name = Device | Graph | Data | Orthonormal

  (* Multiplier to get "user-friendly" values (e.g. 12pt instead of 0.024) *)
  let usr_lw, usr_ts, usr_ms = 500., 500., 100.
  let def_lw, def_ts, def_ms = 0.002, 0.024, 0.01

(* General functions
 ***********************************************************************)

  let get_coord_from_name vp = function
    | Device -> vp.coord_device
    | Graph -> vp.coord_graph
    | Data -> vp.coord_data
    | Orthonormal -> vp.coord_orthonormal

  let to_parent coord (x, y) = Coordinate.to_parent coord ~x ~y
  let from_parent coord (x, y) = Coordinate.from_parent coord ~x ~y

  let data_norm_log axes_system (x, y) =
    let axis_norm_log axis x =
      if axis.Axes.log then
        let xmin, xmax = axis.Axes.gx0, axis.Axes.gxend in
        (log (x /. xmin) /. log (xmax /. xmin)) *. (xmax -. xmin) +. xmin
      else x
    in
    (axis_norm_log axes_system.Axes.x x, axis_norm_log axes_system.Axes.y y)

  let rec ortho_from vp coord_name pos = match coord_name with
    | Device ->
        ortho_from vp Orthonormal (from_parent vp.coord_orthonormal pos)
    | Graph ->
        ortho_from vp Device (to_parent vp.coord_graph pos)
    | Data ->
        let pos = data_norm_log vp.axes_system pos in
        ortho_from vp Graph (to_parent vp.coord_data pos)
    | Orthonormal ->
        pos

  let data_unnorm_log axes_system (x, y) =
    let axis_unnorm_log axis x =
      if axis.Axes.log then
        let x0, xend = axis.Axes.gx0, axis.Axes.gxend in
        exp ((x -. x0) /. (xend -. x0) *. log (xend /. x0) +. log x0)
      else x
    in
    (axis_unnorm_log axes_system.Axes.x x,
     axis_unnorm_log axes_system.Axes.y y)

  let rec data_from vp coord_name pos = match coord_name with
    | Device -> data_from vp Graph (from_parent vp.coord_graph pos)
    | Graph ->
        let pos = data_unnorm_log vp.axes_system pos in
        from_parent vp.coord_data pos
    | Data -> pos
    | Orthonormal -> data_from vp Device (to_parent vp.coord_orthonormal pos)

  let get_path ?(notransform=false) vp p coord_name =
    let p = match p with
      | None -> vp.path
      | Some p -> p
    in
    if coord_name = Data && not notransform &&
      (vp.axes_system.Axes.x.Axes.log || vp.axes_system.Axes.y.Axes.log) then
      Path.transform p (data_norm_log vp.axes_system)
    else p

(* Primitives
 ***********************************************************************)

  let set_line_width_direct vp lw () =
    Backend.set_line_width vp.backend lw

  let set_font_size_direct vp ts () =
    vp.font_size <- ts;
    Backend.set_font_size vp.backend ts

  let set_mark_size_direct vp ms () =
    vp.mark_size <- ms

  let set_rel_line_width_direct vp lw =
    set_line_width_direct vp (lw /. usr_lw *. vp.square_side)

  let set_rel_font_size_direct vp ts =
    set_font_size_direct vp (ts /. usr_ts *. vp.square_side)

  let set_rel_mark_size_direct vp ms =
    set_mark_size_direct vp (ms /. usr_ms *. vp.square_side)

  let set_color_direct vp color () =
    vp.color <- color;
    Backend.set_color vp.backend color

  let set_line_cap_direct vp lcap () =
    Backend.set_line_cap vp.backend lcap

  (* Fixme: Find more appropriated names for x, y *)
  let set_dash_direct vp x y () =
    Backend.set_dash vp.backend x y

  let set_line_join_direct vp join () =
    Backend.set_line_join vp.backend join

  let stroke_direct ?path vp coord_name () =
    let path = get_path vp path coord_name in
    let coord = get_coord_from_name vp coord_name in
    let ctm = Coordinate.use vp.backend coord in
    Path.stroke_on_backend path vp.backend;
    Coordinate.restore vp.backend ctm

  let fill_direct ?path vp coord_name () =
    let path = get_path vp path coord_name in
    let coord = get_coord_from_name vp coord_name in
    let ctm = Coordinate.use vp.backend coord in
    Path.fill_on_backend path vp.backend;
    Coordinate.restore vp.backend ctm

  let clip_rectangle_direct vp ~x ~y ~w ~h () =
    Backend.clip_rectangle vp.backend x y w h

  let select_font_face_direct vp slant weight family () =
    Backend.select_font_face vp.backend slant weight family

  let show_text_direct vp coord_name ?(rotate=0.) ~x ~y pos text () =
    let ctm = Coordinate.use vp.backend vp.coord_orthonormal in
    let x, y = ortho_from vp coord_name (x, y) in
    Backend.show_text vp.backend ~rotate ~x ~y pos text;
    Coordinate.restore vp.backend ctm

  let orthoinstr_direct vp ~x ~y f =
    let ms = vp.mark_size /. vp.square_side in
    let x, y = ortho_from vp Data (x, y) in
    let coord = Coordinate.make_translate vp.coord_orthonormal
      (x -. ms /. 2.) (y -. ms /. 2.) in
    Coordinate.scale coord ms ms;
    let ctm = Coordinate.use vp.backend coord in
    f vp.backend;
    Coordinate.restore vp.backend ctm

  let path_direct vp ~x ~y path () =
    orthoinstr_direct vp ~x ~y (Path.stroke_on_backend path)

  let mark_direct vp ~x ~y name () =
    orthoinstr_direct vp ~x ~y (Pointstyle.render name)

  let save_direct vp () =
    let save = {
      lw = Backend.get_line_width vp.backend;
      fs = vp.font_size;
      ms = vp.mark_size;
      c = vp.color;
      line_cap = Backend.get_line_cap vp.backend;
      dash = Backend.get_dash vp.backend;
      line_join = Backend.get_line_join vp.backend
    } in
    vp.saves <- save :: vp.saves

  let restore_direct vp () =
    let save = List.hd vp.saves in
    vp.saves <- List.tl vp.saves;
    set_line_width_direct vp save.lw ();
    set_font_size_direct vp save.fs ();
    set_mark_size_direct vp save.ms ();
    set_color_direct vp save.c ();
    set_line_cap_direct vp save.line_cap ();
    let x, y = save.dash in
    set_dash_direct vp y x ();
    set_line_join_direct vp save.line_join ()

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
      font_size = def_ts;
      mark_size = def_ms;
      color = Color.black;
      instructions = Queue.create ();
      immediate_drawing = false;
      redim = (fun _ _ _ -> ());
      square_side = size0;
      saves = []
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
      Coordinate.make_translate coord_device 0.1 0.1 in
    Coordinate.scale coord_graph 0.8 0.8;
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
      font_size = def_ts;
      color = vp.color;
      instructions = Queue.create ();
      immediate_drawing = false;
      redim = redim;
      square_side = size0;
      saves = []
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
    if vp.saves != [] then print_string "Warning: saves list is not empty\n";
    blank vp;
    set_color_direct vp vp.color ();
    Queue.iter (fun f -> f ()) vp.instructions;
    List.iter do_instructions (List.rev vp.children)

  let save vp = add_instruction (save_direct vp) vp
  let restore vp = add_instruction (restore_direct vp) vp

  let close vp =
    let parent = vp.parent in
    parent.children <- List.filter (fun x -> not (x == vp)) parent.children;
    if vp == parent then begin
      do_instructions vp;
      Backend.close vp.backend
    end

(* Axes update functions
 ***********************************************************************)

  let xmin vp = vp.axes_system.Axes.x.Axes.gx0
  let xmax vp = vp.axes_system.Axes.x.Axes.gxend
  let ymin vp = vp.axes_system.Axes.y.Axes.gx0
  let ymax vp = vp.axes_system.Axes.y.Axes.gxend

  let update_coordinate_system vp =
    let x0, xend, y0, yend = xmin vp, xmax vp, ymin vp, ymax vp in
    let m = Matrix.make_scale
      (1. /. (xend -. x0)) (1. /. (yend -. y0))
    in
    Matrix.translate m (-. x0) (-. y0);
    Coordinate.transform vp.coord_data m

  (* Utility function for (x|y)range *)
  (* TODO: Check for valid input ! We do not want nan nor empty
     intervals. *)
  let update_axis vp axis gx0 gxend =
    axis.Axes.auto_x0 <- is_nan_or_inf gx0;
    if not (is_nan_or_inf gx0) then axis.Axes.gx0 <- gx0;
    axis.Axes.auto_xend <- is_nan_or_inf gxend;
    if not (is_nan_or_inf gxend) then axis.Axes.gxend <- gxend;
    List.iter update_coordinate_system axis.Axes.viewports;
    if vp.immediate_drawing then
    List.iter do_instructions axis.Axes.viewports

  let xrange vp = update_axis vp vp.axes_system.Axes.x
  let yrange vp = update_axis vp vp.axes_system.Axes.y

  let set_xlog vp v = vp.axes_system.Axes.x.Axes.log <- v
  let set_ylog vp v = vp.axes_system.Axes.y.Axes.log <- v

  let resize_axis vp axis size =
    Axes.resize_axis axis size;
    List.iter update_coordinate_system axis.Axes.viewports;
    if vp.immediate_drawing then
      List.iter do_instructions axis.Axes.viewports

  let resize_xaxis vp = resize_axis vp vp.axes_system.Axes.x
  let resize_yaxis vp = resize_axis vp vp.axes_system.Axes.y

  (* TODO: add an option on autofit to auto_orthonormalize. *)
  let orthonormalize_axes vp =
    let ratio =
      let w, h = ortho_from vp Device (1., 1.) in
      w /. h
    in
    let xaxis = vp.axes_system.Axes.x
    and yaxis = vp.axes_system.Axes.y in
    let xsize = xaxis.Axes.gxend -. xaxis.Axes.gx0
    and yscaled = (yaxis.Axes.gxend -. yaxis.Axes.gx0) *. ratio in
    if xsize > yscaled then
      resize_yaxis vp (xsize /. ratio)
    else if xsize < yscaled then
      resize_xaxis vp yscaled

  let auto_fit vp x0 y0 x1 y1 =
    let axes = vp.axes_system in
    let xaxis = axes.Axes.x
    and yaxis = axes.Axes.y in
    let x0', x1', y0', y1' = min x0 x1, max x0 x1, min y0 y1, max y0 y1 in
    let xupdated = ref false in
    let yupdated = ref false in
    if xaxis.Axes.auto_x0 then
      if is_nan_or_inf xaxis.Axes.x0 || x0' < xaxis.Axes.x0 then
        (xaxis.Axes.x0 <- x0'; xupdated := true);
    if xaxis.Axes.auto_xend then
      if is_nan_or_inf xaxis.Axes.xend || x1' > xaxis.Axes.xend then
        (xaxis.Axes.xend <- x1'; xupdated := true);
    if yaxis.Axes.auto_x0 then
      if is_nan_or_inf yaxis.Axes.x0 || y0' < yaxis.Axes.x0 then
        (yaxis.Axes.x0 <- y0'; yupdated := true);
    if yaxis.Axes.auto_xend then
      if is_nan_or_inf yaxis.Axes.xend || y1' > yaxis.Axes.xend then
        (yaxis.Axes.xend <- y1'; yupdated := true);
    let l1 =
      if !xupdated then begin
        Axes.fit_axis xaxis;
        axes.Axes.x.Axes.viewports
      end else [] in
    let l2 =
      if !yupdated then begin
        Axes.fit_axis yaxis;
        axes.Axes.y.Axes.viewports
      end else [] in
    (* We want to merge the 2 lists without duplicates *)
    let l2' = List.filter (fun x -> not (List.exists (( == ) x) l1)) l2 in
    let l = List.rev_append l1 l2' in
    List.iter update_coordinate_system l;
    if vp.immediate_drawing then
      (* TODO Is there a way to optimize that bunch of code ? *)
      List.iter do_instructions l

(* Synchronization
 ***********************************************************************)

  let sync ?(x=true) ?(y=true) vp vp_base =
    if x then begin
      vp.axes_system.Axes.x <- vp_base.axes_system.Axes.x;
      Axes.add_viewport vp_base.axes_system.Axes.x vp
    end;
    if y then begin
      vp.axes_system.Axes.y <- vp_base.axes_system.Axes.y;
      Axes.add_viewport vp_base.axes_system.Axes.y vp
    end


(* Layouts
 ***********************************************************************)

(* TODO ! *)
  (* Uniform grid; redim: identity *)
  let layout_grid ?(syncs=(false, false, false, false)) ?(axes_sys=false)
      vp rows cols =
    let cols_sync_x, cols_sync_y, rows_sync_x, rows_sync_y = syncs in
    let redim _ _ _ = () in
    let xstep = 1. /. (float cols) and ystep = 1. /. (float rows) in
    let ret = Array.make (rows * cols) vp in
    let init_viewport i =
      let x = i / rows and y = i mod rows in
      let xmin = float x *. xstep
      and ymin = float y *. ystep in
      let xmax = xmin +. xstep
      and ymax = ymin +. ystep in
      ret.(i) <- make ~axes_sys vp Device xmin xmax ymin ymax redim;
      if i >= rows then
        sync ~x:cols_sync_x ~y:cols_sync_y ret.(i) ret.(i mod rows);
      if i mod rows > 0 then
        sync ~x:rows_sync_x ~y:rows_sync_y ret.(i) ret.(i - (i mod rows))
    in
    for i = 0 to rows * cols - 1 do init_viewport i done;
    ret

  let layout_rows ?(syncs=false, false) ?(axes_sys=false) vp n =
    let sync_x, sync_y = syncs in
    layout_grid ~syncs:(false, false, sync_x, sync_y) ~axes_sys vp n 1

  let layout_columns ?(syncs=false, false) ?(axes_sys=false) vp n =
    let sync_x, sync_y = syncs in
    layout_grid ~syncs:(sync_x, sync_y, false, false) ~axes_sys vp 1 n

  let fixed_left ?(axes_sys=false) init_prop vp =
    let redim_fixed vp xfactor _ = begin
      let coord = vp.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.;
    end in
    let vp_fixed =
      make ~axes_sys vp Device 0. init_prop 0. 1. redim_fixed in
    let redim vp xfactor _ = begin
      let coord = vp.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.;
      let fixed_right, _ =
        Coordinate.to_parent vp_fixed.coord_device ~x:1. ~y:0. in
      let vp_left, _ = Coordinate.to_parent vp.coord_device ~x:0. ~y:0. in
      Coordinate.translate coord (vp_left -. fixed_right) 0.
    end in
    let vp' = make ~axes_sys vp Device init_prop 1. 0. 1. redim in
    (vp_fixed, vp')

  let fixed_right ?(axes_sys=false) init_prop vp =
    let redim_fixed vp xfactor _ = begin
      let coord = vp.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.;
      Coordinate.translate
        coord (-. (fst (Coordinate.to_parent coord ~x:1. ~y:0.))) 0.
    end in
    let vp_fixed =
      make ~axes_sys vp Device init_prop 1. 0. 1. redim_fixed in
    let redim vp xfactor _ = begin
      let coord = vp.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.
    end in
    let vp' = make ~axes_sys vp Device 0. init_prop 0. 1. redim in
    (vp_fixed, vp')

  let fixed_top ?(axes_sys=false) init_prop vp =
    let redim_fixed vp _ yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor);
    end in
    let vp_fixed =
      make ~axes_sys vp Device 0. 1. (1. -. init_prop) 1. redim_fixed
    in
    let rec redim vp _ yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor);
      let _, fixed_bottom =
        Coordinate.to_parent vp_fixed.coord_device ~x:0. ~y:1. in
      let _, vp_top = Coordinate.to_parent vp.coord_device ~x:0. ~y:0. in
      Coordinate.translate coord (vp_top -. fixed_bottom) 0.
    end in
    let vp' = make ~axes_sys vp Device 0. 1. 0. (1. -. init_prop) redim in
    (vp_fixed, vp')

  let fixed_bottom ?(axes_sys=false) init_prop vp =
    let redim_fixed vp _ yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor);
      Coordinate.translate
        coord 0. (-. (snd (Coordinate.to_parent coord ~x:0. ~y:1.)))
    end in
    let vp_fixed =
      make ~axes_sys vp Device 0. 1. 0. init_prop redim_fixed in
    let rec redim vp _ yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor)
    end in
    let vp' = make ~axes_sys vp Device 0. 1. init_prop 1. redim in
    (vp_fixed, vp')

  (* Border layouts, of desired sizes *)
  let layout_borders ?(north=0.) ?(south=0.) ?(west=0.) ?(east=0.)
      ?(axes_sys=false) vp =
    if south +. north >= 1. || east +. west >= 1. then
      invalid_arg "Archimedes.Viewport.Viewport.layout_borders: \
                   invalid borders dimensions (sum need to be < 1).";
    let east, vp =
      if east > 0. then fixed_right ~axes_sys east vp else vp, vp
    in
    let west, vp =
      if west > 0. then fixed_left ~axes_sys west vp else vp, vp
    in
    let south, vp =
      if south > 0. then fixed_bottom ~axes_sys south vp else vp, vp
    in
    let north, center =
      if north > 0. then fixed_top ~axes_sys north vp else vp, vp
    in
    (north, south, west, east, center)


(* Data that depends directly on viewports
 ***********************************************************************)

  let set_line_width vp lw =
    add_instruction (set_line_width_direct vp lw) vp

  let set_font_size vp ts =
    add_instruction (set_font_size_direct vp ts) vp

  let set_mark_size vp ms =
    add_instruction (set_mark_size_direct vp ms) vp

  let set_rel_line_width vp lw =
    add_instruction (set_rel_line_width_direct vp lw) vp

  let set_rel_font_size vp ts =
    add_instruction (set_rel_font_size_direct vp ts) vp

  let set_rel_mark_size vp ms =
    add_instruction (set_rel_mark_size_direct vp ms) vp

  (* Fixme: Are sizes still used ? *)
  let get_line_width vp = (Sizes.get_lw vp.sizes) *. usr_lw
  let get_font_size vp = (Sizes.get_ts vp.sizes) *. usr_ts
  let get_mark_size vp = (Sizes.get_marks vp.sizes) *. usr_ms

(* ......................................................................... *)

  let lower_left_corner vp =
    Coordinate.to_device vp.coord_device ~x:0. ~y:0.

  let upper_right_corner vp =
    Coordinate.to_device vp.coord_device ~x:1. ~y:1.

  let dimensions vp =
    Coordinate.to_device_distance vp.coord_device ~dx:1. ~dy:1.

  let set_color vp c =
    add_instruction (set_color_direct vp c) vp

  let set_global_line_cap vp lc =
    add_instruction (set_line_cap_direct vp lc) vp

  let set_global_dash vp x y =
    add_instruction (set_dash_direct vp x y) vp

  let set_global_line_join vp join =
    add_instruction (set_line_join_direct vp join) vp

  let get_line_cap vp = Backend.get_line_cap vp.backend
  let get_dash vp = Backend.get_dash vp.backend
  let get_line_join vp = Backend.get_line_join vp.backend

  let get_color vp = vp.color

(* Viewport path manipulation
 ***********************************************************************)

  let move_to vp = Path.move_to vp.path
  let line_to vp = Path.line_to vp.path
  let rel_move_to vp = Path.rel_move_to vp.path
  let rel_line_to vp = Path.rel_line_to vp.path

  let curve_to vp = Path.curve_to vp.path
  let rectangle vp = Path.rectangle vp.path
  let arc vp = Path.arc vp.path

  let close_path vp = Path.close vp.path
  let clear_path vp = Path.clear vp.path

  let path_extents vp = Path.extents vp.path

  let fit_path vp path =
    let e = Path.extents path in
    let x0 = e.Matrix.x
    and y0 = e.Matrix.y in
    let x1 = x0 +. e.Matrix.w
    and y1 = y0 +. e.Matrix.h in
    auto_fit vp x0 y0 x1 y1

  let stroke_preserve ?path vp coord_name =
    let path = get_path ~notransform:true vp path coord_name in
    if coord_name = Data then
      fit_path vp path;
    let path = Path.copy path in
    add_instruction (stroke_direct ~path vp coord_name) vp

  let stroke ?path vp coord_name =
    stroke_preserve ?path vp coord_name;
    if path = None then add_instruction (fun () -> Path.clear vp.path) vp

  let fill_preserve ?path vp coord_name =
    let path = get_path ~notransform:true vp path coord_name in
    if coord_name = Data then
      fit_path vp path;
    let path = Path.copy path in
    add_instruction (fill_direct ~path vp coord_name) vp

  let fill ?path vp coord_name =
    fill_preserve ?path vp coord_name;
    if path = None then add_instruction (fun () -> Path.clear vp.path) vp

  let clip_rectangle vp ~x ~y ~w ~h =
    add_instruction (clip_rectangle_direct vp ~x ~y ~w ~h) vp

(* Text, marks
 ***********************************************************************)

  let select_font_face vp slant weight family =
    add_instruction (select_font_face_direct vp slant weight family) vp

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

  let mark vp ~x ~y name =
    auto_fit vp x y x y; (* TODO we want all the mark to be included *)
    add_instruction (mark_direct vp ~x ~y name) vp

  let xlog vp = vp.axes_system.Axes.x.Axes.log
  let ylog vp = vp.axes_system.Axes.y.Axes.log

end
