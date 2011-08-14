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

open Utils

module rec Axes : sig
  type sign = Positive | Negative

  type 'a sync = {
    mutable value: 'a;
    mutable vps: Viewport.t list
  }

  type range = {
    mutable x0: float;     mutable auto_x0: bool;
    mutable xend: float;   mutable auto_xend: bool;
    (* Real extents of data. *)
    mutable data_x0: float;
    mutable data_xend: float
  }

  type axis = {
    mutable gx0: float;
    mutable gxend: float;
    mutable unit_size: float sync;
    mutable range: range sync;
    mutable log: bool;
    mutable orientation: sign;
  }

  type t = {
    mutable x: axis;
    mutable y: axis;
    mutable ratio: float option sync
  }

  val default_axes_system: unit -> t

  val fit_range : range -> unit
  val resize_axis : axis -> float -> unit
  val update_axes_system : t -> float -> float -> unit

end = struct
  module V = Viewport
  module B = Backend

  type sign = Positive | Negative

  type 'a sync = {
    mutable value: 'a;
    mutable vps: Viewport.t list
  }

  type range = {
    mutable x0: float;     mutable auto_x0: bool;
    mutable xend: float;   mutable auto_xend: bool;
    mutable data_x0: float;
    mutable data_xend: float
  }

  type axis = {
    mutable gx0: float;
    mutable gxend: float;
    mutable unit_size: float sync;
    mutable range: range sync;
    mutable log: bool;
    mutable orientation: sign;
  }

  type t = {
    mutable x: axis;
    mutable y: axis;
    mutable ratio: float option sync  }

  let default_sync def_val =
    { value = def_val; vps = [] }

  let default_range () =
    { x0 = 0.; xend = 1.; auto_x0 = true; auto_xend = true;
      data_x0 = infinity; data_xend = -.infinity }

  let default_axis () =
    { gx0 = 0.; gxend = 1.; unit_size = default_sync infinity;
      range = default_sync (default_range ());
      log = false; orientation = Positive }

  let default_axes_system () =
    { x = default_axis ();
      y = default_axis ();
      ratio = default_sync None }

  let fit_range range =
    match range.auto_x0, range.auto_xend with
    | false, false -> ()
    | false, true ->
        (* TODO: test if using the magnitude of the values for interval
           epsilon is good looking or not. *)
        range.xend <- max range.data_xend (range.x0 +. 0.01);
    | true, false ->
        range.x0 <- min range.data_x0 (range.xend -. 0.01);
    | true, true ->
        assert (range.data_x0 = infinity || range.data_x0 > -.infinity);
        assert (range.data_xend = -.infinity || range.data_xend < infinity);
        if range.data_x0 = infinity && range.data_xend = -.infinity then begin
          range.x0 <- 0.;
          range.xend <- 1.
        end else begin
          range.x0 <- min range.data_x0 (range.data_xend -. 0.01);
          range.xend <- max range.data_xend (range.data_x0 +. 0.01)
        end

  let resize_axis axis size =
    let r = axis.range.value in
    let size = max size (r.xend -. r.x0) in
    match r.auto_x0, r.auto_xend with
    | false, false | true, true ->
      axis.gx0 <- (r.x0 +. r.xend -. size) *. 0.5;
      axis.gxend <- (r.x0 +. r.xend +. size) *. 0.5
    | false, true ->
      axis.gxend <- r.x0 +. size
    | true, false ->
      axis.gx0 <- r.xend -. size

  let update_axis axis unit_size size =
    resize_axis axis (size /. unit_size)

  let update_axes_system axes sizex sizey =
    let xunit = axes.x.unit_size.value
    and yunit = axes.y.unit_size.value in
    match axes.ratio.value with
    | None ->
      (* FIXME: When only one axis is updated, we should only update that one
         axis. *)
      (* sizes should have been choosed so that
         [size / unit_size >= xend - x0] *)
      resize_axis axes.x (sizex /. xunit);
      resize_axis axes.y (sizey /. yunit)
    | Some r ->
      let xscaled = yunit /. r in
      if xscaled <= xunit then begin
        resize_axis axes.x (sizex /. xscaled);
        resize_axis axes.y (sizey /. yunit)
      end else begin
        resize_axis axes.x (sizex /. xunit);
        resize_axis axes.y (sizey /. (xunit *. r))
      end

end
and Viewport : sig
  type save_data

  type t = {
    backend: Backend.t;
    parent: t;
    bg_color: Color.t;  (* background color *)
    mutable children: t list;
    mutable coord_device: Coordinate.t;
    mutable coord_graph: Coordinate.t;
    mutable coord_orthonormal: Coordinate.t;
    mutable coord_data: Coordinate.t;
    mutable square_side: float;
    path: Path.t;  (* current path, one per viewport *)
    mutable axes_system: Axes.t;
    mutable xaxis_size: float;
    mutable yaxis_size: float;
    mutable line_width: float;
    mutable font_size: float;
    mutable mark_size: float;
    mutable color: Color.t;
    mutable instructions: (unit -> unit) Queue.t;
    mutable immediate_drawing: bool;
    redim: t -> float -> float -> unit;
    mutable clip: bool;
    mutable saves: save_data list;
  }
  type coord_name = [`Device | `Graph | `Data | `Orthonormal]
  val get_coord_from_name : t -> coord_name -> Coordinate.t
  val init : ?lines:float -> ?text:float -> ?marks:float ->
    ?bg:Color.t -> ?w:float -> ?h:float -> ?dirs:string list -> string list -> t
  val make : t -> ?lines:float -> ?text:float -> ?marks:float ->
    ?redim:(t -> float -> float -> unit) ->
    ?coord:[`Device | `Graph | `Orthonormal] ->
    float -> float -> float -> float -> t

  val show : t -> unit

  val get_backend : t -> Backend.t

  val desync_ratio : t -> unit
  val sync_ratio : t -> t -> unit
  val desync_range : ?x:bool -> ?y:bool -> t -> unit
  val sync_range : ?x:bool -> ?y:bool -> t -> t -> unit
  val desync_unit_size : ?x:bool -> ?y:bool -> t -> unit
  val sync_unit_size : ?x:bool -> ?y:bool -> t -> t -> unit
  val sync : ?x:bool -> ?y:bool -> t -> t -> unit

  val grid : ?syncs:(bool * bool * bool * bool) ->
    t -> int -> int -> t array array
  val rows : ?syncs:(bool * bool) -> t ->
    int -> t array
  val columns : ?syncs:(bool * bool) -> t ->
    int -> t array
  val fixed_left : float -> t -> t * t
  val fixed_right : float -> t -> t * t
  val fixed_top : float -> t -> t * t
  val fixed_bottom : float -> t -> t * t
  val layout_borders : ?north:float -> ?south:float -> ?west:float ->
    ?east:float -> t -> t * t * t * t * t

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
  val get_background_color : t -> Color.t
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
  val stroke_preserve : ?path:Path.t -> ?fit:bool -> t -> coord_name -> unit
  val stroke : ?path:Path.t -> ?fit:bool -> t -> coord_name -> unit
  val fill_preserve : ?path:Path.t -> ?fit:bool -> t -> coord_name -> unit
  val fill : ?path:Path.t -> ?fit:bool -> t -> coord_name -> unit
  val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
    (*  val save_vp : t -> unit
        val restore_vp : t -> unit*)
  val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit
  val text :
    t -> ?coord:coord_name ->
    ?rotate:float ->
    float -> float -> ?pos:Backend.text_position -> string -> unit
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

  val axes_ratio : t -> float -> unit
  val xrange : t -> float -> float -> unit
  val yrange : t -> float -> float -> unit
  val xlabel : t -> string -> unit
  val ylabel : t -> string -> unit
  val title  : t -> string -> unit

  val set_xlog : t -> bool -> unit
  val set_ylog : t -> bool -> unit

  val xmin : t -> float
  val xmax : t -> float
  val ymin : t -> float
  val ymax : t -> float
  val xlog : t -> bool
  val ylog : t -> bool
  val close : t -> unit

  val add_instruction : t -> (unit -> unit) -> unit
  val do_instructions : t -> unit

  val auto_fit : t -> float -> float -> float -> float -> unit
  val fit : t -> Matrix.rectangle -> unit

  val save : t -> unit
  val restore : t -> unit
end = struct
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
    bg_color: Color.t;  (* background color *)
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
    (* Size of axes in backend coordinates. *)
    mutable xaxis_size: float;
    mutable yaxis_size: float;
    (* Sizes in backend coordinates. *)
    mutable line_width: float;
    mutable font_size: float;
    mutable mark_size: float;
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

    (* Should we clip the elements out of the viewport ? *)
    mutable clip: bool;

    (* internal *)
    mutable saves: save_data list;
  }

  type coord_name = [`Device | `Graph | `Data | `Orthonormal]

  (* Multiplier to get "user-friendly" values (e.g. 12pt instead of 0.024) *)
  let usr_lw, usr_ts, usr_ms = 500., 500., 100.

  let def_lw, def_ts, def_ms = 1., 12., 7.
  let def_color = Color.black
  let def_line_cap = Backend.BUTT
  let def_dash = (0., [| |])
  let def_line_join = Backend.JOIN_MITER

(* General functions
 ***********************************************************************)

  let xmin vp = vp.axes_system.Axes.x.Axes.gx0
  let xmax vp = vp.axes_system.Axes.x.Axes.gxend
  let ymin vp = vp.axes_system.Axes.y.Axes.gx0
  let ymax vp = vp.axes_system.Axes.y.Axes.gxend

  let get_coord_from_name vp = function
    | `Device -> vp.coord_device
    | `Graph -> vp.coord_graph
    | `Data -> vp.coord_data
    | `Orthonormal -> vp.coord_orthonormal

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
    | `Device ->
        ortho_from vp `Orthonormal (from_parent vp.coord_orthonormal pos)
    | `Graph ->
        ortho_from vp `Device (to_parent vp.coord_graph pos)
    | `Data ->
        let pos = data_norm_log vp.axes_system pos in
        ortho_from vp `Graph (to_parent vp.coord_data pos)
    | `Orthonormal ->
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
    | `Device -> data_from vp `Graph (from_parent vp.coord_graph pos)
    | `Graph ->
        let pos = data_unnorm_log vp.axes_system pos in
        from_parent vp.coord_data pos
    | `Data -> pos
    | `Orthonormal -> data_from vp `Device (to_parent vp.coord_orthonormal pos)

  let get_path ?(notransform=false) vp p coord_name =
    let p = match p with
      | None -> vp.path
      | Some p -> p
    in
    if coord_name = `Data && not notransform &&
      (vp.axes_system.Axes.x.Axes.log || vp.axes_system.Axes.y.Axes.log) then
      Path.map p (data_norm_log vp.axes_system)
    else p

  (* Merges two non-sorted lists without duplicates. *)
  let merge l1 l2 =
    let l2' = List.filter (fun x -> not (List.exists (( == ) x) l1)) l2 in
    List.rev_append l1 l2'


(* Primitives
 ***********************************************************************)

  let set_line_width_direct vp lw () =
    vp.line_width <- lw;
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

  let apply_clip vp coord_name =
    if vp.clip then (
      match coord_name with
      | `Data ->
        let x = xmin vp and y = ymin vp in
        Backend.clip_rectangle vp.backend x y (xmax vp -. x) (ymax vp -. y);
      | `Orthonormal ->
        let maxx, maxy = ortho_from vp `Device (1., 1.) in
        Backend.clip_rectangle vp.backend 0. 0. maxx maxy
      | `Device | `Graph ->
        Backend.clip_rectangle vp.backend 0. 0. 1. 1.
    )

  let stroke_direct ?path vp coord_name () =
    let path = get_path vp path coord_name in
    let coord = get_coord_from_name vp coord_name in
    Backend.save vp.backend;
    ignore(Coordinate.use vp.backend coord);
    apply_clip vp coord_name;
    Backend.stroke_path_preserve vp.backend path;
    Backend.restore vp.backend (* remove CTM and clip *)

  let fill_direct ?path vp coord_name () =
    let path = get_path vp path coord_name in
    let coord = get_coord_from_name vp coord_name in
    Backend.save vp.backend;
    ignore(Coordinate.use vp.backend coord);
    apply_clip vp coord_name;
    Backend.fill_path_preserve vp.backend path;
    Backend.restore vp.backend (* remove CTM and clip *)

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
    let x, y = ortho_from vp `Data (x, y) in
    (* FIXME: the idea of coordinate system is that we create them
       and use/update them, not that we create new ones all the time. *)
    let coord = Coordinate.make_translate vp.coord_orthonormal
      (x -. ms /. 2.) (y -. ms /. 2.) in
    Coordinate.scale coord ms ms;
    let ctm = Coordinate.use vp.backend coord in
    f vp.backend;
    Coordinate.restore vp.backend ctm

  let path_direct vp ~x ~y path () =
    orthoinstr_direct vp ~x ~y (fun b -> Backend.stroke_path_preserve b path)

  let mark_direct vp ~x ~y name () =
    orthoinstr_direct vp ~x ~y (Pointstyle.render name)

  let save_direct vp () =
    let save = {
      lw = vp.line_width;
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

(* Basic functions
 ***********************************************************************)

  let get_backend vp = vp.backend

  let initial_scale = 1.

  let blank vp =
    let ctm = Coordinate.use vp.backend vp.coord_device in
    Backend.save vp.backend;
    Backend.clear_path vp.backend;
    Backend.rectangle vp.backend 0. 0. 1. 1.;
    Backend.fill_with_color vp.backend vp.bg_color;
    Backend.restore vp.backend;
    Coordinate.restore vp.backend ctm

  let add_instruction vp f = Queue.push f vp.instructions

  (* Note: if a vp is synchronized with one of its children, this children
     will be redrawn two times. *)
  let rec do_instructions vp =
    if vp.saves <> [] then print_string "Warning: saves list is not empty\n";
    blank vp;
    Queue.iter (fun f -> f ()) vp.instructions;
    List.iter do_instructions (List.rev vp.children)

  let save vp = add_instruction vp (save_direct vp)
  let restore vp = add_instruction vp (restore_direct vp)

  let show vp =
    do_instructions vp; (* => also for children *)
    Backend.show vp.backend

  let close vp =
    let parent = vp.parent in
    parent.children <- List.filter (fun x -> x != vp) parent.children;
    if vp == parent then begin
      do_instructions vp;
      Backend.close vp.backend
    end


(* Data that depends directly on viewports
 ***********************************************************************)

  let set_line_width vp lw =
    vp.line_width <- lw;
    add_instruction vp (set_line_width_direct vp lw)

  let set_font_size vp ts =
    vp.font_size <- ts;
    add_instruction vp (set_font_size_direct vp ts)

  let set_mark_size vp ms =
    vp.mark_size <- ms;
    add_instruction vp (set_mark_size_direct vp ms)

  let set_rel_line_width vp lw =
    vp.line_width <- (lw /. usr_lw *. vp.square_side);
    add_instruction vp (set_rel_line_width_direct vp lw)

  let set_rel_font_size vp ts =
    vp.font_size <- (ts /. usr_ts *. vp.square_side);
    add_instruction vp (set_rel_font_size_direct vp ts)

  let set_rel_mark_size vp ms =
    vp.mark_size <- (ms /. usr_ms *. vp.square_side);
    add_instruction vp (set_rel_mark_size_direct vp ms)

  let get_line_width vp = vp.line_width
  let get_font_size vp = vp.font_size
  let get_mark_size vp = vp.mark_size

(* ......................................................................... *)

  let lower_left_corner vp =
    Coordinate.to_device vp.coord_device ~x:0. ~y:0.

  let upper_right_corner vp =
    Coordinate.to_device vp.coord_device ~x:1. ~y:1.

  let dimensions vp =
    Coordinate.to_device_distance vp.coord_device ~dx:1. ~dy:1.

  let set_clip vp =
    vp.clip = true

  let set_noclip vp =
    vp.clip = false

  let set_color vp c =
    vp.color <- c;  (* one may query the viewport! *)
    add_instruction vp (set_color_direct vp c)

  let set_global_line_cap vp lc =
    add_instruction vp (set_line_cap_direct vp lc)

  let set_global_dash vp x y =
    add_instruction vp (set_dash_direct vp x y)

  let set_global_line_join vp join =
    add_instruction vp (set_line_join_direct vp join)

  (* FIXME: a field in viewport should be added as for line_width... *)
  let get_line_cap vp = Backend.get_line_cap vp.backend
  let get_dash vp = Backend.get_dash vp.backend
  let get_line_join vp = Backend.get_line_join vp.backend

  let get_color vp = vp.color
  let get_background_color vp = vp.bg_color

(* Initialization functions
 ***********************************************************************)

  let init ?(lines=def_lw) ?(text=def_ts) ?(marks=def_ms)
      ?(bg=Color.white) ?(w=640.) ?(h=480.) ?dirs backend_name =
    let backend = Backend.make ?dirs backend_name w h in
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
    let xaxis_size = w *. 0.8
    and yaxis_size = h *. 0.8 in
    let rec viewport = {
      backend = backend;
      parent = viewport;
      bg_color = bg;
      children = [];
      coord_device = coord_device; coord_graph = coord_graph;
      coord_orthonormal =
        Coordinate.make_scale coord_device (size0 /. w) (size0 /. h);
        (* We don't care; will be updated as soon as points are added or we
	   change axes. *)
      coord_data = Coordinate.make_identity coord_graph;
      path = Path.make ();
      axes_system = Axes.default_axes_system ();
      xaxis_size = xaxis_size;
      yaxis_size = yaxis_size;
      line_width = nan; (* They'll be updated right after. *)
      font_size = nan;
      mark_size = nan;
      color = def_color;
      instructions = Queue.create ();
      immediate_drawing = false;
      redim = (fun _ _ _ -> ());
      square_side = size0;
      clip = true;
      saves = []
    } in
    let axes = viewport.axes_system in
    axes.Axes.ratio.Axes.vps <- [ viewport ];
    axes.Axes.x.Axes.unit_size.Axes.vps <- [ viewport ];
    axes.Axes.x.Axes.range.Axes.vps <- [ viewport ];
    axes.Axes.y.Axes.unit_size.Axes.vps <- [ viewport ];
    axes.Axes.y.Axes.range.Axes.vps <- [ viewport ];
    set_line_width viewport lines;
    set_font_size viewport text;
    set_mark_size viewport marks;
    set_color viewport Color.black;
    (*set_line_cap viewport def_line_cap;
    let x, y = def_dash in
    set_dash viewport y x ();
    set_line_join viewport def_line_join;*)
    viewport

  let do_nothing_when_redim _ _ _ = ()

  let make vp ?(lines=def_lw) ?(text=def_ts) ?(marks=def_ms)
      ?(redim=do_nothing_when_redim)
      ?(coord=`Device) xmin xmax ymin ymax =
    let coord_parent = get_coord_from_name vp coord in
    let w, h, size0 =
      (* We can't be sure of the y orientation, so we them in absolute *)
      let xmax', y2' = Coordinate.to_device coord_parent xmax ymax
      and xmin', y1' = Coordinate.to_device coord_parent xmin ymin in
      let w = xmax' -. xmin' and h = abs_float (y2' -. y1') in
      w, h, min w h
    in
    let coord_device =
      Coordinate.make_scale
        (Coordinate.make_translate coord_parent xmin ymin)
        (xmax -. xmin) (ymax -. ymin)
    in
    let coord_graph =
      Coordinate.make_translate coord_device 0.1 0.1 in
    Coordinate.scale coord_graph 0.8 0.8;
    let xaxis_size = w *. 0.8
    and yaxis_size = h *. 0.8 in
    let viewport = {
      backend = vp.backend;
      parent = vp;
      bg_color = vp.bg_color;
      children = [];
      coord_device = coord_device; coord_graph = coord_graph;
      coord_orthonormal =
        Coordinate.make_scale coord_device (size0 /. w) (size0 /. h);
      (* We don't care; will be updated as soon as points are added or we
	 change axes. *)
      coord_data = Coordinate.make_identity coord_graph;
      path = Path.make ();
      axes_system = Axes.default_axes_system ();
      xaxis_size = xaxis_size;
      yaxis_size = yaxis_size;
      line_width = lines;
      mark_size = marks;
      font_size = text;
      color = vp.color;
      instructions = Queue.create ();
      immediate_drawing = false;
      redim = redim;
      square_side = size0;
      clip = true;
      saves = []
    } in
    let axes = viewport.axes_system in
    axes.Axes.ratio.Axes.vps <- [ viewport ];
    axes.Axes.x.Axes.unit_size.Axes.vps <- [ viewport ];
    axes.Axes.x.Axes.range.Axes.vps <- [ viewport ];
    axes.Axes.y.Axes.unit_size.Axes.vps <- [ viewport ];
    axes.Axes.y.Axes.range.Axes.vps <- [ viewport ];
    vp.children <- viewport :: vp.children;
    set_line_width viewport lines;
    set_font_size viewport text;
    set_mark_size viewport marks;
    set_color viewport Color.black;
    (*set_line_cap_direct viewport def_line_cap;
    let x, y = def_dash in
    set_dash viewport y x ();
    set_line_join viewport def_line_join;*)
    viewport

(* Axes update functions
 ***********************************************************************)

  let update_coordinate_system vp =
    let x0, xend, y0, yend = xmin vp, xmax vp, ymin vp, ymax vp in
    let m = Matrix.make_scale
      (1. /. (xend -. x0)) (1. /. (yend -. y0))
    in
    Matrix.translate m (-. x0) (-. y0);
    Coordinate.transform vp.coord_data m

  (* Updates gx0 and gxend for an axes system and then redraw them. *)
  let update_axes_system vp =
    Axes.update_axes_system vp.axes_system vp.xaxis_size vp.yaxis_size;
    update_coordinate_system vp;
    if vp.immediate_drawing then
      do_instructions vp

  let update_ratio vp ratio =
    let sync = vp.axes_system.Axes.ratio in
    sync.Axes.value <- ratio;
    List.iter update_axes_system sync.Axes.vps

  let norm_ratio_auto_axes axes =
    match axes.Axes.ratio.Axes.value with
    | None -> ()
    | Some _ ->
      (* Ratio forces axes systems where one axis is full manual (not
         auto_x0 nor _xend) to have both axes full manual. *)
      let xrange = axes.Axes.x.Axes.range.Axes.value
      and yrange = axes.Axes.y.Axes.range.Axes.value in
      if not (xrange.Axes.auto_x0 || xrange.Axes.auto_xend) then begin
        xrange.Axes.auto_x0 <- false;
        xrange.Axes.auto_xend <- false
      end else
        if not (yrange.Axes.auto_x0 || yrange.Axes.auto_xend) then begin
          xrange.Axes.auto_x0 <- false;
          yrange.Axes.auto_xend <- false
        end

  let axes_ratio vp ratio =
    let ratio = if 0. < ratio && ratio < infinity then Some ratio else None in
    norm_ratio_auto_axes vp.axes_system;
    update_ratio vp ratio

  (* [update_axis_range axis axis_size] updates x0 and xend ranges around
     data and updates unit_size according to the size of the axis on the
     backend. Returns the list of viewports affected by the update. *)
  let update_axis_range axis axis_size =
    Axes.fit_range axis.Axes.range.Axes.value;
    let xrange =
      (axis.Axes.range.Axes.value.Axes.xend
       -. axis.Axes.range.Axes.value.Axes.x0)
    in
    (* A change in range implies a change of the unit vector size. We have
       to update it the maintain the minimal unit size in the
       synchronization group for unit size. *)
    (* TODO: we may want to factorise that with the function to update the
       unit size. *)
    let unit_size = axis_size /. xrange in
    if unit_size < axis.Axes.unit_size.Axes.value then begin
      axis.Axes.unit_size.Axes.value <- unit_size;
      merge axis.Axes.range.Axes.vps axis.Axes.unit_size.Axes.vps
    end else
      axis.Axes.range.Axes.vps

  (* Call update of x0, xend ranges and update of unit_size. Then, call an
     update of gx0 and gxend on affected viewports (with redraw if
     necessary). *)
  let update_axes_ranges vp xupdate yupdate =
    let xaxis = vp.axes_system.Axes.x
    and yaxis = vp.axes_system.Axes.y in
    let l1 = if xupdate then update_axis_range xaxis vp.xaxis_size else [] in
    let l2 = if yupdate then update_axis_range yaxis vp.yaxis_size else [] in
    let l = merge l1 l2 in
    List.iter update_axes_system l

  let auto_fit vp x0 y0 x1 y1 =
    let axes = vp.axes_system in
    let xaxis = axes.Axes.x
    and yaxis = axes.Axes.y in
    let xrange = xaxis.Axes.range.Axes.value
    and yrange = yaxis.Axes.range.Axes.value in
    let x0', x1', y0', y1' = min x0 x1, max x0 x1, min y0 y1, max y0 y1 in
    let xupdated = ref false in
    let yupdated = ref false in
    assert (not (is_nan xrange.Axes.data_x0));
    assert (not (is_nan xrange.Axes.data_xend));
    assert (not (is_nan yrange.Axes.data_x0));
    assert (not (is_nan yrange.Axes.data_xend));
    (* Update data ranges. *)
    if xrange.Axes.auto_x0 && is_finite x0'
      && (is_inf xrange.Axes.data_x0 || x0' < xrange.Axes.data_x0) then
      (xrange.Axes.data_x0 <- x0'; xupdated := true);
    if xrange.Axes.auto_xend && is_finite x1'
      && (is_inf xrange.Axes.data_xend || x1' > xrange.Axes.data_xend) then
      (xrange.Axes.data_xend <- x1'; xupdated := true);
    if yrange.Axes.auto_x0 && is_finite y0'
      && (is_inf yrange.Axes.data_x0 || y0' < yrange.Axes.data_x0) then
      (yrange.Axes.data_x0 <- y0'; yupdated := true);
    if yrange.Axes.auto_xend && is_finite y1'
      && (is_inf yrange.Axes.data_xend || y1' > yrange.Axes.data_xend) then
      (yrange.Axes.data_xend <- y1'; yupdated := true);
    (* Update x0, xend ranges, unit_size and gx0, gxend and redraw... *)
    update_axes_ranges vp !xupdated !yupdated

  let fit vp r =
    auto_fit vp r.Matrix.x r.Matrix.y (r.Matrix.x +. r.Matrix.w)
      (r.Matrix.y +. r.Matrix.h)

  (* Utility function for (x|y)range *)
  let update_axis vp axis axis_size x0 xend =
    if x0 < xend then begin
      let range = axis.Axes.range.Axes.value in
      range.Axes.auto_x0 <- is_inf x0;
      if not (is_inf x0) then range.Axes.x0 <- x0;
      range.Axes.auto_xend <- is_inf xend;
      if not (is_inf xend) then range.Axes.xend <- xend;
      norm_ratio_auto_axes vp.axes_system;
      let unit_size = axis_size /. (range.Axes.xend -. range.Axes.x0) in
      if unit_size < axis.Axes.unit_size.Axes.value then begin
        axis.Axes.unit_size.Axes.value <- unit_size;
        List.iter update_axes_system axis.Axes.unit_size.Axes.vps
      end else
        update_axes_system vp
    end else
      invalid_arg "Archimedes.Viewport.x/yrange: invalid range."

  let xrange vp = update_axis vp vp.axes_system.Axes.x vp.xaxis_size
  let yrange vp = update_axis vp vp.axes_system.Axes.y vp.yaxis_size

  let set_xlog vp v = vp.axes_system.Axes.x.Axes.log <- v
  let set_ylog vp v = vp.axes_system.Axes.y.Axes.log <- v

  (* FIXME: poor implementations.  The label should be stored and used
     to determine the space for the graphic. *)
  let xlabel_direct vp s =
    show_text_direct vp `Device ~x:0.5 ~y:0.01 Backend.CT s

  let ylabel_direct vp s =
    show_text_direct vp `Device ~x:0.01 ~y:0.5 Backend.RC s
      ~rotate:1.57079632679489656 (* pi / 2 *)

  let title_direct vp s =
    show_text_direct vp `Device ~x:0.5 ~y:0.99 Backend.CB s


(* Synchronization
 ***********************************************************************)

  let remove_from_sync vp sync =
    sync.Axes.vps <- List.filter (fun vp' -> not (vp' == vp)) sync.Axes.vps

  (** [desync_ratio vp] make [vp] single. *)
  let desync_ratio vp =
    let vp_ratio = vp.axes_system.Axes.ratio in
    remove_from_sync vp vp_ratio;
    vp.axes_system.Axes.ratio <- { vp_ratio with Axes.vps = [vp] };
    update_axes_system vp

  let sync_ratio vp vp_base =
    let base_axes = vp_base.axes_system in
    remove_from_sync vp vp.axes_system.Axes.ratio;
    vp.axes_system.Axes.ratio <- base_axes.Axes.ratio;
    base_axes.Axes.ratio.Axes.vps <- vp :: base_axes.Axes.ratio.Axes.vps;
    update_axes_system vp

  let desync_range_axis vp axis =
    let range = axis.Axes.range in
    remove_from_sync vp range;
    axis.Axes.range <- {
      Axes.vps = [vp];
      Axes.value = { range.Axes.value with Axes.x0 = range.Axes.value.Axes.x0 }
    }

  let desync_range ?(x=true) ?(y=true) vp =
    if x then desync_range_axis vp vp.axes_system.Axes.x;
    if y then desync_range_axis vp vp.axes_system.Axes.y;
    if x || y then update_axes_system vp

  let sync_range ?x ?y vp vp_base =
    let x, y = match x, y with
      | None, None -> true, true
      | Some x, None -> x, false
      | None, Some y -> false, y
      | Some x, Some y -> x, y
    in
    let sync_axis_range sync_axis axis axis_base =
      let base_range = axis_base.Axes.range in
      if sync_axis then begin
        let range = axis.Axes.range in
        let x0 = range.Axes.value.Axes.data_x0
        and x1 = range.Axes.value.Axes.data_xend in
        remove_from_sync vp range;
        axis.Axes.range <- base_range;
        base_range.Axes.vps <- vp :: base_range.Axes.vps;
        update_axes_system vp;
        (x0, x1)
      end else
        (base_range.Axes.value.Axes.data_x0,
         base_range.Axes.value.Axes.data_xend)
    in
    let axes = vp.axes_system
    and base_axes = vp_base.axes_system in
    let x0, x1 = sync_axis_range x axes.Axes.x base_axes.Axes.x
    and y0, y1 = sync_axis_range y axes.Axes.y base_axes.Axes.y in
    if x || y then
      auto_fit vp x0 y0 x1 y1

  let sync_axis_unit_size vp axis base_axis =
    let unit_size = axis.Axes.unit_size
    and base_unit_size = base_axis.Axes.unit_size in
    remove_from_sync vp unit_size;
    axis.Axes.unit_size <- base_unit_size;
    base_unit_size.Axes.vps <- vp :: base_unit_size.Axes.vps;
    if unit_size.Axes.value < base_unit_size.Axes.value then begin
      axis.Axes.unit_size.Axes.value <- unit_size.Axes.value;
      axis.Axes.unit_size.Axes.vps
    end else [ vp ]

  let desync_unit_size_axis vp axis =
    let unit_size = axis.Axes.unit_size in
    remove_from_sync vp unit_size;
    axis.Axes.unit_size <- { unit_size with Axes.vps = [vp] }

  let desync_unit_size ?(x=true) ?(y=true) vp =
    if x then desync_unit_size_axis vp vp.axes_system.Axes.x;
    if y then desync_unit_size_axis vp vp.axes_system.Axes.y;
    if x || y then update_axes_system vp

  let sync_unit_size ?(x=true) ?(y=true) vp vp_base =
    let vps1 =
      if x then
        sync_axis_unit_size vp vp.axes_system.Axes.x vp_base.axes_system.Axes.x
      else []
    and vps2 =
      if y then
        sync_axis_unit_size vp vp.axes_system.Axes.y vp_base.axes_system.Axes.y
      else []
    in
    List.iter update_axes_system (merge vps1 vps2)

  let sync ?(x=true) ?(y=true) vp vp_base =
    sync_unit_size ~x ~y vp vp_base;
    sync_range ~x ~y vp vp_base

(* Layouts
 ***********************************************************************)

  (* Uniform grid; redim: identity *)
  let gen_grid vp nx ny ~cols_sync_x ~cols_sync_y ~rows_sync_x ~rows_sync_y
      set get =
    let xstep = 1. /. (float nx) and ystep = 1. /. (float ny) in
    for x = 0 to nx - 1 do
      for y = 0 to ny - 1 do
        let xmin = float x *. xstep
        and ymin = float y *. ystep in
        let xmax = xmin +. xstep
        and ymax = ymin +. ystep in
        set x y (make vp xmin xmax ymin ymax);
        if y > 0 then
          sync ~x:cols_sync_x ~y:cols_sync_y (get x y) (get x 0);
        if x > 0 then
          sync ~x:rows_sync_x ~y:rows_sync_y (get x y) (get 0 y);
      done
    done

  let grid ?(syncs=(false, false, false, false)) vp nx ny =
    let cols_sync_x, cols_sync_y, rows_sync_x, rows_sync_y = syncs in
    let ret = Array.make_matrix nx ny vp in
    let set x y v = ret.(x).(y) <- v
    and get x y = ret.(x).(y) in
    gen_grid vp nx ny ~cols_sync_x ~cols_sync_y ~rows_sync_x ~rows_sync_y
      set get;
    ret

  let rows ?syncs:((rows_sync_x, rows_sync_y)=(false, false)) vp n =
    let ret = Array.make n vp in
    let set _ y v = ret.(y) <- v
    and get _ y = ret.(y) in
    gen_grid vp 1 n ~cols_sync_x:false ~cols_sync_y:false
      ~rows_sync_x ~rows_sync_y set get;
    ret

  let columns ?syncs:((cols_sync_x, cols_sync_y)=(false, false)) vp n =
    let ret = Array.make n vp in
    let set x _ v = ret.(x) <- v
    and get x _ = ret.(x) in
    gen_grid vp n 1 ~cols_sync_x ~cols_sync_y
      ~rows_sync_x:false ~rows_sync_y:false set get;
    ret

  let fixed_left init_prop vp =
    let redim_fixed vp xfactor _ = begin
      let coord = vp.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.;
    end in
    let vp_fixed = make vp 0. init_prop 0. 1. ~redim:redim_fixed in
    let redim vp xfactor _ = begin
      let coord = vp.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.;
      let fixed_right, _ =
        Coordinate.to_parent vp_fixed.coord_device ~x:1. ~y:0. in
      let vp_left, _ = Coordinate.to_parent vp.coord_device ~x:0. ~y:0. in
      Coordinate.translate coord (vp_left -. fixed_right) 0.
    end in
    let vp' = make vp init_prop 1. 0. 1. ~redim in
    (vp_fixed, vp')

  let fixed_right init_prop vp =
    let redim_fixed vp xfactor _ = begin
      let coord = vp.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.;
      Coordinate.translate
        coord (-. (fst (Coordinate.to_parent coord ~x:1. ~y:0.))) 0.
    end in
    let vp_fixed = make vp init_prop 1. 0. 1. ~redim:redim_fixed in
    let redim vp xfactor _ = begin
      let coord = vp.coord_device in
      Coordinate.scale coord (1. /. xfactor) 1.
    end in
    let vp' = make vp 0. init_prop 0. 1. ~redim in
    (vp_fixed, vp')

  let fixed_top init_prop vp =
    let redim_fixed vp _ yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor);
    end in
    let vp_fixed = make vp 0. 1. (1. -. init_prop) 1. ~redim:redim_fixed
    in
    let rec redim vp _ yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor);
      let _, fixed_bottom =
        Coordinate.to_parent vp_fixed.coord_device ~x:0. ~y:1. in
      let _, vp_top = Coordinate.to_parent vp.coord_device ~x:0. ~y:0. in
      Coordinate.translate coord (vp_top -. fixed_bottom) 0.
    end in
    let vp' = make vp 0. 1. 0. (1. -. init_prop) ~redim in
    (vp_fixed, vp')

  let fixed_bottom init_prop vp =
    let redim_fixed vp _ yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor);
      Coordinate.translate
        coord 0. (-. (snd (Coordinate.to_parent coord ~x:0. ~y:1.)))
    end in
    let vp_fixed = make vp 0. 1. 0. init_prop ~redim:redim_fixed in
    let rec redim vp _ yfactor = begin
      let coord = vp.coord_device in
      Coordinate.scale coord 1. (1. /. yfactor)
    end in
    let vp' = make vp 0. 1. init_prop 1. ~redim in
    (vp_fixed, vp')

  (* Border layouts, of desired sizes *)
  let layout_borders ?(north=0.) ?(south=0.) ?(west=0.) ?(east=0.) vp =
    if south +. north >= 1. || east +. west >= 1. then
      invalid_arg "Archimedes.Viewport.layout_borders: \
                   invalid borders dimensions (sum need to be < 1).";
    let east, vp =
      if east > 0. then fixed_right east vp else vp, vp
    in
    let west, vp =
      if west > 0. then fixed_left west vp else vp, vp
    in
    let south, vp =
      if south > 0. then fixed_bottom south vp else vp, vp
    in
    let north, center =
      if north > 0. then fixed_top north vp else vp, vp
    in
    (north, south, west, east, center)


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

  let stroke_preserve ?path ?fit:(do_fit=true) vp coord_name =
    let path = get_path ~notransform:true vp path coord_name in
    if do_fit && coord_name = `Data then fit vp (Path.extents path);
    let path = Path.copy path in
    add_instruction vp (stroke_direct ~path vp coord_name)

  let stroke ?path ?fit vp coord_name =
    stroke_preserve ?path ?fit vp coord_name;
    if path = None then add_instruction vp (fun () -> Path.clear vp.path)

  let fill_preserve ?path ?fit:(do_fit=true) vp coord_name =
    let path = get_path ~notransform:true vp path coord_name in
    if do_fit && coord_name = `Data then fit vp (Path.extents path);
    let path = Path.copy path in
    add_instruction vp (fill_direct ~path vp coord_name)

  let fill ?path ?fit vp coord_name =
    fill_preserve ?path ?fit vp coord_name;
    if path = None then add_instruction vp (fun () -> Path.clear vp.path)

  let clip_rectangle vp ~x ~y ~w ~h =
    add_instruction vp (clip_rectangle_direct vp ~x ~y ~w ~h)

(* Text, marks
 ***********************************************************************)

  let select_font_face vp slant weight family =
    add_instruction vp (select_font_face_direct vp slant weight family)

  let text vp ?(coord=`Data) ?(rotate=0.) x y ?(pos=Backend.CC) text =
    (* auto_fit if Data *)
    if coord = `Data then begin
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
      let x0, y0 = data_from vp `Orthonormal (x, y)
      and xend, yend = data_from vp `Orthonormal (x +. w, y +. h) in
      auto_fit vp x0 y0 xend yend
    end;
    add_instruction vp (show_text_direct vp coord ~rotate ~x ~y pos text)

  let xlabel vp s =
    add_instruction vp (xlabel_direct vp s)

  let ylabel vp s =
    add_instruction vp (ylabel_direct vp s)

  let title vp s =
    add_instruction vp (title_direct vp s)

  let mark vp ~x ~y name =
    auto_fit vp x y x y;
    add_instruction vp (mark_direct vp ~x ~y name)

  let xlog vp = vp.axes_system.Axes.x.Axes.log
  let ylog vp = vp.axes_system.Axes.y.Axes.log
end

include Viewport

(* Local Variables: *)
(* compile-command: "make -k -C .." *)
(* End: *)
