(* File: archimedes_graphics.ml

   Copyright (C) 2009

     Bertrand Desmons <Bertrand.Desmons@umons.ac.be>
     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(** Graphics Archimedes plugin *)

open Printf

exception Invalid_matrix

let is_infinite x = 1. /. x = 0.

type matrix = Backend.matrix = { mutable xx: float; mutable yx: float;
                                 mutable xy: float; mutable yy: float;
                                 mutable x0: float; mutable y0: float; }

let transform_distance m ~dx ~dy =
  (m.xx *. dx +. m.xy *. dy,  m.yx *. dx +. m.yy *. dy)

let transform_point m ~x ~y =
  (m.xx *. x +. m.xy *. y +. m.x0,  m.yx *. x +. m.yy *. y +. m.y0)


module B =
struct
  let name = "graphics"

  let in_use = ref false (* only one Graphics handle can be created *)

  (* Device coordinates and dimensions. *)
  type path_data =
    | MOVE_TO of float * float
    | LINE_TO of float * float
    | RECTANGLE of float * float * float * float
    | CURVE_TO of float * float * float * float * float * float
    | CLOSE_PATH

  (* Record the state of various graphics values (in a form close to
     what Graphics needs). *)
  type state = {
    mutable color: int;
    mutable line_width: float;
    mutable dash_offset: float;
    mutable dash: float array;
    (* (x,y): current position (when creating a path), in user coordinates. *)
    mutable x: float;
    mutable y: float;
    mutable current_path: path_data list; (* Path actions in reverse order *)
    mutable path_extents: Backend.rectangle;
    (* The extent of the current path, in device coordinates. *)
    mutable ctm : matrix; (* current transformation matrix from the
                             user coordinates to the device ones. *)
  }

  type t = {
    mutable closed: bool;
    history: state Stack.t; (* saved states *)
    mutable state: state;   (* current state *)
  }

  let check_valid_handle t =
    if t.closed then failwith "Archimedes_graphics: handle closed"

  let get_state t =
    check_valid_handle t;
    t.state

  let save t =
    let st = get_state t in
    (* Make a copy of the record so that further actions do not modify it *)
    let state_copy = { st with color = st.color } in
    Stack.push state_copy t.history

  let restore t =
    check_valid_handle t;
    try t.state <- Stack.pop t.history
    with Stack.Empty -> ()

  (* FIXME: options "x=" and "y=" for the position *)
  let make ~options width height =
    if !in_use then failwith "Archimedes_graphics.make: in use";
    Graphics.open_graph(sprintf " %.0fx%.0f" width height);
    Graphics.set_window_title "Archimedes";
    in_use := true;
    let state = {
      color = 0x0; (* black *)
      line_width = 1.;
      dash_offset = 0.;
      dash = [| |]; (* no dash *)
      x = 0.;
      y = 0.;
      current_path = [];
      path_extents = { Backend.x=0.; y=0.; w=0.; h=0. };
      (* Identity transformation matrix *)
      ctm = { xx = 1.; xy = 0.; yx = 0.; yy = 1.;  x0 = 0.; y0 = 0. };
    } in
    { closed = false;
      history = Stack.create();
      state = state;
    }

  let close ~options t =
    if not(t.closed) then (
      Graphics.close_graph();
      t.closed <- true;
    )

  let clear_path t =
    let st = get_state t in
    st.current_path <- [];
    st.path_extents <- { Backend.x=0.; y=0.; w=0.; h=0. }

  let set_color t c =
    let st = get_state t in
    let r = truncate(Color.red c *. 255.)
    and g = truncate(Color.green c *. 255.)
    and b = truncate(Color.blue c *. 255.) in
    let color = Graphics.rgb r g b in
    st.color <- color;
    Graphics.set_color color

  let set_line_width t w =
    let st = get_state t in
    st.line_width <- w;
    Graphics.set_line_width (truncate w)

  let get_line_width t = (get_state t).line_width

  (* No Graphics action, see [stroke]. *)
  let set_dash t ofs arr =
    let st = get_state t in
    st.dash_offset <- ofs;
    st.dash <- arr

  let get_dash t =
    let st = get_state t in (st.dash, st.dash_offset)

  (* Not supported, do nothing *)
  let set_line_cap t _ = check_valid_handle t
  let get_line_cap t = check_valid_handle t; Backend.BUTT
  let set_line_join t _ = check_valid_handle t
  let get_line_join t = check_valid_handle t; Backend.JOIN_MITER
  let set_miter_limit t _ = check_valid_handle t

  (* Paths are not acted upon directly but wait for [stroke] or [fill]. *)
  let move_to t ~x ~y =
    let st = get_state t in
    st.x <- x;
    st.y <- y;
    let x', y' = transform_point st.ctm x y in
    st.current_path <- MOVE_TO(x',y') :: st.current_path
      
      (* FIXME: Update extents *)

  let line_to t ~x ~y =
    let st = get_state t in
    st.x <- x;
    st.y <- y;
    let x', y' = transform_point st.ctm x y in
    st.current_path <- LINE_TO(x',y') :: st.current_path
      (* FIXME: Update extents *)

  let rel_move_to t ~x ~y =
    let st = get_state t in move_to t (st.x +. x) (st.y +. y)

  let rel_line_to t ~x ~y =
    let st = get_state t in line_to t (st.x +. x) (st.y +. y)

  let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    (* Suffices to transform the control point by the affine
       transformation to have the affine image of the curve *)
    let st = get_state t in
    let x1', y1' = transform_point st.ctm x1 y1 in
    let x2', y2' = transform_point st.ctm x2 y2 in
    let x3', y3' = transform_point st.ctm x3 y3 in
    st.current_path <- CURVE_TO(x1',y1', x2',y2', x3',y3') :: st.current_path


  let rectangle t ~x ~y ~w ~h =
    let st = get_state t in
    st.current_path <- CLOSE_PATH :: RECTANGLE(x, y, w, h) :: st.current_path

  let arc t ~x ~y ~r ~a1 ~a2 =
    ()

  let close_path t =
    ()

  let path_extents t = (get_state t).path_extents

  let stroke_preserve t =
    ()

  let stroke t = stroke_preserve t; clear_path t

  let fill_preserve t =
    ()

  let fill t = fill_preserve t; clear_path t


  let clip_rectangle t ~x ~y ~w ~h =
    ()

  let translate t ~x ~y =
    let m = (get_state t).ctm in
    m.x0 <- m.x0 +. m.xx *. x +. m.xy *. y;
    m.y0 <- m.y0 +. m.yx *. x +. m.yy *. y

  let scale t ~x ~y =
    if x *. y = 0. (* det = 0. because x or y is 0. or underflow *)
      || is_infinite x || is_infinite y then raise Invalid_matrix;
    let m = (get_state t).ctm in
    m.xx <- m.xx *. x;
    m.yx <- m.yx *. x;
    m.xy <- m.xy *. y;
    m.yy <- m.yy *. y

  let rotate t ~angle =
    let m = (get_state t).ctm in
    if angle <> 0. then (
      if is_infinite angle then raise Invalid_matrix;
      let cosa = cos angle and sina = sin angle in
      let xx = m.xx in
      m.xx <- xx *. cosa +. m.xy *. sina;
      m.xy <- m.xy *. cosa -. xx *. sina;
      let yx = m.yx in
      m.yx <- yx *. cosa +. m.yy *. sina;
      m.yy <- m.yy *. cosa -. yx *. sina
    )

  let set_matrix t m = (get_state t).ctm <- m

  let get_matrix t =
    (* Make a copy of the matrix *)
    let st = get_state t in { st.ctm with xx = st.ctm.xx }

  let select_font_face t slant weight family = ()

  let set_font_size t size = ()

  let show_text t ~rotate ~x ~y pos txt =
    ()
end

let () =
  let module U = Backend.Register(B)  in ()


(* Local Variables: *)
(* compile-command: "make -k archimedes_graphics.cmo" *)
(* End: *)
