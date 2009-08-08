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
open Archimedes
module Matrix = Backend.Matrix

(* Re-export the labels so we do not have to qualify them with [Backend]. *)
type matrix = Backend.matrix = { mutable xx: float; mutable yx: float;
                                 mutable xy: float; mutable yy: float;
                                 mutable x0: float; mutable y0: float; }

let is_infinite x = 1. /. x = 0.
let min a b = if (a:float) < b then a else b
let max a b = if (a:float) > b then a else b

let round x =
  truncate(if x >= 0. then x +. 0.5 else x -. 0.5)

(** Return the smaller rectangle including the rectangle [r] and the
    segment joining [(x0,y0)] and [(x1,y1)]. *)
let update_rectangle r x0 y0 x1 y1 =
  let x = min r.Backend.x (min x0 x1)
  and y = min r.Backend.y (min y0 y1)
  and x' = max (r.Backend.x +. r.Backend.w) (max x0 x1)
  and y' = max (r.Backend.y +. r.Backend.h) (max y0 y1) in
  { Backend.x = x;  y = y;  w = x' -. x;  h = y' -. y }

(** Returns the range of the function f = t -> (1-t)**3 x0 + 3
    (1-t)**2 t x1 + 3 (1-t) t**2 x2 + t**3 x3, 0 <= t <= 1, under the
    form of an interval [xmin, xmax] *)
let range_bezier x0 x1 x2 x3 =
  let f t =
    let t' = 1. -. t in
    let t2 = t *. t in
    t' *. (t' *. (t' *. x0 +. 3. *. t *. x1) +. 3. *. t2 *. x2)
    +. t2 *. t *. x3 in
  let a = x3 -. 3. *. x2 +. 3. *. x1 -. x0
  and b = 2. *. x2 -. 4. *. x1 +. 2. *. x0
  and c = x1 -. x0 in
  if a = 0. then
    if b = 0. then min x0 x3, max x0 x3 (* deg 1 (=> monotone) *)
    else
      let root = -. c /. b in
      if 0. < root && root < 1. then
        let x = f root in min x (min x0 x3), max x (max x0 x3)
      else min x0 x3, max x0 x3         (* monotone for t in [0,1] *)
  else
    let delta = b *. b -. 4. *. a *. c in
    if delta < 0. then min x0 x3, max x0 x3 (* monotone *)
    else if delta = 0. then
      let root = -. b /. (2. *. a) in
      if 0. < root && root < 1. then
        let x = f root in min x (min x0 x3), max x (max x0 x3)
      else min x0 x3, max x0 x3         (* monotone for t in [0,1] *)
    else (* delta > 0. *)
      let root1 = (if b >= 0. then -. b -. sqrt delta
                   else -. b +. sqrt delta) /. (2. *. a) in
      let root2 = c /. (a *. root1) in
      if 0. < root1 && root1 < 1. then
        let f1 = f root1 in
        if 0. < root2 && root2 < 1. then
          let f2 = f root2 in
          min (min f1 f2) (min x0 x3), max (max f1 f2) (max x0 x3)
        else min f1 (min x0 x3), max f1 (max x0 x3)
      else (* root1 outside [0,1] *)
        if 0. < root2 && root2 < 1. then
          let f2 = f root2 in
          min f2 (min x0 x3), max f2 (max x0 x3)
        else min x0 x3, max x0 x3
;;

(** Return the smaller reactangle containing [r] and the Bézier curve
    given by the control points. *)
let update_curve r x0 y0 x1 y1 x2 y2 x3 y3 =
  let xmin, xmax = range_bezier x0 x1 x2 x3 in
  let xmin = min xmin r.Backend.x in
  let w = max xmax (r.Backend.x +. r.Backend.w) -. xmin in
  let ymin, ymax = range_bezier y0 y1 y2 y3 in
  let ymin = min ymin r.Backend.y in
  let h = max ymax (r.Backend.y +. r.Backend.h) -. ymin in
  { Backend.x = xmin;  y = ymin; w = w; h = h }


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
    | CLOSE_PATH of float * float

  (* Record the state of various graphics values (in a form close to
     what Graphics needs). *)
  type state = {
    mutable color: int;
    mutable line_width: float;
    mutable dash_offset: float;
    mutable dash: float array;
    (* (x,y): current point (when creating a path), in user coordinates. *)
    mutable curr_pt: bool;
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
    try
      let st = Stack.pop t.history in
      t.state <- st;
      (* Re-enable previous settings in case they were changed *)
      Graphics.set_color st.color;
      Graphics.set_line_width (round st.line_width)
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
      curr_pt = false;
      x = 0.;
      y = 0.;
      current_path = [];
      path_extents = { Backend.x=0.; y=0.; w=0.; h=0. };
      (* Identity transformation matrix *)
      ctm = Matrix.make_identity();
    } in
    { closed = false;
      history = Stack.create();
      state = state;
    }

  let close ~options t =
    if not(t.closed) then (
      Graphics.close_graph();
      t.closed <- true;
      in_use := false;
    )

  let clear_path t =
    let st = get_state t in
    st.current_path <- [];
    st.path_extents <- { Backend.x=0.; y=0.; w=0.; h=0. }

  let set_color t c =
    let st = get_state t in
    let r,g,b = Color.rgb c in
    let r = round(Color.red c *. 255.)
    and g = round(Color.green c *. 255.)
    and b = round(Color.blue c *. 255.) in
    let color = Graphics.rgb r g b in
    st.color <- color;
    Graphics.set_color color

  let set_line_width t w =
    let st = get_state t in
    st.line_width <- w;
    Graphics.set_line_width (round w)

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
    let x', y' = Matrix.transform_point st.ctm x y in
    st.current_path <- MOVE_TO(x',y') :: st.current_path;
    (* move only updates the current point but not the path extents *)
    st.curr_pt <- true;
    st.x <- x;
    st.y <- y

  let line_to t ~x ~y =
    let st = get_state t in
    let x', y' = Matrix.transform_point st.ctm x y in
    st.current_path <- LINE_TO(x',y') :: st.current_path;
    (* Update extents and current point *)
    if st.curr_pt then (
      let x0', y0' = Matrix.transform_point st.ctm st.x st.y in
      st.path_extents <- update_rectangle st.path_extents x0' y0' x' y';
    );
    st.curr_pt <- true;
    st.x <- x;
    st.y <- y

  let rel_move_to t ~x ~y =
    let st = get_state t in move_to t (st.x +. x) (st.y +. y)

  let rel_line_to t ~x ~y =
    let st = get_state t in line_to t (st.x +. x) (st.y +. y)

  let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    (* Suffices to transform the control point by the affine
       transformation to have the affine image of the curve *)
    let st = get_state t in
    let x1', y1' = Matrix.transform_point st.ctm x1 y1 in
    let x2', y2' = Matrix.transform_point st.ctm x2 y2 in
    let x3', y3' = Matrix.transform_point st.ctm x3 y3 in
    let x0', y0' =
      if not st.curr_pt then (
        st.current_path <- MOVE_TO(x1', y1') :: st.current_path;
        x1', y1'
      )
      else  Matrix.transform_point st.ctm st.x st.y in
    st.current_path <- CURVE_TO(x1',y1', x2',y2', x3',y3') :: st.current_path;
    (* Update the current point and extents *)
    st.path_extents <-
      update_curve st.path_extents x0' y0' x1' y1' x2' y2' x3' y3';
    st.curr_pt <- true;
    st.x <- x3;
    st.y <- y3


  let rectangle t ~x ~y ~w ~h =
    let st = get_state t in
    let x', y' = Matrix.transform_point st.ctm x y
    and w', h' = Matrix.transform_distance st.ctm w h in

    st.current_path <- RECTANGLE(x, y, w, h) :: st.current_path;

    st.path_extents <-
      update_rectangle st.path_extents x' y' (x' +. w') (y' +. h')

  let arc t ~x ~y ~r ~a1 ~a2 =
    ()

  let rec beginning_of_subpath = function
    | [] -> failwith "No subpath"
    | (MOVE_TO(x,y) | CLOSE_PATH(x,y)) :: _ -> x,y
    | _ :: tl -> beginning_of_subpath tl

  let close_path t =
    let st = get_state t in
    if st.curr_pt then (
      (* Search for the beginning of the current sub-path, if any *)
      let x, y = beginning_of_subpath st.current_path in
      st.current_path <- CLOSE_PATH(x,y) :: st.current_path;
      st.curr_pt <- true;
      st.x <- x;
      st.y <- y
    )


  let path_extents t = (get_state t).path_extents

  let stroke_preserve t =
    let st = get_state t in
    List.iter begin function
    | MOVE_TO(x,y) -> Graphics.moveto (round x) (round y)
    | LINE_TO(x,y) -> Graphics.lineto (round x) (round y)
    | RECTANGLE(x,y,w,h) ->
        Graphics.draw_rect (round x) (round y) (round w) (round h)
    | CURVE_TO(x1,y1, x2,y2, x3,y3) ->
        Graphics.curveto
          (round x1, round y1) (round x2, round y2) (round x3, round y3)
    | CLOSE_PATH(x,y) -> Graphics.lineto (round x) (round y)
    end (List.rev st.current_path)

  let stroke t = stroke_preserve t; clear_path t

  let fill_preserve t =
    ()

  let fill t = fill_preserve t; clear_path t


  let clip_rectangle t ~x ~y ~w ~h =
    ()

  let translate t ~x ~y = Matrix.translate (get_state t).ctm x y

  let scale t ~x ~y = Matrix.scale (get_state t).ctm x y

  let rotate t ~angle = Matrix.rotate (get_state t).ctm ~angle

  let set_matrix t m = (get_state t).ctm <- m

  let get_matrix t = Matrix.copy (get_state t).ctm

  let select_font_face t slant weight family =
    (* FIXME: can we be more clever and try some *)
    Graphics.set_font family

  let set_font_size t size =
    (* FIXME: must be saved in the state ? *)
    Graphics.set_text_size (round size)

  (*FIXME: Rough approximation!*)
  let text_extents t txt =
    {Backend.x = 0.; y = 0.; w = float (String.length txt) *. 10.; h = 10.}

  let show_text t ~rotate ~x ~y pos txt =
    let st = get_state t in
    (* Compute the angle between the desired direction and the X axis
       in the device coord. system. *)
    let dx, dy = Matrix.transform_distance st.ctm (cos rotate) (sin rotate) in
    let angle = atan2 dy dx in
    let x', y' = Matrix.transform_point st.ctm x y in
    if abs_float angle <= 1e-6 then
      let w, h = Graphics.text_size txt in
      
      Graphics.moveto (round x') (round y');
      Graphics.draw_string txt
    else (
      (* Text rotation is not possible with graphics.  Just display
         the text along the desired direction. *)
      (* FIXME: rotations *)
    )
end

let () =
  let module U = Backend.Register(B)  in ()


(* Local Variables: *)
(* compile-command: "make -k archimedes_graphics.cmo" *)
(* End: *)
