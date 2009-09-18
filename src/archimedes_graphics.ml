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
module Matrix = Archimedes.Matrix
module Backend = Archimedes.Backend

(* Re-export the labels so we do not have to qualify them with [Matrix]. *)
type matrix = Matrix.t = { mutable xx: float; mutable yx: float;
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
  let x = min r.Matrix.x (min x0 x1)
  and y = min r.Matrix.y (min y0 y1)
  and x' = max (r.Matrix.x +. r.Matrix.w) (max x0 x1)
  and y' = max (r.Matrix.y +. r.Matrix.h) (max y0 y1) in
  { Matrix.x = x;  y = y;  w = x' -. x;  h = y' -. y }

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

(** Return the smaller rectangle containing [r] and the Bézier curve
    given by the control points. *)
let update_curve r x0 y0 x1 y1 x2 y2 x3 y3 =
  let xmin, xmax = range_bezier x0 x1 x2 x3 in
  let xmin = min xmin r.Matrix.x in
  let w = max xmax (r.Matrix.x +. r.Matrix.w) -. xmin in
  let ymin, ymax = range_bezier y0 y1 y2 y3 in
  let ymin = min ymin r.Matrix.y in
  let h = max ymax (r.Matrix.y +. r.Matrix.h) -. ymin in
  { Matrix.x = xmin;  y = ymin; w = w; h = h }


module B =
struct
  let name = "graphics"

  let backend_to_device _ = Matrix.make_identity()
    (*A Graphics handle has already the "good" coordinates.*)
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
    (* (x,y): current point (when creating a path), in device coordinates. *)
    mutable curr_pt: bool;
    mutable x: float;
    mutable y: float;
    mutable current_path: path_data list; (* Path actions in reverse order *)
    mutable path_extents: Matrix.rectangle;
    (* The extent of the current path, in device coordinates. *)
    mutable ctm : Matrix.t; (* current transformation matrix from the
                             user coordinates to the device ones. *)
    mutable font_size : float;
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
    (* Make a copy of the record so that further actions do not modify
       it. We need to store a *copy* of the ctm, because
       scaling/translation/rotation mustn't modify this stored
       matrix.*)
    let state_copy = { st with ctm = Matrix.copy st.ctm} in
    Stack.push state_copy t.history

  let restore t =
    check_valid_handle t;
    try
      let st = Stack.pop t.history in
      t.state <- st;
      (* Re-enable previous settings in case they were changed *)
      Graphics.set_color st.color;
      Graphics.set_line_width (round st.line_width)
    with Stack.Empty -> () (* nothing to restore *)

  (* On windows, the size given to open_graph is the one of the window
     WITH decorations.  These quantities tell how much need to be
     added to the width and height so that the surface has the desired
     size (set below, when the library is loaded). *)
  let ofsw = ref 0.
  and ofsh = ref 0.

  (* FIXME: options "x=" and "y=" for the position *)
  let make ~options:_ width height =
    if !in_use then failwith "Archimedes_graphics.make: in use";
    printf "Init graphics: %f %f\n%!" !ofsh !ofsh;
    Graphics.open_graph(sprintf " %.0fx%.0f"
                          (width +. !ofsw) (height +. !ofsh));
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
      path_extents = { Matrix.x=0.; y=0.; w=0.; h=0. };
      (* Identity transformation matrix *)
      ctm = Matrix.make_identity();
      font_size = 10.;
    } in
    { closed = false;
      history = Stack.create();
      state = state;
    }

  let close ~options:_ t =
    if not(t.closed) then (
      (* FIXME: Temprary solution, the interactive module must handle this. *)
      printf "Please press a key to continue...%!";
      ignore (Graphics.wait_next_event [Graphics.Key_pressed]);
      Graphics.close_graph();
      t.closed <- true;
      in_use := false;
    )

  let clear_path t =
    let st = get_state t in
    st.current_path <- [];
    st.path_extents <- { Matrix.x=0.; y=0.; w=0.; h=0. };
    st.curr_pt <- false

  let set_color t c =
    let st = get_state t in
    let r = round(Archimedes.Color.r c *. 255.)
    and g = round(Archimedes.Color.g c *. 255.)
    and b = round(Archimedes.Color.b c *. 255.) in
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
  let device_move_to t x y =
    let st = get_state t in
    st.current_path <- MOVE_TO(x,y) :: st.current_path;
    (* move only updates the current point but not the path extents *)
    st.curr_pt <- true;
    st.x <- x;
    st.y <- y

  let device_line_to t x y =
    let st = get_state t in
    (*Note: if there's no current point then line_to behaves as move_to.*)
    if st.curr_pt then (
      st.current_path <- LINE_TO(x,y) :: st.current_path;
      (* Update extents*)
      st.path_extents <- update_rectangle st.path_extents st.x st.y x y;
    )
    else (
      st.curr_pt <- true;
      st.current_path <- MOVE_TO(x,y) :: st.current_path
    );
    (* Update current point *)
    st.x <- x;
    st.y <- y

  let move_to t ~x ~y =
    let st = get_state t in
    let x', y' = Matrix.transform_point st.ctm x y in
    device_move_to t x' y'

  let line_to t ~x ~y =
    let st = get_state t in
    let x', y' = Matrix.transform_point st.ctm x y in
    device_line_to t x' y'

  let rel_move_to t ~x ~y =
    let st = get_state t in
    let x,y = Matrix.transform_distance st.ctm x y in
    device_move_to t (st.x +. x) (st.y +. y)

  let rel_line_to t ~x ~y =
    let st = get_state t in
    let x',y' = Matrix.transform_distance st.ctm x y in
    device_line_to t (st.x +. x') (st.y +. y')

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
        st.curr_pt <- true;
        x1', y1'
      )
      else  Matrix.transform_point st.ctm st.x st.y in
    st.current_path <- CURVE_TO(x1',y1', x2',y2', x3',y3') :: st.current_path;
    (* Update the current point and extents *)
    st.path_extents <-
      update_curve st.path_extents x0' y0' x1' y1' x2' y2' x3' y3';
    st.x <- x3';
    st.y <- y3'


  let rectangle t ~x ~y ~w ~h =
    let st = get_state t in
    let x', y' = Matrix.transform_point st.ctm x y
    and w', h' = Matrix.transform_distance st.ctm w h in
    (*FIXME: this is not sufficient to make a rectangle ("rectangle on
      their corner"...)*)
    st.current_path <- RECTANGLE(x', y', w', h') :: st.current_path;
    (* Update the current point and extents *)
    st.path_extents <-
      update_rectangle st.path_extents x' y' (x' +. w') (y' +. h');
    st.curr_pt <- true;
    st.x <- x';
    st.y <- y'

  let arc t ~r ~a1 ~a2 =
    let st = get_state t in
    let rxx, rxy = Matrix.transform_distance st.ctm r 0.
    and ryx, ryy = Matrix.transform_distance st.ctm 0. r in
    (*FIXME: better radius*)
    let r = (sqrt(rxx *. rxx +. rxy *. rxy) +. sqrt(ryx *. ryx +. ryy *. ryy)) /.2. in
    let b1 = atan2 rxy rxx in
    let rec arcin a1 a2 =
      let diff_angle = abs_float (a2 -. a1) in
      let curvelen = r *. diff_angle in
      if diff_angle < (atan 1.)  && curvelen < 100. then (
        let rcos1 = r *. cos a1 and rsin1 = r *. sin a1 in
        let rcos2 = r *. cos a2 and rsin2 = r *. sin a2 in
        let coeff = (2. *. (sqrt 5.) -. 4.) /. 3. in
        (*let coeff = 1. in*)
        (*This coefficient makes the middle point of a Bezier curve
          coïncide with the arc.*)
        let f z a = z +.coeff *. a in
        if st.curr_pt then
          let x = st.x -. rcos1 and y = st.y -. rsin1 in
          let x' = x +. rcos2 and y' = y +. rsin2 in
          let cx = f st.x ( -.rsin1) and cy = f st.y rcos1 in
          let cx' = f x' rsin2 and cy' = f y' ( -. rcos2) in
          st.current_path <- CURVE_TO(cx,cy, cx',cy', x',y') :: st.current_path;
          (* Update the current point and extents *)
          st.path_extents <-
            update_curve st.path_extents st.x st.y cx cy cx' cy' x' y';
          st.curr_pt <- true;
          st.x <- x';
          st.y <- y'
        else failwith "Archimedes_graphics.arc: no current point"
      )
      else (
        let a3 = (a1 +. a2) /.2. in
        arcin a1 a3;
        arcin a3 a2)
    in
    arcin (b1 +. a1) (b1 +. a2)

  let rec beginning_of_subpath list =
    match list with
    | [] -> failwith "Archimedes_graphics: No subpath"
    | MOVE_TO(x,y):: _ -> x,y
    | CLOSE_PATH(x,y):: _ -> x,y
    | RECTANGLE(x,y,_,_):: _ -> x,y
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
    let st = get_state t in
    let add_to_first_list x = function
        [] -> failwith ""
      | list::tail -> (List.rev_append x list)::tail
    in
    let rec make_points_for_curve ?(reverse=true) x0 y0 x1 y1 x2 y2 x3 y3 list =
      let diffx01 = x1 - x0
      and diffy01 = y1 - y0
      and diffx12 = x2 - x1
      and diffy12 = y2 - y1
      and diffx23 = x3 - x2
      and diffy23 = y3 - y2 in
      let eqslope y x y' x' = y * x' = x * y' in
      if eqslope diffy12 diffx12 diffy01 diffx01
        && eqslope diffy12 diffx12 diffy23 diffx23 then
          (*All slopes are equal : finish by adding the last point*)
          [x3,y3]
      else if true then [(x3, y3);(x2,y2);(x1,y1)]
      else
        (*FIXME : this causes a Stack overflow.*)
        (*Find the middle point of the curve.*)
        let x01 = float(x0 + x1) /.2.
        and x12 = float(x1 + x2) /.2.
        and x23 = float(x2 + x3) /.2.
        and y01 = float(y0 + y1) /.2.
        and y12 = float(y1 + y2) /.2.
        and y23 = float(y2 + y3) /.2. in
        let x0' = (x01 +. x12) /. 2.
        and x1' = (x12 +. x23) /. 2.
        and y0' = (y01 +. y12) /. 2.
        and y1' = (y12 +. y23) /. 2. in
        let x5 = truncate ((x0' +. x1') /.2.)
        and y5 = truncate ((y0' +. y1') /.2.) in
        (* Then find the point for each part of the curve. *)
        let part1 =
          make_points_for_curve ~reverse
            x5 y5 (truncate x1') (truncate y1')
            (truncate x23) (truncate y23) x3 y3 list in
        let reverse = not reverse in
        let part2 =
          make_points_for_curve ~reverse
            x0 y0 (truncate x01) (truncate y01)
            (truncate x0') (truncate y0') x5 y5 list in
        List.rev_append part2 part1
          (*if x0 = x1 && x1 = x2 && x2 = x3 then
          (*end with a line y0--y3*)
            else if y0 = y1 && y1 = y2 && y2 = y3 then
          (*x0--x3*)
            else ()*)
    in
    let add_points list = function
      | MOVE_TO(x,y) -> [((round x), (round y))]::list
      | LINE_TO(x,y) -> add_to_first_list [((round x), (round y))] list
      | RECTANGLE(x,y,w,h) ->
          let x' = round (x+.w) and y' = round (y+.h) in
          let x = round x and y = round y in
          let list = [(x,y)]::list in
          add_to_first_list [(x,y'); (x',y'); (x',y)] list
      | CURVE_TO(x1,y1, x2,y2, x3,y3) ->
          let x0,y0 =
            match list with
              (current::points)::others -> current
            | _ -> (round x1), (round y1)
          in
          let points_curve = make_points_for_curve x0 y0
            (round x1) (round y1) (round x2) (round y2) (round x3) (round y3) []
          in
          add_to_first_list points_curve list
      | CLOSE_PATH(x,y) -> add_to_first_list [((round x), (round y))] list
    in
    let subpaths = List.fold_left add_points [] (List.rev st.current_path) in
    let subpath_arrays = List.map Array.of_list subpaths in
    let rec fill = function
        [] -> ()
      | path::list -> 
          if Array.length path < 3 then
            (if Array.length path = 2 then
               (let x,y = path.(0)
                and x',y' = path.(1) in
                Graphics.moveto x y;
                Graphics.lineto x' y'))
          else Graphics.fill_poly path;
          fill list
    in fill subpath_arrays


  let fill t = fill_preserve t; clear_path t


  let clip_rectangle t ~x ~y ~w ~h =
    ()

  let translate t ~x ~y = Matrix.translate (get_state t).ctm x y

  let scale t ~x ~y = Matrix.scale (get_state t).ctm x y

  let rotate t ~angle = Matrix.rotate (get_state t).ctm ~angle

  let set_matrix t m =
    (*Replaces the ctm with a *copy* of m so that modifying m does not
      change the (newly set) coordinate system.*)
    (get_state t).ctm <- Matrix.copy m

  let get_matrix t = Matrix.copy (get_state t).ctm

  let select_font_face t slant weight family =
    (* FIXME: can we be more clever and try some *)
    Graphics.set_font family

  let set_font_size t size =
    (get_state t).font_size <- size;
    Graphics.set_text_size (round size)

  let text_extents t txt =
    check_valid_handle t;
    let w, h = Graphics.text_size txt in
    { Matrix.x = 0.; y = 0.; w = float w ; h = float h }

  let show_text t ~rotate ~x ~y pos txt =
    let st = get_state t in
    (* Compute the angle between the desired direction and the X axis
       in the device coord. system. *)
    let dx, dy = Matrix.transform_distance st.ctm (cos rotate) (sin rotate) in
    let angle = atan2 dy dx in
    let x', y' = Matrix.transform_point st.ctm x y in
    if abs_float angle <= 1e-6 then
      let w', h' = Graphics.text_size txt in
      (* text_size returns size already in device coords.*)
      (*let wx, wy = Matrix.transform_distance st.ctm (float w) 0.
      and hx, hy = Matrix.transform_distance st.ctm 0. (float h) in*)
      let wx = float w' and wy = 0. in
      let hx = 0. and hy = float h' in
      let x'' =  match pos with
        | Backend.CC | Backend.CT | Backend.CB ->
            x' -. (wx +. hx) *. 0.5
        | Backend.RC | Backend.RT | Backend.RB ->
            x'
        | Backend.LC | Backend.LT | Backend.LB ->
            x' -. wx -. hx
      and y'' = match pos with
        | Backend.CC | Backend.RC | Backend.LC ->
            y' -. (hy +. wy) *. 0.5
        | Backend.CT | Backend.RT | Backend.LT ->
            y'
        | Backend.CB | Backend.RB | Backend.LB ->
            y' -. hy -. wy
      in
      Graphics.moveto (round x'') (round y'');
      Graphics.draw_string txt
    else (
      (* Text rotation is not possible with graphics.  Just display
         the text along the desired direction. *)
      (* FIXME: rotations *)
    )
end

let () =
  let module U = Backend.Register(B)  in
  if Sys.os_type = "Win32" then (
    (* Set offsets so the actual surface is of the requested size. *)
    Graphics.open_graph " 100x100";
    let w = Graphics.size_x ()
    and h = Graphics.size_y() in
    Graphics.close_graph ();
    ofsw := float (100 - w);
    ofsh := float (100 - h);
  )


(* Local Variables: *)
(* compile-command: "make -k archimedes_graphics.cmo archimedes_graphics.cmxs" *)
(* End: *)
