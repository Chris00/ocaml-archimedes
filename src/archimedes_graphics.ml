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

(* Re-export the labels so we do not have to qualify them with [Matrix]. *)
type matrix = Matrix.t = { mutable xx: float; mutable yx: float;
                           mutable xy: float; mutable yy: float;
                           mutable x0: float; mutable y0: float; }

let is_infinite x = 1. /. x = 0.
let min a b = if (a:float) < b then a else b
let max a b = if (a:float) > b then a else b

let round x = truncate(if x >= 0. then x +. 0.5 else x -. 0.5)

let fourth_pi = atan 1.
let pi = 4. *. fourth_pi
let two_pi = 2. *. pi

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
        (* RECTANGLE(x, y, width, height) *)
    | CURVE_TO of float * float * float * float * float * float * float * float
    | CLOSE_PATH of float * float

  (* Record the state of various graphics values (in a form close to
     what Graphics needs). *)
  type state = {
    mutable color: int;
    mutable line_width : float;
    (* current line width.  When stroking, this is used to set the
       graphics line width, according to the CTM. *)
    mutable dash_offset: float;
    mutable dash: float array;
    (* The extent of the current path, in device coordinates. *)
    mutable ctm : Matrix.t; (* current transformation matrix from the
                               user coordinates to the device ones. *)
    mutable font_slant: Backend.slant;
    mutable font_weight: Backend.weight;
    mutable font_family: string;
    mutable font_size : float;
  }

  type t = {
    mutable closed: bool;
    hold: bool; (* on close, hold the windows until a key is pressed *)
    history: state Stack.t; (* saved states *)
    mutable state: state;   (* current state *)
    (* save/restore do not affect the current path. *)
    mutable current_path: path_data list; (* Path actions in reverse order *)
    mutable path_extents: Matrix.rectangle;
    (* (x,y): current point (when creating a path), in device coordinates. *)
    mutable curr_pt: bool;
    mutable x: float;
    mutable y: float;
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
      Graphics.set_color st.color
    with Stack.Empty ->
      invalid_arg "Archimedes_graphics.restore: no save issued."

  (* On windows, the size given to open_graph is the one of the window
     WITH decorations.  These quantities tell how much need to be
     added to the width and height so that the surface has the desired
     size (set below, when the library is loaded). *)
  let ofsw = ref 0.
  and ofsh = ref 0.

  (* FIXME: options "x=" and "y=" for the position *)
  let make ~options width height =
    if !in_use then failwith "Archimedes_graphics.make: in use";
    (* Parse options *)
    let hold = ref false in
    List.iter (fun o ->
                 if o = "hold" then hold := true;
              ) options;
    Graphics.open_graph(sprintf " %.0fx%.0f-10+10"
                          (width +. !ofsw) (height +. !ofsh));
    Graphics.set_window_title "Archimedes";
    Graphics.auto_synchronize false;
    in_use := true;
    let state = {
      color = 0x0; (* black *)
      line_width = 1.;
      dash_offset = 0.;
      dash = [| |]; (* no dash *)
      (* Identity transformation matrix *)
      ctm = Matrix.make_identity();
      font_slant = Backend.Upright;
      font_weight = Backend.Normal;
      font_family = "*";
      font_size = 10.;
    } in
    { closed = false;
      hold = !hold;
      history = Stack.create();
      state = state;
      current_path = [];
      path_extents = { Matrix.x=0.; y=0.; w=0.; h=0. };
      curr_pt = false;
      x = 0.;
      y = 0.;
    }

  let close ~options:_ t =
    if not(t.closed) then (
      (* FIXME: Temprary solution, the interactive module must handle this. *)
      if t.hold then (
        Graphics.set_window_title "Archimedes [Press a key to close]";
        ignore(Graphics.wait_next_event [Graphics.Key_pressed]);
      );
      Graphics.close_graph();
      t.closed <- true;
      in_use := false;
    )

  let clear_path t =
    check_valid_handle t;
    t.current_path <- [];
    t.path_extents <- { Matrix.x=0.; y=0.; w=0.; h=0. };
    t.curr_pt <- false

  let set_color t c =
    let st = get_state t in
    let r = round(Color.r c *. 255.)
    and g = round(Color.g c *. 255.)
    and b = round(Color.b c *. 255.) in
    let color = Graphics.rgb r g b in
    st.color <- color;
    Graphics.set_color color

  let set_line_width t w =
    if w < 0. then invalid_arg "set_line_width";
    (get_state t).line_width <- w

  let get_line_width t = (get_state t).line_width

  let graphics_set_line_width st =
    (* FIXME: the linewidth is independent of the CTM.  Do we want to
       keep it that way ?  Can we otherwise handle it properly for Graphics? *)
    (* let w = st.line_width *. min st.ctm.xx st.ctm.yy in
       Graphics.set_line_width (round w) *)
    Graphics.set_line_width (round st.line_width)

  (* No Graphics action, see [stroke]. *)
  let set_dash t ofs arr =
    let st = get_state t in
    st.dash_offset <- ofs;
    st.dash <- arr

  let get_dash t =
    let st = get_state t in (st.dash, st.dash_offset)

  (* Not supported, do nothing *)
  let set_line_cap t _ = check_valid_handle t
  let get_line_cap t = check_valid_handle t; Backend.ROUND
  let set_line_join t _ = check_valid_handle t
  let get_line_join t = check_valid_handle t; Backend.JOIN_MITER
  let set_miter_limit t _ = check_valid_handle t

  (* Paths are not acted upon directly but wait for [stroke] or [fill]. *)
  let device_move_to t x y =
    t.current_path <- MOVE_TO(x,y) :: t.current_path;
    (* move only updates the current point but not the path extents *)
    t.curr_pt <- true;
    t.x <- x;
    t.y <- y

  let device_line_to t x y =
    (*Note: if there's no current point then line_to behaves as move_to.*)
    if t.curr_pt then (
      t.current_path <- LINE_TO(x,y) :: t.current_path;
      (* Update extents*)
      t.path_extents <- update_rectangle t.path_extents t.x t.y x y;
      (* Update current point *)
      t.x <- x;
      t.y <- y
    )
    else device_move_to t x y

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
    let x, y = Matrix.transform_distance st.ctm x y in
    device_move_to t (t.x +. x) (t.y +. y)

  let rel_line_to t ~x ~y =
    let st = get_state t in
    let x',y' = Matrix.transform_distance st.ctm x y in
    device_line_to t (t.x +. x') (t.y +. y')

  let rectangle t ~x ~y ~w ~h =
    let st = get_state t in
    let x', y' = Matrix.transform_point st.ctm x y
    and w', h' = Matrix.transform_distance st.ctm w h in
    (*FIXME: this is not sufficient to make a rectangle ("rectangle on
      their corner"...)*)
    t.current_path <- RECTANGLE(x', y', w', h') :: t.current_path;
    (* Update the current point and extents *)
    t.path_extents <-
      update_rectangle t.path_extents x' y' (x' +. w') (y' +. h');
    t.curr_pt <- true;
    t.x <- x';
    t.y <- y'

  let internal_curve_to t st ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    (* Suffices to transform the control point by the affine
       transformation to have the affine image of the curve *)
    let x1', y1' = Matrix.transform_point st.ctm x1 y1 in
    let x2', y2' = Matrix.transform_point st.ctm x2 y2 in
    let x3', y3' = Matrix.transform_point st.ctm x3 y3 in
    let x0', y0' =
      if t.curr_pt then t.x, t.y
      else (
        t.current_path <- MOVE_TO(x1', y1') :: t.current_path;
        t.curr_pt <- true;
        x1', y1'
      ) in
    t.current_path <-
      CURVE_TO(x0', y0', x1',y1', x2',y2', x3',y3') :: t.current_path;
    (* Update the current point and extents *)
    t.path_extents <-
      update_curve t.path_extents x0' y0' x1' y1' x2' y2' x3' y3';
    t.x <- x3';
    t.y <- y3'

  let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    internal_curve_to t (get_state t) ~x1 ~y1 ~x2 ~y2 ~x3 ~y3

  (* Constant to determine the control points so that the bezier curve
     passes by middle point of the arc. *)
  let arc_control = 4. /. 3. (* (1 - cos(b))/(sin b),  b = (a1 - a2)/2 *)

  let rec bezier_arc t st x0 y0 r a1 a2 =
    let da = 0.5 *. (a2 -. a1) in
    if abs_float(da) <= fourth_pi then
      let k = arc_control *. (1. -. cos da) /. sin da in
      let rcos_a1 = r *. cos a1 and rsin_a1 = r *. sin a1 in
      let rcos_a2 = r *. cos a2 and rsin_a2 = r *. sin a2 in
      let x3 = x0 -. rcos_a1 +. rcos_a2
      and y3 = y0 -. rsin_a1 +. rsin_a2 in
      let x1 = x0 -. k *. rsin_a1
      and y1 = y0 +. k *. rcos_a1 in
      let x2 = x3 +. k *. rsin_a2
      and y2 = y3 -. k *. rcos_a2 in
      internal_curve_to t st x1 y1 x2 y2 x3 y3;
      x3, y3
    else (* several Bezier curves are needed. *)
      let mid = 0.5 *. (a1 +. a2) in
      let x0, y0 = bezier_arc t st x0 y0 r a1 mid in
      bezier_arc t st x0 y0 r mid a2

  let arc t ~r ~a1 ~a2 =
    (* Approximate the arc by Bezier curves to allow for arbitrary
       affine transformations. *)
    let st = get_state t in
    if not t.curr_pt then failwith "archimedes_graphics.arc: no current point";
    let x0, y0 = Matrix.inv_transform_point st.ctm t.x t.y in
    ignore(bezier_arc t st x0 y0 r a1 a2)

  let rec beginning_of_subpath = function
    | [] -> failwith "Archimedes_graphics: No subpath"
    | MOVE_TO(x,y) :: _ -> x,y
    | CLOSE_PATH(x,y) :: _ -> x,y
    | RECTANGLE(x,y,_,_) :: _ -> x,y
    | (LINE_TO _ | CURVE_TO _) :: tl -> beginning_of_subpath tl

  let close_path t =
    check_valid_handle t;
    if t.curr_pt then (
      (* Search for the beginning of the current sub-path, if any *)
      let x, y = beginning_of_subpath t.current_path in
      t.current_path <- CLOSE_PATH(x,y) :: t.current_path;
      t.x <- x;
      t.y <- y
    )


  let path_extents t = t.path_extents

  let stroke_preserve t =
    let st = get_state t in
    graphics_set_line_width st;
    List.iter begin function
    | MOVE_TO(x,y) -> Graphics.moveto (round x) (round y)
    | LINE_TO(x,y) -> Graphics.lineto (round x) (round y)
    | RECTANGLE(x,y,w,h) ->
        let x, w =
          if w >= 0. then x, w
          else x +. w, -. w
        in
        let y, h =
          if h >= 0. then y, h
          else y +. h, -. h
        in
        let x = round x and y = round y
        and w = round (x +. w) - round x and h = round (y +. h) - round y in
        Graphics.draw_rect x y w h
    | CURVE_TO(_, _, x1,y1, x2,y2, x3,y3) ->
        Graphics.curveto
          (round x1, round y1) (round x2, round y2) (round x3, round y3)
    | CLOSE_PATH(x,y) -> Graphics.lineto (round x) (round y)
    end (List.rev t.current_path);
    Graphics.synchronize()

  let stroke t = stroke_preserve t; clear_path t

  (* We will use the fill_poly primitive of graphics for all fillings.
     Thus one must transform each sub-path into an array of coordinates. *)
  let curve_nsamples = 20 (* curve_nsamples + 1 points *)
  let curve_dt = 1. /. float curve_nsamples

  (* Add some points on the Bezier curve to [coords] in *reverse*
     order, except the 1st point which is sopposed to be added by the
     previous component of the path. *)
  let add_curve_sampling x0 y0  x1 y1  x2 y2  x3 y3 coords =
    let coords = ref coords in
    for i = curve_nsamples downto 1 do
      let t = float i *. curve_dt in
      let tm = 1. -. t in
      let t2 = t *. t   and tm2 = tm *. tm in
      let t3 = t2 *. t  and tm3 = tm2 *. tm in
      let t' = 3. *. t2 *. tm  and t'' = 3. *. t *. tm2 in
      let x = tm3 *. x0 +. t'' *. x1 +. t' *. x2 +. t3 *. x3
      and y = tm3 *. y0 +. t'' *. y1 +. t' *. y2 +. t3 *. y3 in
      coords := (round x, round y) :: !coords;
    done;
    !coords

  (* [path] gives the path elements in *reverse* order. *)
  let rec gather_subpath path coords =
    match path with
    | [] -> fill_subpath coords (* reached the beginning of the path *)
    | MOVE_TO(x,y) :: tl ->
        fill_subpath ((round x, round y) :: coords);
        gather_subpath tl []
    | LINE_TO(x,y) :: tl ->
        gather_subpath tl ((round x, round y) :: coords)
    | RECTANGLE(x,y,w,h) :: tl ->
        let x, w =
          if w >= 0. then x, w
          else x +. w, -. w
        in
        let y, h =
          if h >= 0. then y, h
          else y +. h, -. h
        in

        (*let x = round x and y = round y in
        let w = round w and h = round h in*)
        let x = round x and y = round y
        and w = round (x +. w) - round x and h = round (y +. h) - round y in
        (* If there was no "MOVE_TO" after the rectangle, the base
           point of the rectangle is used. *)
        if coords = [] then
          (* This rectangle is not continued by another path.  We can
             optimize using the [fill_rect] Graphics' primitive. *)
          Graphics.fill_rect x y w h
        else (
          let x1 = x + w and y1 = y + h in
          let c = (x,y) :: (x1,y) :: (x1,y1) :: (x,y1) :: (x,y) :: coords in
          fill_subpath c;
        );
        gather_subpath tl []
    | CLOSE_PATH(x,y) :: tl ->
        fill_subpath ((round x, round y) :: coords);
        gather_subpath tl []
    | CURVE_TO(x0,y0, x1,y1, x2,y2, x3,y3) :: tl ->
        let coords = add_curve_sampling x0 y0 x1 y1 x2 y2 x3 y3 coords in
        gather_subpath tl coords
  and fill_subpath = function
    | [] | [ _ ] -> ()
    | [ (x1,y1); (x0,y0) ] ->
          Graphics.moveto x0 y0;  Graphics.lineto x1 y1
    | coords -> Graphics.fill_poly (Array.of_list coords)

  let fill_preserve t =
    (* Line width does not matter for "fill". *)
    gather_subpath t.current_path [];
    Graphics.synchronize()

  let fill t = fill_preserve t; clear_path t


  let clip_rectangle _t ~x:_ ~y:_ ~w:_ ~h:_ =
    (* FIXME: we could try a sophisticated procedure of plotting on a
       copy and extrating the rectangle of interest. *)
    ()

  let translate t ~x ~y = Matrix.translate (get_state t).ctm x y

  let scale t ~x ~y = Matrix.scale (get_state t).ctm x y

  let rotate t ~angle = Matrix.rotate (get_state t).ctm ~angle

  let set_matrix t m =
    (*Replaces the ctm with a *copy* of m so that modifying m does not
      change the (newly set) coordinate system.*)
    (get_state t).ctm <- Matrix.copy m

  let get_matrix t = Matrix.copy (get_state t).ctm

  (* FIXME: What about win32 and mac ? *)
  let string_of_font st =
    let slant = match st.font_slant with
      | Backend.Upright -> 'r'
      | Backend.Italic -> 'i' in
    let weight = match st.font_weight with
      | Backend.Normal -> "medium"
      | Backend.Bold -> "bold" in
    sprintf "-*-%s-%s-%c-normal--%i-*-*-*-*-*-iso10646-*"
      st.font_family weight slant (round st.font_size)

  let select_font_face t slant weight family =
    let st = get_state t in
    st.font_slant <- slant;
    st.font_weight <- weight;
    st.font_family <- family;
    Graphics.set_font (string_of_font st)

  let set_font_size t size =
    let st = get_state t in
    st.font_size <- size;
    (* Graphics.set_text_size (round size) *) (* no effect on unix *)
    Graphics.set_font (string_of_font st)

  let text_extents t txt =
    check_valid_handle t;
    let w, h = Graphics.text_size txt in
    { Matrix.x = 0.; y = 0.; w = float w ; h = float h }

  let rotate_extents angle x0 y0 w0 h0 =
    Printf.printf "DEBUG: Before: %f %f %f %f\n%!" x0 y0 w0 h0;
    let angle = mod_float angle (2. *. pi) in
    let sina, cosa = sin angle, cos angle in
    let x, y, w, h = -. w0 /. 2., -. h0 /. 2., w0, h0 in
    Printf.printf "DEBUG: Normalized: %f %f %f %f\n%!" x y w h;
    let xproj x y = cosa *. x -. sina *. y in
    let yproj x y = sina *. x +. cosa *. y in
    let x =
      if angle < pi /. 2. then xproj x (y +. h)
      else if angle < pi then xproj (x +. w) (y +. h)
      else if angle < 1.5 *. pi then xproj (x +. w) y
      else xproj x y
    and y =
      if angle < pi /. 2. then yproj x y
      else if angle < pi then yproj x (y +. h)
      else if angle < 1.5 *. pi then yproj (x +. w) (y +. h)
      else yproj (x +. w) y
    in
    Printf.printf "DEBUG: %f %f\n%!" x y;
    let retw, reth = -. 2. *. x, -. 2. *. y in
    let retx, rety = x0 -. (retw -. w0) /. 2., y0 -. (reth -. h0) /. 2. in
    Printf.printf "DEBUG: After: %f %f %f %f\n%!" retx rety retw reth;
    retx, rety, retw, reth

  let show_text t ~rotate ~x ~y pos txt =
    let st = get_state t in
    (* Compute the angle between the desired direction and the X axis
       in the device coord. system. *)
    let dx, dy = Matrix.transform_distance st.ctm (cos rotate) (sin rotate) in
    let angle = atan2 dy dx in
    let x', y' = Matrix.transform_point st.ctm x y in
    let w', h' = Graphics.text_size txt in
    (* text_size returns size already in device coords.*)
    let x'' = match pos with
      | Backend.CC | Backend.CT | Backend.CB -> x' -. float w' *. 0.5
      | Backend.RC | Backend.RT | Backend.RB -> x'
      | Backend.LC | Backend.LT | Backend.LB -> x' -. float w'
    and y'' =  match pos with
      | Backend.CC | Backend.RC | Backend.LC -> y' -. float h' *. 0.5
      | Backend.CT | Backend.RT | Backend.LT -> y'
      | Backend.CB | Backend.RB | Backend.LB -> y' -. float h'
    in let x'' = round x'' and y'' = round y'' in
    Graphics.moveto x'' y'';
    if abs_float angle <= 1e-6 then
      Graphics.draw_string txt
    else begin
      save t;
      (* Sauvegarde de la portion de travail *)
      let backup = Graphics.get_image x'' y'' w' h' in
      let invisx, invisy, invisw, invish =
        rotate_extents angle (float x'') (float y'') (float w') (float h')
      in
      (* Graphics.display_mode false; *)
      Graphics.set_color Graphics.white;
      Printf.printf "DEBUG(show_text): %f %f %f %f\n%!"
        invisx invisy invisw invish;
      Graphics.fill_rect (round invisx) (round invisy)
        (round invisw) (round invish);
      Graphics.moveto x'' y'';
      restore t;
      Graphics.draw_string txt;
      let m = Graphics.dump_image(Graphics.get_image x'' y'' w' h') in
      let m2 = Array.make_matrix (round invish) (round invish) (-1) in
      let place y x v =
        if v <> Graphics.white then
          let sina = sin angle and cosa = cos angle in
          let xcentered, ycentered = float (x - w' / 2), float (y - h' / 2) in
          let xrotated, yrotated =
            cosa *. xcentered -. sina *. ycentered +. invisw /. 2.,
            sina *. xcentered +. cosa *. ycentered +. invish /. 2.
          in
          try
            m2.(round yrotated).(round xrotated) <- v
          with _ -> ()
      in
      Array.iteri (fun y -> Array.iteri (place y)) m;
      let img2 = Graphics.make_image m2 in
      Graphics.draw_image backup x'' y'';
      (* Graphics.display_mode true; *)
      Graphics.draw_image img2 (round invisx) (round invisy);
      Graphics.synchronize()
    end

  let flipy _t = false
end

let () =
  let module U = Backend.Register(B) in
  if Sys.os_type = "Win32" then (
    (* Set offsets so the actual surface is of the requested size. *)
    Graphics.open_graph " 100x100";
    let w = Graphics.size_x ()
    and h = Graphics.size_y() in
    Graphics.close_graph ();
    B.ofsw := float (100 - w);
    B.ofsh := float (100 - h);
  )


(* Local Variables: *)
(* compile-command: "ocamlbuild -classic-display archimedes_graphics.cmo" *)
(* End: *)
