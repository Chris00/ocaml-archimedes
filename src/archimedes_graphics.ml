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
open Bigarray
module A = Archimedes
module Matrix = A.Matrix
module P = Archimedes_internals.Path

let min a b = if (a:float) < b then a else b
let max a b = if (a:float) > b then a else b

let round x = truncate(if x < 0. then x -. 0.5 else x +. 0.5)

let fourth_pi = atan 1.
let pi = 4. *. fourth_pi


module B =
struct
  let name = "graphics"

  let backend_to_device _ = Matrix.make_identity()
    (*A Graphics handle has already the "good" coordinates.*)
  let in_use = ref false (* only one Graphics handle can be created *)

  (* Record the state of various graphics values (in a form close to
     what Graphics needs). *)
  type state = {
    mutable color: int;
    mutable line_width : float;
    (* current line width.  When stroking, this is used to set the
       graphics line width, according to the CTM. *)
    mutable dash_offset: float;
    mutable dash: float array;
    mutable ctm : A.Matrix.t; (* current transformation matrix from the
                               user coordinates to the device ones. *)
    mutable font_slant: A.Backend.slant;
    mutable font_weight: A.Backend.weight;
    mutable font_family: string;
    mutable font_size : float;
    mutable clip : A.Matrix.rectangle;
    mutable clip_set : bool;
  }

  type t = {
    mutable closed: bool;
    hold: bool; (* on close, hold the windows until a key is pressed *)
    history: state Stack.t; (* saved states *)
    mutable state: state;   (* current state *)
    (* save/restore do not affect the current path. *)
    (* The current path, in device coordinates.  The path structure
       includes its extent and the current point. *)
    mutable current_path: A.Path.t
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
    let state_copy = { st with ctm = A.Matrix.copy st.ctm} in
    Stack.push state_copy t.history

  let restore t =
    check_valid_handle t;
    try
      let st = Stack.pop t.history in
      t.state <- st;
      (* Re-enable previous settings in case they were changed *)
      Graphics.set_color st.color
    with Stack.Empty ->
      invalid_arg "Archimedes_graphics.restore: no prvious save issued."

  (* On windows, the size given to open_graph is the one of the window
     WITH decorations.  These quantities tell how much need to be
     added to the width and height so that the surface has the desired
     size (set below, when the library is loaded). *)
  let ofsw = ref 0.
  and ofsh = ref 0.

  (* FIXME: options "x=" and "y=" for the position *)
  let make ~options width height =
    if !in_use then failwith "Archimedes_graphics.make: already in use";
    (* Parse options *)
    let hold = ref false in
    List.iter (fun o ->
                 if o = "hold" then hold := true;
              ) options;
    Graphics.open_graph(sprintf " %.0fx%.0f"
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
      ctm = A.Matrix.make_identity();
      font_slant = A.Backend.Upright;
      font_weight = A.Backend.Normal;
      font_family = "*";
      font_size = 10.;
      clip = { A.Matrix.x = nan; y = nan; w = nan; h = nan };
      clip_set = false;
    } in
    { closed = false;
      hold = !hold;
      history = Stack.create();
      state = state;
      current_path = A.Path.make();
    }

  let show _t = Graphics.synchronize()

  let close ~options:_ t =
    if not(t.closed) then (
      (* FIXME: Temporary solution, the interactive module must handle this. *)
      if t.hold then (
        Graphics.synchronize();
        Graphics.set_window_title "Archimedes [Press a key to close]";
        ignore(Graphics.wait_next_event [Graphics.Key_pressed]);
      );
      Graphics.close_graph();
      t.closed <- true;
      in_use := false;
    )

  let clear_path t =
    check_valid_handle t;
    A.Path.clear t.current_path

  let set_color t c =
    let st = get_state t in
    let r = round(A.Color.r c *. 255.)
    and g = round(A.Color.g c *. 255.)
    and b = round(A.Color.b c *. 255.) in
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
  let get_line_cap t = check_valid_handle t; A.Backend.ROUND
  let set_line_join t _ = check_valid_handle t
  let get_line_join t = check_valid_handle t; A.Backend.JOIN_MITER
  let set_miter_limit t _ = check_valid_handle t

  let move_to t ~x ~y =
    let st = get_state t in
    let x', y' = A.Matrix.transform_point st.ctm x y in
    A.Path.move_to t.current_path x' y'

  let line_to t ~x ~y =
    let st = get_state t in
    let x', y' = A.Matrix.transform_point st.ctm x y in
    A.Path.line_to t.current_path x' y'

  let rel_move_to t ~x ~y =
    let st = get_state t in
    let x, y = A.Matrix.transform_distance st.ctm x y in
    A.Path.rel_move_to t.current_path x y

  let rel_line_to t ~x ~y =
    let st = get_state t in
    let x',y' = A.Matrix.transform_distance st.ctm x y in
    A.Path.rel_line_to t.current_path x' y'

  let rectangle t ~x ~y ~w ~h =
    let st = get_state t in
    let x', y' = A.Matrix.transform_point st.ctm x y
    and w'x, w'y = A.Matrix.transform_distance st.ctm w 0.
    and h'x, h'y = A.Matrix.transform_distance st.ctm 0. h in
    A.Path.move_to t.current_path x' y';
    A.Path.rel_line_to t.current_path w'x w'y;
    A.Path.rel_line_to t.current_path h'x h'y;
    A.Path.rel_line_to t.current_path (-. w'x) (-. w'y);
    A.Path.close t.current_path

  let internal_curve_to t st ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    (* Suffices to transform the control point by the affine
       transformation to have the affine image of the curve *)
    let x1', y1' = A.Matrix.transform_point st.ctm x1 y1 in
    let x2', y2' = A.Matrix.transform_point st.ctm x2 y2 in
    let x3', y3' = A.Matrix.transform_point st.ctm x3 y3 in
    A.Path.curve_to t.current_path x1' y1' x2' y2' x3' y3'

  let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    internal_curve_to t (get_state t) ~x1 ~y1 ~x2 ~y2 ~x3 ~y3

  let arc_add_piece t st ~x0 ~y0 ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    internal_curve_to t st ~x1 ~y1 ~x2 ~y2 ~x3 ~y3

  let arc t ~r ~a1 ~a2 =
    let st = get_state t in
    (* One must transform the arc to bezier curves before acting with
       the CTM as it may deform the arc. *)
    let x, y =
      try A.Path.current_point t.current_path
      with _ -> failwith "archimedes_graphics.arc: no current point" in
    let x0, y0 = A.Matrix.inv_transform_point st.ctm x y in
    P.bezier_of_arc st (arc_add_piece t) ~x0 ~y0 ~r ~a1 ~a2

  let close_path t =
    check_valid_handle t;
    A.Path.close t.current_path

  let path_extents t = A.Path.extents t.current_path

  let clip_rectangle t ~x ~y ~w ~h =
    let st = get_state t in
    let x, y = A.Matrix.transform_point st.ctm x y in
    let w, h = A.Matrix.transform_distance st.ctm w h in
    st.clip <- { A.Matrix.x = x; y = y; w = w; h = h };
    st.clip_set <- true

  let translate t ~x ~y = A.Matrix.translate (get_state t).ctm x y

  let scale t ~x ~y = A.Matrix.scale (get_state t).ctm x y

  let rotate t ~angle = A.Matrix.rotate (get_state t).ctm ~angle

  let set_matrix t m =
    (*Replaces the ctm with a *copy* of m so that modifying m does not
      change the (newly set) coordinate system.*)
    (get_state t).ctm <- A.Matrix.copy m

  let get_matrix t = A.Matrix.copy (get_state t).ctm


  (* Real plotting procedures (perform clipping)
   ***********************************************************************)

  type box_curr_pt = {
    x0: float; y0: float; x1: float; y1: float; (* clipping box *)
    must_clip: bool;
    mutable x: float;  mutable y: float; (* current point *) }

  let box st =
    let m = st.clip in
    { x0 = m.A.Matrix.x;  y0 = m.A.Matrix.y;
      x1 = m.A.Matrix.x +. m.A.Matrix.w;  y1 = m.A.Matrix.y +. m.A.Matrix.h;
      must_clip = st.clip_set;
      x = nan; y = nan; }

  let eps = 1E-6
  let inside x0 x1 x = x0 -. eps <= x && x <= x1 +. eps
  let inside_box box x y =
    inside box.x0 box.x1 x && inside box.y0 box.y1 y

  (** Return the point of intersection between *)
  let intersection x1 y1 x2 y2 x3 y3 x4 y4 =
    (* Formula taken from Wikipedia; article "Line-line intersection" *)
    let f = 1. /. ((x1 -. x2) *. (y3 -. y4) -. (y1 -. y2) *. (x3 -. x4)) in
    let f1 = x1 *. y2 -. y1 *. x2 and f2 = x3 *. y4 -. y3 *. x4 in
    (f1 *. (x3 -. x4) -. (x1 -. x2) *. f2) *. f,
    (f1 *. (y3 -. y4) -. (y1 -. y2) *. f2) *. f

  let distance1 x y x' y' =
    abs_float (x -. x') +. abs_float (y -. y')

  let clip_point box x y x' y' =
    (* FIXME this code seems really improvable *)
    let x1, y1 = intersection box.x0 box.y0 box.x1 box.y0 x y x' y'
    and x2, y2 = intersection box.x1 box.y0 box.x1 box.y1 x y x' y'
    and x3, y3 = intersection box.x0 box.y1 box.x1 box.y1 x y x' y'
    and x4, y4 = intersection box.x0 box.y0 box.x0 box.y1 x y x' y' in
    let d1 = if inside box.x0 box.x1 x1 then distance1 x y x1 y1 else infinity
    and d2 = if inside box.y0 box.y1 y2 then distance1 x y x2 y2 else infinity
    and d3 = if inside box.x0 box.x1 x3 then distance1 x y x3 y3 else infinity
    and d4 = if inside box.y0 box.y1 y4 then distance1 x y x4 y4 else infinity in
    let data = [(x1, y1, d1); (x2, y2, d2); (x3, y3, d3); (x4, y4, d4)] in
    let third (_, _, d) = d in
    let default = (nan, nan, infinity) in
    let x, y, _ = List.fold_left
      (fun a b -> if third b < third a then b else a) default data
    in x, y

  let clipped_segment b x y x' y' =
    if b.must_clip then (
      let nx, ny =
        if inside_box b x y then x, y
        else clip_point b x y x' y'
      and nx', ny' =
        if inside_box b x' y' then x', y'
        else clip_point b x' y' x y
      in
    (* Note: If one variable (here nx) is correct, the other are also *)
      if nx < min x x' || nx > max x x' then (nan, nan, nan, nan)
      else (nx, ny, nx', ny')
    )
    else (x, y, x', y')

  let stroke_line_to b x y =
    let cx, cy, cx', cy' = clipped_segment b b.x b.y x y in
    if cx = cx && cx' = cx' (* no NaN *) then begin
      if cx <> b.x || cy <> b.y then Graphics.moveto (round cx) (round cy);
      Graphics.lineto (round cx') (round cy');
      if cx' <> x || cy' <> y then Graphics.moveto (round x) (round y)
    end
    else Graphics.moveto (round x) (round y);
    b.x <- x;
    b.y <- y

  (* FIXME: macro to avoid the call [to_bk] ? *)
  (* [to_bk x y]: transform coordinates to the graphics ones. *)
  let stroke_on_backend b to_bk = function
    | P.Move_to(x,y) ->
      let x, y = to_bk x y in
      Graphics.moveto (round x) (round y);
      b.x <- x;
      b.y <- y;
    | P.Line_to(x,y)
    | P.Close(x, y) ->
      let x, y = to_bk x y in
      stroke_line_to b x y
    | P.Array(x, y, i0, i1) ->
      if i0 <= i1 then
        for i = i0 to i1 do
          let x, y = to_bk x.(i) y.(i) in stroke_line_to b x y
        done
      else
        for i = i0 downto i1 do
          let x, y = to_bk x.(i) y.(i) in stroke_line_to b x y
        done
    | P.Fortran(x, y, i0, i1) ->
      if i0 <= i1 then
        for i = i0 to i1 do
          let x, y = to_bk x.{i} y.{i} in stroke_line_to b x y
        done
      else
        for i = i0 downto i1 do
          let x, y = to_bk x.{i} y.{i} in stroke_line_to b x y
        done
    | P.C(x, y, i0, i1) ->
      if i0 <= i1 then
        for i = i0 to i1 do
          let x, y = to_bk x.{i} y.{i} in stroke_line_to b x y
        done
      else
        for i = i0 downto i1 do
          let x, y = to_bk x.{i} y.{i} in stroke_line_to b x y
        done
    | P.Curve_to(_, _, x1, y1, x2, y2, x3, y3) ->
      let x1, y1 = to_bk x1 y1
      and x2, y2 = to_bk x2 y2
      and x3, y3 = to_bk x3 y3 in
      (* FIXME: clip Bézier curve *)
      Graphics.curveto
        (round x1, round y1) (round x2, round y2) (round x3, round y3)

  (* When the path is already in the backend coordinates, no need for
     the CTM. *)
  let id x y = (x, y)
  let stroke_preserve t =
    let st = get_state t in
    graphics_set_line_width st;
    P.iter t.current_path (stroke_on_backend (box st) id)

  let stroke t =
    stroke_preserve t;
    A.Path.clear t.current_path

  let stroke_path_preserve t path =
    let st = get_state t in
    graphics_set_line_width st;
    let to_bk x y = A.Matrix.transform_point st.ctm x y in
    P.iter path (stroke_on_backend (box st) to_bk)


  (* We will use the fill_poly primitive of graphics for all fillings.
     Thus one must transform each sub-path into an array of coordinates. *)
  let curve_nsamples = 20 (* curve_nsamples + 1 points *)
  let curve_dt = 1. /. float curve_nsamples

  let fill_line_to b x y coords =
    let cbx, cby, cx, cy = clipped_segment b b.x b.y x y in
    if cbx = cbx && cx = cx (* no NaN *) then begin
      if cbx <> b.x || cby <> b.y then
        coords := (round cbx, round cby) :: !coords;
      coords := (round cx, round cy) :: !coords
    end;
    b.x <- x;
    b.y <- y

  (* Add some points on the Bezier curve to [coords], except the 1st
     point which is supposed to be added by the previous component of
     the path. *)
  let add_curve_sampling b x0 y0  x1 y1  x2 y2  x3 y3 coords =
    for i = 1 to curve_nsamples do
      let t = float i *. curve_dt in
      let tm = 1. -. t in
      let t2 = t *. t   and tm2 = tm *. tm in
      let t3 = t2 *. t  and tm3 = tm2 *. tm in
      let t' = 3. *. t2 *. tm  and t'' = 3. *. t *. tm2 in
      let x = tm3 *. x0 +. t'' *. x1 +. t' *. x2 +. t3 *. x3
      and y = tm3 *. y0 +. t'' *. y1 +. t' *. y2 +. t3 *. y3 in
      (* FIXME: Clipping each subsegment is very expensive *)
      fill_line_to b x y coords
    done

  let fill_subpath = function
    | [] | [ _ ] -> ()
    | [ (x1,y1); (x0,y0) ] ->
      Graphics.moveto x0 y0;  Graphics.lineto x1 y1
    | coords -> Graphics.fill_poly (Array.of_list coords)

  let rec gather_subpath b to_bk coords = function
    | P.Move_to(x,y) ->
      fill_subpath !coords;  (* previous subpath *)
      coords := [];  (* Clean the coords of the subpath already filled. *)
      let x, y = to_bk x y in
      b.x <- x;
      b.y <- y;  (* Do no put the pt in coords in case 2 Move_to follow *)
    | P.Line_to(x,y)
    | P.Close(x,y) ->
      let x, y = to_bk x y in
      fill_line_to b x y coords
    | P.Array(x, y, i0, i1) ->
      if i0 <= i1 then
        for i = i0 to i1 do
          let x, y = to_bk x.(i) y.(i) in fill_line_to b x y coords
        done
      else
        for i = i0 downto i1 do
          let x, y = to_bk x.(i) y.(i) in fill_line_to b x y coords
        done
    | P.Fortran(x, y, i0, i1) ->
      if i0 <= i1 then
        for i = i0 to i1 do
          let x, y = to_bk x.{i} y.{i} in fill_line_to b x y coords
        done
      else
        for i = i0 downto i1 do
          let x, y = to_bk x.{i} y.{i} in fill_line_to b x y coords
        done
    | P.C(x, y, i0, i1) ->
      if i0 <= i1 then
        for i = i0 to i1 do
          let x, y = to_bk x.{i} y.{i} in fill_line_to b x y coords
        done
      else
        for i = i0 downto i1 do
          let x, y = to_bk x.{i} y.{i} in fill_line_to b x y coords
        done
    | P.Curve_to(x0,y0, x1,y1, x2,y2, x3,y3) ->
      let x0, y0 = to_bk x0 y0
      and x1, y1 = to_bk x1 y1
      and x2, y2 = to_bk x2 y2
      and x3, y3 = to_bk x3 y3 in
      add_curve_sampling b x0 y0 x1 y1 x2 y2 x3 y3 coords


  let fill_preserve t =
    let st = get_state t in
    (* Line width does not matter for "fill". *)
    let coords = ref [] in
    P.iter t.current_path (gather_subpath (box st) id coords);
    (* fill the last gathered path (even if no close was issued). *)
    fill_subpath !coords

  let fill t =
    fill_preserve t;
    A.Path.clear t.current_path

  let fill_with_color t c =
    let st = get_state t in
    let color = st.color in
    set_color t c;
    fill_preserve t;  (* no need to do additional work *)
    (* Restore color *)
    st.color <- color;
    Graphics.set_color color

  let fill_path_preserve t path =
    let st = get_state t in
    let to_bk x y = A.Matrix.transform_point st.ctm x y in
    (* Line width does not matter for "fill". *)
    let coords = ref [] in
    P.iter path (gather_subpath (box st) to_bk coords);
    fill_subpath !coords

  (* Fonts
   ***********************************************************************)

  (* FIXME: What about win32 and mac ? *)
  let string_of_font st =
    let slant = match st.font_slant with
      | A.Backend.Upright -> 'r'
      | A.Backend.Italic -> 'i' in
    let weight = match st.font_weight with
      | A.Backend.Normal -> "medium"
      | A.Backend.Bold -> "bold" in
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
    { A.Matrix.x = 0.; y = 0.; w = float w ; h = float h }

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

  (* Rotation of a color matrix image from center point using nearest
     neightbor sampling. Also gives the position of some point from source
     image in the new image. *)
  (* FIXME: Use Graphics Gems shearing rotation. GG 1, p. 179 *)
  let rotate_image simg px py angle =
    (* FIXME: If angle is a multiple of PI/2, use trivial transposition
       and symmetry. *)
    assert (Array.length simg > 0 && Array.length simg.(0) > 0);
    (* Source Width/Height. We measure distances between center of pixels,
       so a line of 3 pixels is of width 2. *)
    let sw = float (pred (Array.length simg.(0)))
    and sh = float (pred (Array.length simg)) in
    (* Output Width/Height. *)
    (* FIXME: having to set 0. 0. is totally useless; refactor that ! *)
    let _, _, ow, oh = rotate_extents angle 0. 0. sw sh in
    let ow', oh' = round ow, round oh in
    let swc, shc = sw *. 0.5, sh *. 0.5
    and owc, ohc = ow *. 0.5, oh *. 0.5
    and sina = -. sin angle and cosa = cos angle in
    (* Get color in source [simg] for (x, y) pixel in output image. *)
    let rot x y =
      (* dump_image gives a color matrix with coordinates as :
         [| [| (0, 1); (1, 1) |];
         [| (0, 0); (1, 0) |] |] *)
      let x', y' = x -. owc, float oh' -. y -. ohc in
      (* Output coords to source coords => inverse rotation. *)
      let xrot = cosa *. x' -. sina *. y'
      and yrot = sina *. x' +. cosa *. y' in
      (xrot +. swc, sh -. yrot -. shc)
    in
    let get_color x y =
      try
        let sx, sy = rot (float x) (float y) in
        simg.(round sy).(round sx)
      with Invalid_argument _ -> Graphics.transp
    in
    (Array.init (succ oh')
       (fun y -> Array.init (succ ow')
         (fun x -> get_color x y)),
    rot px py)

  (* Returns the color matrix with [str] printed in [color]. *)
  let string_img str color =
    let w, h = Graphics.text_size str in
    let buf = Array.make_matrix h w Graphics.transp in
    let max_w, max_h = Graphics.size_x (), Graphics.size_y () in
    let piece_w, piece_h = Pervasives.min w max_w, Pervasives.min h max_h in
    let w_pieces, h_pieces = piece_w / max_w, piece_h / max_h in
    let w_lst_piece, h_lst_piece = piece_w mod max_w, piece_h mod max_h in
    (* Save graphic work area. *)
    let backup = Graphics.get_image 0 0 piece_w piece_h in
    (* Setup graphic work area. *)
    let transp = abs (pred color) in
    Graphics.set_color transp;
    Graphics.fill_rect 0 0 w h;
    Graphics.set_color color;
    (* Copy pieces of the string image to [buf]. *)
    let x_offset, y_offset = ref 0, ref 0 in
    let cpy_piece x_offset y_offset piece_w piece_h i j =
      Graphics.moveto (-x_offset) y_offset;
      Graphics.draw_string str;
      let piece =
        Graphics.dump_image (Graphics.get_image 0 0 piece_w piece_h)
      in
      let update_buf_pixel i j oi oj color =
        buf.(i + oi).(j + oj) <-
          if color = transp then Graphics.transp else color
      in
      Array.iteri (fun oi ->
        Array.iteri (fun oj color -> update_buf_pixel i j oi oj color)) piece
    in
    for i = 0 to pred h_pieces do
      for j = 0 to pred w_pieces do
        cpy_piece !x_offset !y_offset piece_w piece_h i j;
        x_offset := !x_offset + piece_w
      done;
      (* Copy last horizontal partial piece. *)
      cpy_piece !x_offset !y_offset w_lst_piece piece_h i w_pieces;
      y_offset := !y_offset + piece_h
    done;
    (* Copy last partial piece. *)
    cpy_piece !x_offset !y_offset w_lst_piece h_lst_piece h_pieces w_pieces;
    (* Restore graphics work area. *)
    Graphics.draw_image backup 0 0;
    buf

  let show_text t ~rotate ~x ~y pos txt =
    let st = get_state t in
    (* Compute the angle between the desired direction and the X axis
       in the device coord. system. *)
    let dx, dy = A.Matrix.transform_distance st.ctm (cos rotate) (sin rotate) in
    let angle = atan2 dy dx in
    let x', y' = A.Matrix.transform_point st.ctm x y in
    let w', h' = Graphics.text_size txt in
    (* text_size returns size already in device coords.*)
    let px = match pos with
      | A.Backend.LC | A.Backend.LT | A.Backend.LB -> float w'
      | A.Backend.CC | A.Backend.CT | A.Backend.CB -> float w' *. 0.5
      | A.Backend.RC | A.Backend.RT | A.Backend.RB -> 0.
    and py = match pos with
      | A.Backend.CB | A.Backend.RB | A.Backend.LB -> float h'
      | A.Backend.CC | A.Backend.RC | A.Backend.LC -> float h' *. 0.5
      | A.Backend.CT | A.Backend.RT | A.Backend.LT -> 0.
    in
    if w' > 0 && h' > 0 then (
      if abs_float angle <= 1e-6 then
        (Graphics.moveto (round (x' -. px)) (round (y' -. py));
         Graphics.draw_string txt)
      else
        let rimg, (rx, ry) =
          rotate_image (string_img txt st.color) px py rotate
        in
        Graphics.draw_image (Graphics.make_image rimg)
          (round (x' -. rx)) (round (y' -. ry))
    )

  let flipy _t = false
end

let () =
  let module U = A.Backend.Register(B) in
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
(* compile-command: "make -C .." *)
(* End: *)
