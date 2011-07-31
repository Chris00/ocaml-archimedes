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
open Archimedes
module P = Archimedes_internals.Path

(* Re-export the labels so we do not have to qualify them with [Matrix]. *)
type matrix = Matrix.t = { mutable xx: float; mutable yx: float;
                           mutable xy: float; mutable yy: float;
                           mutable x0: float; mutable y0: float; }

let min a b = if (a:float) < b then a else b
let max a b = if (a:float) > b then a else b

let round x = truncate(if x >= 0. then x +. 0.5 else x -. 0.5)

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
    mutable ctm : Matrix.t; (* current transformation matrix from the
                               user coordinates to the device ones. *)
    mutable font_slant: Backend.slant;
    mutable font_weight: Backend.weight;
    mutable font_family: string;
    mutable font_size : float;
    mutable clip : Matrix.rectangle;
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
    mutable current_path: Path.t
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
      invalid_arg "Archimedes_graphics.restore: no prvious save issued."

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
      clip = { Matrix.x = nan; y = nan; w = nan; h = nan };
      clip_set = false;
    } in
    { closed = false;
      hold = !hold;
      history = Stack.create();
      state = state;
      current_path = Path.make();
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
    Path.clear t.current_path

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

  let move_to t ~x ~y =
    let st = get_state t in
    let x', y' = Matrix.transform_point st.ctm x y in
    Path.move_to t.current_path x' y'

  let line_to t ~x ~y =
    let st = get_state t in
    let x', y' = Matrix.transform_point st.ctm x y in
    Path.line_to t.current_path x' y'

  let rel_move_to t ~x ~y =
    let st = get_state t in
    let x, y = Matrix.transform_distance st.ctm x y in
    Path.rel_move_to t.current_path x y

  let rel_line_to t ~x ~y =
    let st = get_state t in
    let x',y' = Matrix.transform_distance st.ctm x y in
    Path.rel_line_to t.current_path x' y'

  let rectangle t ~x ~y ~w ~h =
    let st = get_state t in
    let x', y' = Matrix.transform_point st.ctm x y
    and w'x, w'y = Matrix.transform_distance st.ctm w 0.
    and h'x, h'y = Matrix.transform_distance st.ctm 0. h in
    Path.move_to t.current_path x' y';
    Path.rel_line_to t.current_path w'x w'y;
    Path.rel_line_to t.current_path h'x h'y;
    Path.rel_line_to t.current_path (-. w'x) (-. w'y);
    (* Call line_to which is less expensive than Path.close and is the
       same for graphics as there are no line caps. *)
    Path.line_to t.current_path x' y'

  let internal_curve_to t st ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    (* Suffices to transform the control point by the affine
       transformation to have the affine image of the curve *)
    let x1', y1' = Matrix.transform_point st.ctm x1 y1 in
    let x2', y2' = Matrix.transform_point st.ctm x2 y2 in
    let x3', y3' = Matrix.transform_point st.ctm x3 y3 in
    Path.curve_to t.current_path x1' y1' x2' y2' x3' y3'

  let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    internal_curve_to t (get_state t) ~x1 ~y1 ~x2 ~y2 ~x3 ~y3

  let arc_add_piece t st ~x0 ~y0 ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    internal_curve_to t st ~x1 ~y1 ~x2 ~y2 ~x3 ~y3

  let arc t ~r ~a1 ~a2 =
    let st = get_state t in
    (* One must transform the arc to bezier curves before acting with
       the CTM as it may deform the arc. *)
    let x, y =
      try Path.current_point t.current_path
      with _ -> failwith "archimedes_graphics.arc: no current point" in
    let x0, y0 = Matrix.inv_transform_point st.ctm x y in
    P.bezier_of_arc st (arc_add_piece t) ~x0 ~y0 ~r ~a1 ~a2

  let close_path t =
    check_valid_handle t;
    Path.close t.current_path

  let path_extents t = Path.extents t.current_path

  let clip_rectangle t ~x ~y ~w ~h =
    let st = get_state t in
    st.clip <- { Matrix.x = x; y = y; w = w; h = h };
    st.clip_set <- true

  let translate t ~x ~y = Matrix.translate (get_state t).ctm x y

  let scale t ~x ~y = Matrix.scale (get_state t).ctm x y

  let rotate t ~angle = Matrix.rotate (get_state t).ctm ~angle

  let set_matrix t m =
    (*Replaces the ctm with a *copy* of m so that modifying m does not
      change the (newly set) coordinate system.*)
    (get_state t).ctm <- Matrix.copy m

  let get_matrix t = Matrix.copy (get_state t).ctm


  (* Real plotting procedures (perform clipping)
   ***********************************************************************)

  let eps = 1E-6
  let inner_x x0 xend x = x0 -. eps <= x && x <= xend +. eps
  let inner_y y0 yend y = y0 -. eps <= y && y <= yend +. eps
  let inner (x0, xend, y0, yend) x y =
    inner_x x0 xend x && inner_y y0 yend y

  let intersection x1 y1 x2 y2 x3 y3 x4 y4 =
    (* Formula taken from Wikipedia; article "Line-line intersection" *)
    let f = 1. /. ((x1 -. x2) *. (y3 -. y4) -. (y1 -. y2) *. (x3 -. x4)) in
    let f1 = x1 *. y2 -. y1 *. x2 and f2 = x3 *. y4 -. y3 *. x4 in
    (f1 *. (x3 -. x4) -. (x1 -. x2) *. f2) *. f,
    (f1 *. (y3 -. y4) -. (y1 -. y2) *. f2) *. f

  let distance x y x' y' =
    abs_float (x -. x') +. abs_float (y -. y')

  let clip_point (x0, xend, y0, yend) x y x' y' =
    (* FIXME this code seems really improvable *)
    let x1, y1 = intersection x0 y0 xend y0 x y x' y'
    and x2, y2 = intersection xend y0 xend yend x y x' y'
    and x3, y3 = intersection x0 yend xend yend x y x' y'
    and x4, y4 = intersection x0 y0 x0 yend x y x' y' in
    let d1 = if inner_x x0 xend x1 then distance x y x1 y1 else infinity
    and d2 = if inner_y y0 yend y2 then distance x y x2 y2 else infinity
    and d3 = if inner_x x0 xend x3 then distance x y x3 y3 else infinity
    and d4 = if inner_y y0 yend y4 then distance x y x4 y4 else infinity in
    let data = [(x1, y1, d1); (x2, y2, d2); (x3, y3, d3); (x4, y4, d4)] in
    let third (_, _, d) = d in
    let default = (nan, nan, infinity) in
    let x, y, _ = List.fold_left
      (fun a b -> if third b < third a then b else a) default data
    in x, y

  let clipped_segment limits x y x' y' =
    let nx, ny =
      if inner limits x y then x, y
      else clip_point limits x y x' y'
    and nx', ny' =
      if inner limits x' y' then x', y'
      else clip_point limits x' y' x y
    in
    (* Note: If one variable (here nx) is correct, the other are also *)
    if nx < min x x' || nx > max x x' then (nan, nan, nan, nan)
    else (nx, ny, nx', ny')

  (* FIXME: macro to avoid the call [to_bk] ? *)
  (* [to_bk x y]: transform coordinates to the graphics ones. *)
  let stroke_on_backend st to_bk = function
    | P.Move_to(x,y) ->
      let x, y = to_bk x y in
      Graphics.moveto (round x) (round y)
    | P.Line_to(x,y) ->
      let x, y = to_bk x y in
        (* let cx, cy, cx', cy' = clipped_segment clip curx cury x y in
           if cx = cx && cx' = cx' then begin
           if cx <> curx || cy <> cury then Backend.move_to b cx cy;
           Backend.line_to b cx' cy';
           if cx' <> x || cy' <> y then Backend.move_to b x y
           end
           else Backend.move_to b x y; *)
      Graphics.lineto (round x) (round y)
    | P.Curve_to(_, _, x1, y1, x2, y2, x3, y3) ->
      let x1, y1 = to_bk x1 y1
      and x2, y2 = to_bk x2 y2
      and x3, y3 = to_bk x3 y3 in
      Graphics.curveto
        (round x1, round y1) (round x2, round y2) (round x3, round y3)
    | P.Close(x, y) ->
      let x, y = to_bk x y in
      Graphics.lineto (round x) (round y)
    | P.Array(x, y) ->
      for i = 0 to Array.length x - 1 do
        let x, y = to_bk x.(i) y.(i) in
        Graphics.lineto (round x) (round y)
      done
    | P.Fortran(x, y) ->
      for i = 1 to Array1.dim x do
        let x, y = to_bk x.{i} y.{i} in
        Graphics.lineto (round x) (round y)
      done

  (* When the path is already in the backend coordinates, no need for
     the CTM. *)
  let id x y = (x, y)
  let stroke_preserve t =
    let st = get_state t in
    let clip = st.clip in
    Queue.iter (stroke_on_backend st id) (P.data t.current_path);
    Graphics.synchronize()

  let stroke t = stroke_preserve t; clear_path t

  let stroke_path_preserve t path =
    failwith "FIXME: to be implemented"


  (* We will use the fill_poly primitive of graphics for all fillings.
     Thus one must transform each sub-path into an array of coordinates. *)
  let curve_nsamples = 20 (* curve_nsamples + 1 points *)
  let curve_dt = 1. /. float curve_nsamples

  (* Add some points on the Bezier curve to [coords] in *reverse*
     order, except the 1st point which is sopposed to be added by the
     previous component of the path. *)
  let add_curve_sampling x0 y0  x1 y1  x2 y2  x3 y3 coords =
    for i = curve_nsamples downto 1 do
      let t = float i *. curve_dt in
      let tm = 1. -. t in
      let t2 = t *. t   and tm2 = tm *. tm in
      let t3 = t2 *. t  and tm3 = tm2 *. tm in
      let t' = 3. *. t2 *. tm  and t'' = 3. *. t *. tm2 in
      let x = tm3 *. x0 +. t'' *. x1 +. t' *. x2 +. t3 *. x3
      and y = tm3 *. y0 +. t'' *. y1 +. t' *. y2 +. t3 *. y3 in
      coords := (round x, round y) :: !coords;
    done

  let fill_subpath = function
    | [] | [ _ ] -> ()
    | [ (x1,y1); (x0,y0) ] ->
          Graphics.moveto x0 y0;  Graphics.lineto x1 y1
    | coords -> Graphics.fill_poly (Array.of_list coords)

  let rec gather_subpath coords = function
    | P.Move_to(x,y) ->
      fill_subpath !coords;
      coords := [(round x, round y)]
    | P.Line_to(x,y) ->
      coords := (round x, round y) :: !coords
    | P.Close(x,y) ->
      fill_subpath ((round x, round y) :: !coords);
      coords := []
    | P.Curve_to(x0,y0, x1,y1, x2,y2, x3,y3) ->
      add_curve_sampling x0 y0 x1 y1 x2 y2 x3 y3 coords
    | P.Array(x, y) ->
      for i = 0 to Array.length x - 1 do
        coords := (round x.(i), round y.(i)) :: !coords
      done
    | P.Fortran(x, y) ->
      for i = 1 to Array1.dim x do
        coords := (round x.{i}, round y.{i}) :: !coords
      done

  let fill_preserve t =
    check_valid_handle t;
    (* Line width does not matter for "fill". *)
    let path = ref [] in
    Queue.iter (gather_subpath path) (P.data t.current_path);
    Graphics.synchronize()

  let fill t = fill_preserve t; clear_path t

  let fill_path_preserve t path =
    failwith "FIXME: to be implemented"

  (* Fonts
   ***********************************************************************)

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
(* compile-command: "make -C .." *)
(* End: *)
