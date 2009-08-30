(* File: archimedes_tikz.ml

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

(** TikZ Archimedes plugin *)

open Printf
module Backend = Archimedes.Backend

(* Re-export the labels so we do not have to qualify them with [Backend]. *)
type matrix = Archimedes.matrix = { mutable xx: float; mutable yx: float;
                                    mutable xy: float; mutable yy: float;
                                    mutable x0: float; mutable y0: float; }

let is_infinite x = 1. /. x = 0.
let min a b = if (a:float) < b then a else b
let max a b = if (a:float) > b then a else b

let pi =  4. *. (atan 1.)

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


let point_string x y = "("^(string_of_float x)^", "^(string_of_float y)^")"

module B =
struct
  let name = "tikz"

   (* Record the state of various graphics values*)
  type state = {
    mutable color: string;
    mutable opacity: string;
    mutable linewidth:string;
    mutable dash_offset: string;
    mutable dash: string;
    mutable dash_data: float array * float;
    mutable line_cap: string;
    mutable line_join: string;
    mutable miter_limit: string;
    (*All options, ready to be printed.*)
    mutable curr_pt: bool;
    (*Says whether a current point is active.*)
    mutable x:float;
    mutable y:float;
    (*Coordinates of the current point (if any)*)
    mutable fsize:float;
    mutable slant_weight_family:string;
    mutable path_extents: Backend.rectangle;
    (* The extent of the current path, in device coordinates. *)
    mutable ctm : matrix; (* current transformation matrix from the
                             user coordinates to the device ones. *)
  }

  type t = {
    mutable color_number: int; (*used to give colors a unique name*)
    mutable closed: bool;
    mutable state:state;
    mutable curr_path:string;
    history: state Stack.t;
    writer: out_channel;(*writes on the file*)
    mutable indent:int;(*indentation of TikZ code*)
   }

  let write t txt =
    let spaces = String.make t.indent ' ' in
    output_string t.writer (spaces^txt^"\r\n")

  let check_valid_handle t =
    if t.closed then failwith "Archimedes_tikz: handle closed"

  let get_state t =
    check_valid_handle t; t.state

 (* let print_ctm st =
    let m = st.ctm in
    Printf.sprintf "{ %f, %f, %f, %f, (%f, %f)}" m.xx m.xy m.yx m.yy m.x0 m.y0*)

  let make_options t =
    let st = get_state t in
   (* "cm="^print_ctm st^","^*)
      st.color^", "^
      st.opacity^", "^
      st.linewidth^", "^
      st.dash_offset^", "^
      st.dash^", "^
      st.line_cap^", "^
      st.line_join^", "^
      st.miter_limit

  let save t =
    let st = get_state t in
    (* Make a copy of the record so that further actions do not modify it *)
    let state_copy = { st with color = st.color } in
    Stack.push state_copy t.history;
    write t "\\begin{scope}";
    t.indent <- t.indent + 2

  let restore t =
    check_valid_handle t;
    try
      let st = Stack.pop t.history in
      t.state <- st;
      (* Re-enable previous settings in case they were changed *)
      write t "\\end{scope}";
      t.indent <- t.indent - 2
    with Stack.Empty -> ()

  let make ~options width height =
    match options with
      [name] ->
        let writer = open_out name in
        output_string writer "%%%Generated by Archimedes\r\n";
        output_string writer "\\begin{tikzpicture}[x=1pt,y=1pt]\n";
        (*FIXME: coherence with other backends.*)
        let w,h = string_of_float width, string_of_float height in
        let device_rectangle =
          "(0,0) rectangle ("^w^","^h^")"
        in
        (*Forbids automatic scaling of TikZ (we do it in higher levels)*)
        output_string writer
          ("\\clip[use as bounding box] "^device_rectangle^";\n");
        let state = {
          color = "color=black";
          opacity = "opacity=1";
          linewidth ="line width=1";
          dash_offset = "dash phase=0";
          dash = "solid";
          dash_data = [||], 0.;
          line_cap = "line cap=butt";
          line_join = "line join=miter";
          miter_limit = "miter limit=10";
          curr_pt = false; x=0.;y=0.; (*No current point*)
          fsize=10.;
          slant_weight_family = "%s";
          path_extents = { Backend.x=0.; y=0.; w=0.; h=0. };
          (* Identity transformation matrix *)
          ctm = Archimedes.Matrix.make_identity();
        } in
        { color_number=0;
          closed = false;
          history = Stack.create();
          state = state;
          curr_path = "";
          writer = writer;
          indent = 2;
        }
    | _ -> invalid_arg "Archimedes_tikz.make"

  let close ~options:_ t =
    if not(t.closed) then (
      write t "\\end{tikzpicture}";
      close_out t.writer;
      t.closed <- true;
    )

  let set_color t c =
    let st =  get_state t in
    let r,g,b,a = Archimedes.Color.get_rgba c in
    write t (Printf.sprintf
               "\\definecolor{archimedes_tikz %i}{rgb}{%.2f, %.2f, %.2f}"
               t.color_number r g b);
    st.color <- "color=archimedes_tikz "^(string_of_int t.color_number);
    st.opacity <- "opacity="^(string_of_float a);
    t.color_number <- t.color_number + 1

  let set_line_width t w =
    let st = get_state t in
    st.linewidth <- sprintf "line width=%f" w

  let get_line_width t =
    let s = (get_state t).linewidth in
    let len = String.length s in
    let s' = String.sub s 11 (len - 13) in (*removes 'line width=' and 'cm'*)
    float_of_string s'


  let set_dash t ofs arr =
    let st = get_state t in
    st.dash_offset <- "dash phase="^(string_of_float ofs);
    let switch = function
      | "on " -> "off "
      | "off " -> "on "
      | _ -> "" (*won't be used*)
    in
    let rec make_dash i mode str =
      try
        let data = string_of_float arr.(i) in
        let new_str = str^mode^data^" " in
        make_dash (i+1) (switch mode) new_str
      with Invalid_argument _ ->
        if str = "" then "solid"
        else if i mod 2 <> 0 then (*adds an "off" with last available value*)
          str^mode^(string_of_float arr.(i-1))^" "
        else str
    in
    st.dash <- "dash pattern="^(make_dash 0 "on " "");
    st.dash_data <- arr, ofs

  let get_dash t =
    let st = get_state t in st.dash_data

  let set_line_cap t cap =
    let st = get_state t in
    st.line_cap <- "line cap="^
      (match cap with
         Backend.BUTT -> "butt"
       | Backend.SQUARE -> "rect"
       | Backend.ROUND -> "round")

  let get_line_cap t =
    let st = get_state t in
    match String.sub st.line_cap 9 (String.length st.line_cap - 9) with
      (*Removes first characters: 'line cap='*)
      "butt" -> Backend.BUTT
    | "rect" -> Backend.SQUARE
    | "round" -> Backend.ROUND
    | _ -> failwith ("Archimedes TikZ.line_cap: error; cannot parse "
                     ^( String.sub st.line_cap 9 (String.length st.line_cap - 9)))


  let set_line_join t join =
    let st = get_state t in
    st.line_join <- "line join="^
      (match join with
         Backend.JOIN_BEVEL -> "bevel"
       | Backend.JOIN_MITER -> "miter"
       | Backend.JOIN_ROUND -> "round")

  let get_line_join t =
    let st = get_state t in
    match String.sub st.line_join 10 (String.length st.line_join - 10) with
      (*Removes first characters: 'line join='*)
      "bevel" -> Backend.JOIN_BEVEL
    | "miter" -> Backend.JOIN_MITER
    | "round" -> Backend.JOIN_ROUND
    | _ -> failwith ("Archimedes TikZ.line_join: error; cannot parse "
                     ^( String.sub st.line_join 10
                          (String.length st.line_join - 10)))


  let set_miter_limit t lim =
    let st = get_state t in
    st.miter_limit <- "miter limit="^(string_of_float lim)

  (* Paths are not acted upon directly but wait for [stroke] or [fill]. *)
  let move_to t ~x ~y =
    let st = get_state t in
    let x,y = Archimedes.Matrix.transform_point st.ctm x y in
    (* move only updates the current point but not the path extents *)
    st.curr_pt <- true;
    st.x <- x;
    st.y <- y;
    t.curr_path <- t.curr_path^" "^(point_string x y)

  let line_to t ~x ~y =
    let st = get_state t in
    let x',y' = Archimedes.Matrix.transform_point st.ctm x y in
    (*Note: if there's no current point then line_to behaves as move_to.*)
    if st.curr_pt then (
      t.curr_path <- t.curr_path^" -- "^(point_string x' y');
      (* Update extents and current point *)
      let x0', y0' = Archimedes.Matrix.transform_point st.ctm st.x st.y in
      st.path_extents <- update_rectangle st.path_extents x0' y0' x' y';
    )
    else
    t.curr_path <- t.curr_path^" "^(point_string x' y');
    st.curr_pt <- true;
    st.x <- x';
    st.y <- y'

  let rel_move_to t ~x ~y =
    let st = get_state t in
    let x,y = Archimedes.Matrix.transform_point st.ctm x y in
    t.curr_path <- t.curr_path^" ++"^(point_string x y);
    st.curr_pt <- true;
    st.x <- st.x +. x;
    st.y <- st.y +. y

  let rel_line_to t ~x ~y =
    let st = get_state t in
    let x,y = Archimedes.Matrix.transform_point st.ctm x y in
    t.curr_path <- t.curr_path^" -- ++"^(point_string x y);
    st.curr_pt <- true;
    st.x <- st.x +. x;
    st.y <- st.y +. y


  let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    (* Suffices to transform the control point by the affine
       transformation to have the affine image of the curve *)
    let st = get_state t in
    let x1', y1' = Archimedes.Matrix.transform_point st.ctm x1 y1 in
    let x2', y2' = Archimedes.Matrix.transform_point st.ctm x2 y2 in
    let x3', y3' = Archimedes.Matrix.transform_point st.ctm x3 y3 in
    let x0', y0' =
      if not st.curr_pt then
        (t.curr_path <- t.curr_path^" "^(point_string x1' y1');
         x1', y1')
      else st.x, st.y
    in
    t.curr_path <- t.curr_path^" ..controls "^(point_string x1' y1')^
      " and "^(point_string x2' y2')^".. "^(point_string x3' y3');
    (* Update the current point and extents *)
    st.path_extents <-
      update_curve st.path_extents x0' y0' x1' y1' x2' y2' x3' y3';
    st.curr_pt <- true;
    st.x <- x3';
    st.y <- y3'


  let rectangle t ~x ~y ~w ~h =
    let st = get_state t in
    let x', y' = Archimedes.Matrix.transform_point st.ctm x y
    and w', h' = Archimedes.Matrix.transform_distance st.ctm w h in
    t.curr_path <-t.curr_path^" "^(point_string x' y')^" rectangle "^
      (point_string (x'+.w') (y'+.h'));
    (* Update the current point and extents *)
    st.path_extents <-
      update_rectangle st.path_extents x' y' (x' +. w') (y' +. h');
    st.curr_pt <- true


  (*FIXME: to be reworked, to take the coordinate transformation into
    account.*)
  let arc t ~x ~y ~r ~a1 ~a2 =
    let st = get_state t in
    let x', y' = Archimedes.Matrix.transform_point st.ctm x y
    in
    move_to t x' y';
    let rec arc a1 a2 = (*FIXME: angles expressed in degrees here*)
      if abs_float (a2 -. a1) >= 360. +. 1.E-16 then
        (t.curr_path <- t.curr_path^" circle("^(string_of_float r)^")";
         let a2 =
           if a2 -. a1 >= 360. +. 1.E-16 then a2 -. 360.
           else a2 +. 360.
         in
         arc a1 a2)
      else
        let arcfin = " arc("^(string_of_float a1)^
          ":"^(string_of_float a2)^":"^
          (string_of_float r)^")"
        in
        t.curr_path <- t.curr_path^arcfin
    in arc (180. *. a1 /. pi) (180.*. a2 /. pi)

  let close_path t =
    let st = get_state t in
    if st.curr_pt then t.curr_path <- t.curr_path^" -- cycle"

  let clear_path t =
    check_valid_handle t;
    t.curr_path <- "";
    t.state.curr_pt <- false;
    t.state.path_extents <- { Backend.x=0.; y=0.; w=0.; h=0. }

  let path_extents t = (get_state t).path_extents

  let stroke_preserve t =
    let opts = make_options t in
    write t ("\\draw["^opts^"] ");
    t.indent <- t.indent + 2;
    write t (t.curr_path^";");
    t.indent <- t.indent - 2

  let stroke t = stroke_preserve t; clear_path t

  let fill_preserve t =
    let opts = make_options t in
    write t ("\\fill["^opts^"] ");
    t.indent <- t.indent + 2;
    write t (t.curr_path^";");
    t.indent <- t.indent - 2

  let fill t = fill_preserve t; clear_path t

  let clip_rectangle t ~x ~y ~w ~h =
    write t ("\\clip "^(point_string x y)^
               " rectangle "^(point_string (x+.w) (y+.h))^";")


  let translate t ~x ~y = Archimedes.Matrix.translate (get_state t).ctm x y

  let scale t ~x ~y = Archimedes.Matrix.scale (get_state t).ctm x y

  let rotate t ~angle = Archimedes.Matrix.rotate (get_state t).ctm ~angle

  let set_matrix t m = (get_state t).ctm <- m

  let get_matrix t = Archimedes.Matrix.copy (get_state t).ctm
  
  let select_font_face t slant weight family =
    check_valid_handle t;
    let begin_slant, end_slant =
      match slant with
        Backend.Upright -> "",""
      | Backend.Italic -> "\textit{","}"
      (*| Backend.Oblique -> "\textsl{","}"(*Slanted*)*)
    in
    let begin_weight, end_weight =
      match weight with
        Backend.Normal -> "",""
      | Backend.Bold -> "\textbf{","}"
    in
    let begin_family, end_family =
      match family with
        _ -> "",""
    in
    let s = begin_slant^begin_weight^begin_family^"%s"^
      end_family^end_weight^end_slant
    in
    t.state.slant_weight_family <- s;
    let series =
      match weight with
        Backend.Normal -> "n"
      | Backend.Bold -> "b"
    and shape =
      match slant with
        Backend.Upright -> "m"
      | Backend.Italic -> "it"
     (* | Backend.Oblique -> "sl" (*Slanted*)*)
    in
    write t (Printf.sprintf "\\usefont{T1}{%s}{%s}{%s}"
               family series shape)

  let set_font_size t size =
    let st = get_state t in
    let prev_size = st.fsize in
    st.fsize <- size;
    let ratio = prev_size /. size in
    write t (Printf.sprintf
               "\\fontsize{%.2f}{%.2f\\f@baselineskip}\\selectfont"
               size ratio)


(*FIXME: raw way to find the extents; must be reworked.*)
  let text_extents t txt =
    let h = (get_state t).fsize in
    let w = (float (String.length txt)) *. h in
    { Backend.x = 0.; y = 0.; w = w ; h = h }

  let show_text t ~rotate ~x ~y pos txt =
    let st = get_state t in
    (* Compute the angle between the desired direction and the X axis
       in the device coord. system. *)
    let dx, dy =
      Archimedes.Matrix.transform_distance st.ctm (cos rotate) (sin rotate) in
    let angle = atan2 dy dx in
    let x', y' = Archimedes.Matrix.transform_point st.ctm x y in
    let pos = match pos with
      | Backend.LT -> "south east"
      | Backend.LC -> "east"
      | Backend.LB -> "north east"
      | Backend.CT -> "south"
      | Backend.CC -> "center"
      | Backend.CB -> "north"
      | Backend.RT -> "south west"
      | Backend.RC -> "west"
      | Backend.RB -> "north west"
    in
    (*let txt = Printf.sprintf (format_of_string st.slant_weight_family) txt in*)
    write t (Printf.sprintf
               "\\node [%s, %s, %s, anchor=%s,rotate=%f] at %s {%s};"
               "inner sep=0pt" st.color st.opacity
               pos (180. *. angle /. pi) (point_string x' y') txt)
end

let () =
  let module U = Backend.Register(B)  in ()


(* Local Variables: *)
(* compile-command: "make -k" *)
(* End: *)
