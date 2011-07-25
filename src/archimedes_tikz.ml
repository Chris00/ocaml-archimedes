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
open Archimedes

(* Re-export the labels so we do not have to qualify them with [Archimedes]. *)
type matrix = Matrix.t = { mutable xx: float; mutable yx: float;
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
    (*Coordinates of the current point (if any), in device coordinates.*)
    mutable fsize:float;
    mutable slant_weight_family:string;
    mutable path_extents: Matrix.rectangle;
    (* The extent of the current path, in device coordinates. *)
    mutable ctm : matrix; (* current transformation matrix from the
                             user coordinates to the device ones. *)
  }

  type t = {
    mutable color_number: int; (*used to give colors a unique name*)
    mutable closed: bool;
    mutable state:state;
    mutable curr_path:string Queue.t;
    history: state Stack.t;
    fh: out_channel; (* channel on which one writes the TikZ code *)
   }

  let check_valid_handle t =
    if t.closed then failwith "Archimedes_tikz: handle closed"

  let get_state t =
    check_valid_handle t; t.state

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
    (* Make a copy of the record so that further actions do not modify
       it. We need to store a *copy* of the ctm, because
       scaling/translation/rotation mustn't modify this stored
       matrix. On the other hand, the path is common to the original
       and the copy.*)
    let state_copy = { st with ctm = Matrix.copy st.ctm} in
    Stack.push state_copy t.history;
    fprintf t.fh "\\begin{scope}"

  let restore t =
    check_valid_handle t;
    try
      let st = Stack.pop t.history in
      t.state <- st;
      (* Re-enable previous settings in case they were changed *)
      fprintf t.fh "\\end{scope}";
    with Stack.Empty ->
      invalid_arg "Archimedes_tikz.restore: no save issued."

  let backend_to_device _ = Matrix.make_identity()

  (*To avoid typos:*)
  let preamble = "preamble"

  let make ~options width height =
    match options with
    | name :: opts ->
        let fh = open_out name in
        output_string fh "%%% Generated by Archimedes OCaml module\n";
        (*Automatic scaling of TikZ?*)
        (match opts with
         | [] -> fprintf fh "\\clip (0,0) rectangle (%g,%g);\n" width height
         | ["bb"] ->
             fprintf fh "\\clip[use as bounding box] (0,0) rectangle (%g,%g);\n"
               width height
         | ["draw"] ->
             fprintf fh "\\draw[black] (0,0) rectangle (%g,%g);\n" width height
         | ["draw"; "bb"] ->
             fprintf fh "\\draw[black, use as bounding box] (0,0) \
	       rectangle (%g,%g);\n" width height
         | _ -> failwith "Archimedes_tikz.make -- unknown options");
        let state = {
          color = "color=black";
          opacity = "opacity=1";
          linewidth ="line width=1";
          dash_offset = ""; (* FIXME: dash phase=0 gives an error *)
          dash = "solid";
          dash_data = [||], 0.;
          line_cap = "line cap=butt";
          line_join = "line join=miter";
          miter_limit = "miter limit=10";
          curr_pt = false; x=0.;y=0.; (*No current point*)
          fsize=10.;
          slant_weight_family = "%s";
          path_extents = { Matrix.x=0.; y=0.; w=0.; h=0. };
          ctm = Matrix.make_identity();
        } in
        { color_number=0;
          closed = false;
          history = Stack.create();
          state = state;
          curr_path = Queue.create ();
          fh = fh;
        }
    | [] -> invalid_arg "Archimedes_tikz.make"

  let close ~options:_ t =
    if not t.closed then (
      if not (Queue.is_empty t.curr_path) then
        printf "Archimedes_tikz -- warning : closing a backend which contains \
          a non-empty path.\n%!";
      close_out t.fh;
      t.closed <- true;
    )

  let set_color t c =
    let st =  get_state t in
    let r,g,b,a = Archimedes.Color.get_rgba c in
    fprintf t.fh "\\definecolor{archimedes_color%i}{rgb}{%f, %f, %f}"
      t.color_number r g b;
    st.color <- sprintf "color=archimedes_color%i" t.color_number;
    st.opacity <- sprintf "opacity=%f" a;
    t.color_number <- t.color_number + 1

  let set_line_width t w =
    let st = get_state t in
    (*
    (*Note: Line width is coordinate-dependent: need to adapt it to
      device coords.*)
    let m = (*Matrix.copy*) st.ctm in
    (*Matrix.invert m;*)
    let x1,x2 = Matrix.transform_distance m w 0.
    and x3,x4 = Matrix.transform_distance m 0. w in
    (*FIXME: is there a better choice for width?*)
    let w = sqrt(x1*.x1 +. x2*.x2 +. x3*.x3 +. x4*.x4) in*)
    st.linewidth <- sprintf "line width=%f" w

  let get_line_width t =
    let s = (get_state t).linewidth in
    let len = String.length s in
    let s' = String.sub s 11 (len - 11) in (*removes 'line width='*)
    float_of_string s'


  let set_dash t ofs arr =
    let st = get_state t in
    (* FIXME: dash phase=... gives an error.  See mail. *)
    (* st.dash_offset <- "dash phase="^(string_of_float ofs); *)
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
    let cap_string =
      (*st.line_cap where first characters: 'line cap=' are removed.  *)
      String.sub st.line_cap 9 (String.length st.line_cap - 9)
    in
    match cap_string with
      "butt" -> Backend.BUTT
    | "rect" -> Backend.SQUARE
    | "round" -> Backend.ROUND
    | _ -> failwith ("Archimedes TikZ.line_cap: error; cannot parse "
                     ^cap_string)


  let set_line_join t join =
    let st = get_state t in
    st.line_join <- "line join="^
      (match join with
         Backend.JOIN_BEVEL -> "bevel"
       | Backend.JOIN_MITER -> "miter"
       | Backend.JOIN_ROUND -> "round")

  let get_line_join t =
    let st = get_state t in
    let join_string =
      (*Removes first characters: 'line join='*)
      String.sub st.line_join 10 (String.length st.line_join - 10)
    in
    match join_string with
      "bevel" -> Backend.JOIN_BEVEL
    | "miter" -> Backend.JOIN_MITER
    | "round" -> Backend.JOIN_ROUND
    | _ -> failwith ("Archimedes TikZ.line_join: error; cannot parse "
                     ^join_string)


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
    Queue.add (sprintf "(%.10f,%.10f)" x y) t.curr_path

  let line_to t ~x ~y =
    let st = get_state t in
    let x',y' = Archimedes.Matrix.transform_point st.ctm x y in
    (*Note: if there's no current point then line_to behaves as move_to.*)
    if st.curr_pt then (
      Queue.push (sprintf " -- (%.10f,%.10f)" x' y') t.curr_path;
      (* Update extents and current point *)
      let x0', y0' = Archimedes.Matrix.transform_point st.ctm st.x st.y in
      st.path_extents <- update_rectangle st.path_extents x0' y0' x' y';
    )
    else
      Queue.add (sprintf "(%.10f,%.10f)" x' y') t.curr_path;
    st.curr_pt <- true;
    st.x <- x';
    st.y <- y'

  let rel_move_to t ~x ~y =
    let st = get_state t in
    let x,y = Archimedes.Matrix.transform_point st.ctm x y in
    if st.curr_pt then
      Queue.add (sprintf " ++ (%.10f,%.10f)" x y) t.curr_path
    else failwith "Archimedes_tikz : no_current point";
    st.curr_pt <- true;
    st.x <- st.x +. x;
    st.y <- st.y +. y

  let rel_line_to t ~x ~y =
    let st = get_state t in
    let x,y = Archimedes.Matrix.transform_distance st.ctm x y in
    let x' = st.x +. x
    and y' = st.y +. y in
    if st.curr_pt then (
      Queue.add (sprintf "-- ++(%.10f,%.10f)" x y) t.curr_path;
      (* Update extents and current point *)
      st.path_extents <- update_rectangle st.path_extents st.x st.y x' y'
    )
    else failwith "Archimedes_tikz : no_current point";
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
      if not st.curr_pt then (
        Queue.add (sprintf "(%.10f,%.10f)" x1' y1') t.curr_path;
         x1', y1'
      )
      else st.x, st.y  in
    let curve =
      sprintf " ..controls (%.10f,%.10f) and (%.10f,%.10f).. (%.10f,%.10f)"
        x1' y1' x2' y2' x3' y3'  in
    Queue.add curve t.curr_path;
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
    let rect = sprintf "(%.10f,%.10f) rectangle (%.10f,%.10f)"
      x' y' (x'+.w') (y'+.h')  in
    Queue.add rect t.curr_path;
    (* Update the current point and extents *)
    st.path_extents <-
      update_rectangle st.path_extents x' y' (x' +. w') (y' +. h');
    st.curr_pt <- true



  let arc t ~r ~a1 ~a2 =
    let st = get_state t in
    (*Coordinate transformations*)
    let rcos t = r *. cos t
    and rsin t = r *. sin t in
    let x1, y1 = Archimedes.Matrix.transform_distance st.ctm (rcos a1) (rsin a1)
    and x2, y2 = Archimedes.Matrix.transform_distance st.ctm (rcos a2) (rsin a2)
    in
    let loops = truncate (abs_float(a2 -. a1) /. (2. *.pi)) in
    let a1' = atan2 y1 x1
    and a2' = atan2 y2 x2 in
    (* printf "%g : %g now are %g : %g with %d loops\n%!"
      a1 a2 a1' a2' loops;*)
    (*FIXME: to be reworked, to take the coordinate transformation into
      account (eg. different scalings along the axes).*)
    let r',_ = Archimedes.Matrix.transform_distance st.ctm r 0. in
    (*Note: without the following, angles are taken "mod 360", so
      circles could not be made using 0 -- 2 pi as (original) argument.*)
    (*TikZ uses the current point as starting point for the arc, but
      as center for circles.*)
    let x' = r' *. cos a1'
    and y' = r' *. sin a1' in
    Queue.add (sprintf "++(%.10f,%.10f)" (-. x') (-. y')) t.curr_path;
    for i = 1 to loops do
      Queue.add (sprintf " circle(%.10f)" r') t.curr_path;
    done;
    (*"Real" arc*)
    let endarc =
      if abs_float (a2' -. a1') >= 1E-9 then
        sprintf " arc(%.10f:%.10f:%.10f)"
          (180. *. a1' /. pi) (180. *. a2' /. pi) r'
      else ""
    in
    Queue.add (sprintf "++(%.10f,%.10f) %s" x' y' endarc) t.curr_path;
    st.curr_pt <- true;
    st.x <- st.x +. r' *. cos a2';
    st.y <- st.y +. r' *. sin a2'



  let close_path t =
    let st = get_state t in
    if st.curr_pt then Queue.add " -- cycle" t.curr_path

  let clear_path t =
    check_valid_handle t;
    Queue.clear t.curr_path;
    t.state.curr_pt <- false;
    t.state.path_extents <- { Matrix.x=0.; y=0.; w=0.; h=0. }

  let path_extents t = (get_state t).path_extents

  let stroke_preserve t =
    fprintf t.fh "\\draw[%s] " (make_options t);
    let path_data = Queue.create () in
    while not (Queue.is_empty t.curr_path) do
      let data = Queue.pop t.curr_path in
      output_string t.fh data;
      Queue.push data path_data;
    done;
    output_string t.fh ";\n";
    Queue.transfer path_data t.curr_path

  let stroke t =
    fprintf t.fh "\\draw[%s] " (make_options t);
    while not (Queue.is_empty t.curr_path) do
      output_string t.fh (Queue.pop t.curr_path)
    done;
    output_string t.fh ";\n";
    t.state.curr_pt <- false

  let fill_preserve t =
    fprintf t.fh "\\fill[%s] " (make_options t);
    let path_data = Queue.create() in
    while not (Queue.is_empty t.curr_path) do
      let data = Queue.pop t.curr_path in
      output_string t.fh data;
      Queue.push data path_data;
    done;
    output_string t.fh ";\n";
    Queue.transfer path_data t.curr_path

  let fill t =
    fprintf t.fh "\\fill[%s] " (make_options t);
    while not (Queue.is_empty t.curr_path) do
      output_string t.fh (Queue.pop t.curr_path)
    done;
    output_string t.fh ";\n";
    t.state.curr_pt <- false

  let clip_rectangle t ~x ~y ~w ~h =
    fprintf t.fh "\\clip (%.10f,%.10f) rectangle (%.10f,%.10f);\n"
      x y (x+.w) (y+.h)


  let translate t ~x ~y = Archimedes.Matrix.translate (get_state t).ctm x y

  let scale t ~x ~y = Archimedes.Matrix.scale (get_state t).ctm x y

  let rotate t ~angle = Archimedes.Matrix.rotate (get_state t).ctm ~angle

  let set_matrix t m =
    (*Sets a copy to avoid that modifications of matrix influence
      the backend's coordinate system.*)
    (get_state t).ctm <- Archimedes.Matrix.copy m

  let get_matrix t = Archimedes.Matrix.copy (get_state t).ctm

  let select_font_face t slant weight family =
    check_valid_handle t;
    let begin_slant, end_slant =
      match slant with
        Backend.Upright -> "",""
      | Backend.Italic -> "\textit{","}"
          (*| Archimedes.Oblique -> "\textsl{","}"(*Slanted*)*)
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
     (* | Archimedes.Oblique -> "sl" (*Slanted*)*)
    in
    fprintf t.fh "\\usefont{T1}{%s}{%s}{%s}\n" family series shape

  let set_font_size t size =
    let st = get_state t in
    st.fsize <- size;
    (* FIXME: this does not work INSIDE a tikz environment (TeXLive?) *)
   (* write t (sprintf "\\fontsize{%.2f}{%.2f}\\selectfont" size (1.2 *. size))*)
;;
(*Following
http://www.tac.dk/cgi-bin/info2www?%28latex%29Low-level%20font%20commands:
"`\fontsize{size}{skip}'
     Set font size. The first parameter is the font size to switch to;
     the second is the `\baselineskip' to use. The unit of both
     parameters defaults to pt. A rule of thumb is that the
     baselineskip should be 1.2 times the font size.
"
*)



(*FIXME: raw way to find the extents; must be reworked.*)
  let text_extents t txt =
    let size = (get_state t).fsize in
    let h = size *. 1.2 in
    let w =
      let rec add_lengths res i =
        if i < 0 then res
        else match txt.[i] with
        |'i'|'j'|'l' -> add_lengths (res +. 0.2) (i-1)
        |'m'|'w'|'A'..'Z'|'0'..'9' -> add_lengths (res +. 0.6) (i-1)
        | _ -> add_lengths (res +. 0.3) (i-1)
      in
      let len = add_lengths 0. ((String.length txt) - 1) in
      h *. len
    in
    { Matrix.x = 0.; y = -.size/.5.; w = w ; h = h }

  let show_text t ~rotate ~x ~y pos txt =
    let st = get_state t in
    (* Compute the angle between the desired direction and the X axis
       in the device coord. system. *)
    let dx, dy = Matrix.transform_distance st.ctm (cos rotate) (sin rotate) in
    let angle = atan2 dy dx in
    let x', y' = Matrix.transform_point st.ctm x y in
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
    (*let txt = sprintf (format_of_string st.slant_weight_family) txt in*)
    fprintf t.fh "\\node[inner sep=0pt, %s, %s, anchor=%s,rotate=%f] \
      at (%.10f,%.10f) {%s};"
      st.color st.opacity pos (180. *. angle /. pi) x' y' txt

  let flipy _ = false
end

let () =
  let module U = Backend.Register(B)  in ()
