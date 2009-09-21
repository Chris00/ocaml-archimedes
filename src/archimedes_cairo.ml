(* File: archimedes_cairo.ml

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

(** Cairo Archimedes plugin *)

module M = Archimedes.Matrix
module Backend = Archimedes.Backend

module B : Backend.Capabilities =
struct
  include Cairo

  let name = "cairo"

  (* identity CTM -- never modified *)
  let id = { Cairo.xx = 1.; xy = 0.;  yx = 0.; yy = 1.;  x0 = 0.; y0 = 0. }

  type t = { cr:Cairo.context; h:float}
      (* [h] is for height, to make the backend_to_device matrix.*)

  (* let path_extents cr = Cairo.Path.extents cr
     let close_path cr = Cairo.Path.close cr
     let clear_path cr = Cairo.Path.clear cr*)

  let backend_to_device t =
    { M.xx = 1.; xy = 0.; yx = 0.; yy = -1.; x0 = 0.; y0 = t.h }


  (* Same type (same internal representation), just in different modules *)
  let set_line_cap t c = set_line_cap t.cr (Obj.magic c : Cairo.line_cap)
  let get_line_cap t = (Obj.magic(get_line_cap t.cr) : Backend.line_cap)

  let set_line_join t j = set_line_join t.cr (Obj.magic j : Cairo.line_join)
  let get_line_join t = (Obj.magic(get_line_join t.cr) : Backend.line_join)

  (*Get needed functions which are in submodule Path*)
  let path_extents t = (Obj.magic(Cairo.Path.extents t.cr): M.rectangle)
  let close_path t = Cairo.Path.close t.cr
  let clear_path t = Cairo.Path.clear t.cr


  let set_line_width t = set_line_width t.cr

  let get_line_width t = get_line_width t.cr

  let set_dash t ofs arr = set_dash t.cr ~ofs arr
  let get_dash t = get_dash t.cr

  let set_matrix t m =
(*    Gc.compact ();*)
    let m' = { Cairo.xx = m.M.xx; xy = m.M.xy;
                    yx = m.M.yx; yy = m.M.yy;
                    x0 = m.M.x0; y0 = m.M.y0;}
      (*(Obj.magic m : Cairo.matrix)*)
    in
    set_matrix t.cr m'

  let get_matrix t =
    let m = get_matrix t.cr in
    { M.xx = m.Cairo.xx;  xy = m.Cairo.xy;
      yx = m.Cairo.yx; yy = m.Cairo.yy;
      x0 = m.Cairo.x0; y0 = m.Cairo.y0;}
    (*(Obj.magic (get_matrix cr) : Backend.matrix)*)
  let translate t = translate t.cr
  let scale t = scale t.cr
  let rotate t = rotate t.cr

  let set_color t c =
    let r,g,b,a = Archimedes.Color.get_rgba c in
    Cairo.set_source_rgba t.cr r g b a

  let move_to t = move_to t.cr
  let line_to t = line_to t.cr
  let rel_move_to t = rel_move_to t.cr
  let rel_line_to t = rel_line_to t.cr
  let curve_to t = curve_to t.cr
  let rectangle t = rectangle t.cr

  let arc t ~r ~a1 ~a2 =
    let x,y = Cairo.Path.get_current_point t.cr in
    let x = x -. r *. cos a1
    and y = y -. r *. sin a1 in
    arc t.cr ~x ~y ~r ~a1 ~a2

  let stroke t =
    (* FIXME: Do we really want this? are we not supposed to always
       draw in a nice coordinate system? *)
    let m = Cairo.get_matrix t.cr in
    Cairo.set_matrix t.cr id; (* to avoid the lines being deformed by [m] *)
    stroke t.cr;
    Cairo.set_matrix t.cr m

  let stroke_preserve t =
    Cairo.save t.cr;
    Cairo.set_matrix t.cr id;
    stroke_preserve t.cr;
    Cairo.restore t.cr

  let fill t = fill t.cr
  let fill_preserve t = fill_preserve t.cr

  let clip_rectangle t ~x ~y ~w ~h =
    Cairo.Path.clear t.cr;
    Cairo.rectangle t.cr ~x ~y ~w ~h;
    Cairo.clip t.cr

  let save t = save t.cr
  let restore t = restore t.cr

(*
  (* FIXME: must be reworked *)
  let text cr ~size ~x ~y txt =
    Cairo.move_to cr ~x ~y;
    Cairo.set_font_size cr size;
    Cairo.show_text cr txt
*)

  (* FIXME: better error message for options *)
  let make ~options width height =
    let surface = match options with
      | ["PDF"; fname] -> PDF.create fname width height
      | ["PS"; fname] -> PS.create fname width height
      | ["PNG"; _] -> (* We need to modify the close function *)
          Image.create Image.ARGB32 (truncate width) (truncate height)
      | [] -> (* interactive display. FIXME: when ready *)
          Image.create Image.ARGB32 (truncate width) (truncate height)
      | _ ->
          let opt = String.concat "; " options in
          failwith("Archimedes_cairo.make: options [" ^ opt
                   ^ "] not understood") in
    let cr = Cairo.create surface in
    (* Round line caps are the only option currently offered by
       graphics.  Be coherent with that. *)
    Cairo.set_line_cap cr Cairo.ROUND;
    {cr = cr; h=height}

  let close ~options t =
    let surface = Cairo.get_target t.cr in
    (match options with
     | ["PNG"; fname] -> PNG.write surface fname;
     | _ -> ());
    Surface.finish surface


  let select_font_face t slant weight family =
    (* Could be (unsafely) optimized *)
    let slant = match slant with
      | Backend.Upright -> Cairo.Upright
      | Backend.Italic -> Cairo.Italic
    and weight = match weight with
      | Backend.Normal -> Cairo.Normal
      | Backend.Bold -> Cairo.Bold in
    Cairo.select_font_face t.cr ~slant ~weight family

  let set_font_size t = set_font_size t.cr

  let show_text t ~rotate ~x ~y pos text =
    (* Compute the angle between the desired direction and the X axis
       in the device coord. system. *)
    let cr = t.cr in
    let dx, dy = user_to_device_distance cr (cos rotate) (sin rotate) in
    let angle = atan2 dy dx in
    Cairo.save cr;
    Cairo.move_to cr x y;
    Cairo.set_matrix cr id;
    Cairo.rotate cr angle;
    let te = Cairo.text_extents cr text in
    let x0 = match pos with
      | Backend.CC | Backend.CT | Backend.CB ->
          te.x_bearing +. 0.5 *. te.width
      | Backend.RC | Backend.RT | Backend.RB ->
          te.x_bearing
      | Backend.LC | Backend.LT | Backend.LB ->
          te.x_bearing +. te.width
    and y0 = match pos with
      | Backend.CC | Backend.RC | Backend.LC ->
          te.y_bearing +. 0.5 *. te.height
      | Backend.CT | Backend.RT | Backend.LT ->
          te.y_bearing +. te.height
      | Backend.CB | Backend.RB | Backend.LB ->
          te.y_bearing
    in
    Cairo.rel_move_to cr (-. x0) (-. y0);
    Cairo.show_text cr text;
    Cairo.stroke cr; (* without this, the current position is the end
                        of the text which is not desired. *)
    Cairo.restore cr

  let text_extents t text =
    let te = Cairo.text_extents t.cr text in
    (*An extents is always expressed in current coordinates; however,
      show_text switches to device coordinates before "making the
      text". So we need to go to user coordinates.*)
    (*Note: The following transformations assume that the coordinates
      are orthogonal.*)
    (*let x,y = Cairo.device_to_user_distance t.cr te.x_bearing te.y_bearing in
    let w,h = Cairo.device_to_user_distance t.cr te.width te.height in
    { M.x = x; y = -.y; w = w; h = -.h}*)
    { M.x = te.x_bearing; y = te.y_bearing;
      w = te.width; h = te.height}
end

let () =
  let module U = Backend.Register(B)  in ()



(* Local Variables: *)
(* compile-command: "make -k archimedes_cairo.cmo archimedes_cairo.cmxs" *)
(* End: *)
