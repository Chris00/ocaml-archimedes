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

open Archimedes
module M = Matrix

module B : Backend.Capabilities =
struct
  include Cairo

  let name = "cairo"

  type t = Cairo.context

  (* Same type (same internal representation), just in different modules *)
  let set_line_cap t c =
    set_line_cap t (match c with
                    | Backend.BUTT -> Cairo.BUTT
                    | Backend.ROUND -> Cairo.ROUND
                    | Backend.SQUARE -> Cairo.SQUARE)
  let get_line_cap t = (match get_line_cap t with
                        | Cairo.BUTT -> Backend.BUTT
                        | Cairo.ROUND -> Backend.ROUND
                        | Cairo.SQUARE -> Backend.SQUARE)
  let set_line_join t j =
    set_line_join t (match j with
                     | Backend.JOIN_MITER -> Cairo.JOIN_MITER
                     | Backend.JOIN_ROUND -> Cairo.JOIN_ROUND
                     | Backend.JOIN_BEVEL -> Cairo.JOIN_BEVEL)
  let get_line_join t = (match get_line_join t with
                         | Cairo.JOIN_MITER -> Backend.JOIN_MITER
                         | Cairo.JOIN_ROUND -> Backend.JOIN_ROUND
                         | Cairo.JOIN_BEVEL -> Backend.JOIN_BEVEL)

  let path_extents t =
    (* (Obj.magic(Cairo.Path.extents t): M.rectangle) *)
    let e = Cairo.Path.extents t in
    { M.x = e.Cairo.x;  y = e.Cairo.y;  w = e.Cairo.w;  h = e.Cairo.h }
  let close_path t = Cairo.Path.close t
  let clear_path t = Cairo.Path.clear t

  let set_dash t ofs arr = set_dash t ~ofs arr

  let set_matrix t m =
    (* let m' = (Obj.magic m : Cairo.matrix) in *)
    let m' = { Cairo.xx = m.M.xx; xy = m.M.xy;
               yx = m.M.yx; yy = m.M.yy;
               x0 = m.M.x0; y0 = m.M.y0 } in
    set_matrix t m'

  let get_matrix t =
    (* (Obj.magic (get_matrix cr) : Backend.matrix) *)
    let m = get_matrix t in
    { M.xx = m.Cairo.xx;  xy = m.Cairo.xy;
      yx = m.Cairo.yx; yy = m.Cairo.yy;
      x0 = m.Cairo.x0; y0 = m.Cairo.y0 }

  let flipy _ = true

  let set_color t c =
    let r,g,b,a = Color.get_rgba c in
    Cairo.set_source_rgba t r g b a

  let arc t ~r ~a1 ~a2 =
    let x,y = Cairo.Path.get_current_point t in
    let x = x -. r *. cos a1
    and y = y -. r *. sin a1 in
    arc t ~x ~y ~r ~a1 ~a2

  (* identity CTM -- never modified *)
  let id = { Cairo.xx = 1.; xy = 0.;  yx = 0.; yy = 1.;  x0 = 0.; y0 = 0. }

  let stroke t =
    (* FIXME: Do we really want this? are we not supposed to always
       draw in a nice coordinate system? *)
    let m = Cairo.get_matrix t in
    Cairo.set_matrix t id; (* to avoid the lines being deformed by [m] *)
    stroke t;
    Cairo.set_matrix t m

  let stroke_preserve t =
    Cairo.save t;
    Cairo.set_matrix t id;
    stroke_preserve t;
    Cairo.restore t

  let clip_rectangle t ~x ~y ~w ~h =
    Cairo.Path.clear t;
    Cairo.rectangle t ~x ~y ~w ~h;
    Cairo.clip t

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
    cr

  let close ~options t =
    let surface = Cairo.get_target t in
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
    Cairo.select_font_face t ~slant ~weight family

  let show_text t ~rotate ~x ~y pos text =
    (* Compute the angle between the desired direction and the X axis
       in the device coord. system. *)
    let cr = t in
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
    let te = Cairo.text_extents t text in
    (*An extents is always expressed in current coordinates; however,
      show_text switches to device coordinates before "making the
      text". So we need to go to user coordinates.*)
    (*Note: The following transformations assume that the coordinates
      are orthogonal.*)
    (*let x,y = Cairo.device_to_user_distance t te.x_bearing te.y_bearing in
    let w,h = Cairo.device_to_user_distance t te.width te.height in
    { M.x = x; y = -.y; w = w; h = -.h}*)
    { M.x = te.x_bearing; y = te.y_bearing;
      w = te.width; h = te.height}
end

let () =
  let module U = Backend.Register(B)  in ()



(* Local Variables: *)
(* compile-command: "make -k archimedes_cairo.cmo archimedes_cairo.cmxs" *)
(* End: *)
