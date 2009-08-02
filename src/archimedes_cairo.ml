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

open Cairo
open Archimedes

module B : Backend.Capabilities =
struct
  include Cairo
  let name = "cairo"

  type t = Cairo.context
  let path_extents = Cairo.Path.extents
  let close_path = Cairo.Path.close
  let clear_path = Cairo.Path.clear

  (* Same type (same internal representation), just in different modules *)
  let set_line_cap cr c = set_line_cap cr (Obj.magic c : Cairo.line_cap)
  let get_line_cap cr = (Obj.magic(get_line_cap cr) : Backend.line_cap)

  let set_line_join cr j = set_line_join cr (Obj.magic j : Cairo.line_join)
  let get_line_join cr = (Obj.magic(get_line_join cr) : Backend.line_join)

  let path_extents cr = (Obj.magic (path_extents cr) : Backend.rectangle)

  let set_matrix cr m = set_matrix cr (Obj.magic m : Cairo.matrix)
  let get_matrix cr = (Obj.magic (get_matrix cr) : Backend.matrix)


  let set_dash cr ofs arr = set_dash cr ~ofs arr

  let set_color cr c =
    Cairo.set_source_rgba cr
      (Color.red c) (Color.green c) (Color.blue c) (Color.alpha c)

  let clip_rectangle cr ~x ~y ~w ~h =
    Cairo.Path.clear cr;
    Cairo.rectangle cr ~x ~y ~w ~h;
    Cairo.clip cr

  (* FIXME: must be reworked *)
  let text cr ~size ~x ~y txt =
    Cairo.move_to cr ~x ~y;
    Cairo.set_font_size cr size;
    Cairo.show_text cr txt


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
    Cairo.create surface

  let close ~options cr =
    let surface = Cairo.get_target cr in
    (match options with
     | ["PNG"; fname] -> PNG.write surface fname;
     | _ -> ());
    Surface.finish surface


  let select_font_face cr slant weight family =
    (* Could be (unsafely) optimized *)
    let slant = match slant with
      | Backend.Upright -> Cairo.Upright
      | Backend.Italic -> Cairo.Italic
    and weight = match weight with
      | Backend.Normal -> Cairo.Normal
      | Backend.Bold -> Cairo.Bold in
    Cairo.select_font_face cr ~slant ~weight family

  (* identity CTM -- never modified *)
  let id = { Cairo.xx = 1.; xy = 0.;  yx = 0.; yy = 1.;  x0 = 0.; y0 = 0. }

  let show_text cr ~rotate ~x ~y pos text =
    (* Compute the angle between the desired direction and the X axis
       in the device coord. system. *)
    let dx, dy = user_to_device_distance cr (cos rotate) (sin rotate) in
    let angle = atan2 dy dx in
    Cairo.save cr;
    Cairo.move_to cr x y;
    Cairo.set_matrix cr id;
    Cairo.rotate cr angle;
    let te = Cairo.text_extents cr text in
    let x0 = match pos with
      | Backend.CC | Backend.CT | Backend.CB -> te.x_bearing +. 0.5 *. te.width
      | Backend.RC | Backend.RT | Backend.RB -> te.x_bearing
      | Backend.LC | Backend.LT | Backend.LB -> te.x_bearing +. te.width
    and y0 = match pos with
      | Backend.CC | Backend.RC | Backend.LC -> te.y_bearing +. 0.5 *. te.height
      | Backend.CT | Backend.RT | Backend.LT -> te.y_bearing +. te.height
      | Backend.CB | Backend.RB | Backend.LB -> te.y_bearing  in
    Cairo.rel_move_to cr (-. x0) (-. y0);
    Cairo.show_text cr text;
    Cairo.restore cr
end

let () =
  let module U = Backend.Register(B)  in ()



(* Local Variables: *)
(* compile-command: "make -k archimedes_cairo.cmo" *)
(* End: *)
