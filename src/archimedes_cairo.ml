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

module A = Archimedes
open Bigarray
module M = A.Matrix

module B : A.Backend.Capabilities =
struct
  include Cairo

  let name = "cairo"

  type t = Cairo.context

  (* Same type (same internal representation), just in different modules *)
  let set_line_cap t c =
    set_line_cap t (match c with
                    | A.Backend.BUTT -> Cairo.BUTT
                    | A.Backend.ROUND -> Cairo.ROUND
                    | A.Backend.SQUARE -> Cairo.SQUARE)
  let get_line_cap t = (match get_line_cap t with
                        | Cairo.BUTT -> A.Backend.BUTT
                        | Cairo.ROUND -> A.Backend.ROUND
                        | Cairo.SQUARE -> A.Backend.SQUARE)
  let set_line_join t j =
    set_line_join t (match j with
                     | A.Backend.JOIN_MITER -> Cairo.JOIN_MITER
                     | A.Backend.JOIN_ROUND -> Cairo.JOIN_ROUND
                     | A.Backend.JOIN_BEVEL -> Cairo.JOIN_BEVEL)
  let get_line_join t = (match get_line_join t with
                         | Cairo.JOIN_MITER -> A.Backend.JOIN_MITER
                         | Cairo.JOIN_ROUND -> A.Backend.JOIN_ROUND
                         | Cairo.JOIN_BEVEL -> A.Backend.JOIN_BEVEL)

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
    let r,g,b,a = A.Color.get_rgba c in
    Cairo.set_source_rgba t r g b a

  let arc t ~r ~a1 ~a2 =
    let x,y = Cairo.Path.get_current_point t in
    let x = x -. r *. cos a1
    and y = y -. r *. sin a1 in
    arc t x y ~r ~a1 ~a2

  (* identity CTM -- never modified *)
  let id = { Cairo.xx = 1.; xy = 0.;  yx = 0.; yy = 1.;  x0 = 0.; y0 = 0. }

  let show t =
    Cairo.Surface.flush (get_target t)

  let clip_rectangle t x y ~w ~h =
    Cairo.Path.clear t;
    Cairo.rectangle t x y ~w ~h;
    Cairo.clip t

  (* FIXME: better error message for options *)
  let make ~options width height =
    let surface = match options with
      | ["PDF"; fname] -> PDF.create fname width height
      | ["PS"; fname] -> PS.create fname width height
      | ["SVG"; fname] -> SVG.create fname ~w:width ~h:height
      | ["PNG"; _] -> (* saving done by the close function *)
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


  let stroke cr =
    (* FIXME: Do we really want this? are we not supposed to always
       draw in a nice coordinate system? *)
    let m = Cairo.get_matrix cr in
    Cairo.set_matrix cr id; (* to avoid the lines being deformed by [m] *)
    Cairo.stroke cr;
    Cairo.set_matrix cr m

  let stroke_preserve cr =
    let m = Cairo.get_matrix cr in
    Cairo.set_matrix cr id; (* to avoid the lines being deformed by [m] *)
    Cairo.stroke_preserve cr;
    Cairo.set_matrix cr m

  module P = Archimedes_internals.Path

  let path_to_cairo cr = function
    | P.Move_to(x, y) -> Cairo.move_to cr x y
    | P.Line_to(x, y) ->
      (* FIXME: Maybe it souldn't be to the backend to handle that. *)
      if x = x && y = y then
        Cairo.line_to cr x y
    | P.Curve_to(_, _, x1, y1, x2, y2, x3, y3) ->
      Cairo.curve_to cr x1 y1 x2 y2 x3 y3
    | P.Close(_, _) -> Cairo.Path.close cr
    | P.Array(x, y, i0, i1) ->
        if i0 <= i1 then
          for i = i0 to i1 do
            if x.(i) = x.(i) && y.(i) = y.(i) then Cairo.line_to cr x.(i) y.(i)
          done
        else
          for i = i0 downto i1 do
            if x.(i) = x.(i) && y.(i) = y.(i) then Cairo.line_to cr x.(i) y.(i)
          done
    | P.Fortran(x, y, i0, i1) ->
      if i0 <= i1 then
        for i = i0 to i1 do
          if x.{i} = x.{i} && y.{i} = y.{i} then Cairo.line_to cr x.{i} y.{i}
        done
      else
        for i = i0 downto i1 do
          if x.{i} = x.{i} && y.{i} = y.{i} then Cairo.line_to cr x.{i} y.{i}
        done
    | P.C(x, y, i0, i1) ->
      if i0 <= i1 then
        for i = i0 to i1 do
          if x.{i} = x.{i} && y.{i} = y.{i} then Cairo.line_to cr x.{i} y.{i}
        done
      else
        for i = i0 downto i1 do
          if x.{i} = x.{i} && y.{i} = y.{i} then Cairo.line_to cr x.{i} y.{i}
        done

  (* The clipping is taken care of by the cairo backend. *)
  let stroke_path_preserve cr p =
    Cairo.Path.clear cr;
    P.iter p (path_to_cairo cr);
    (* Line width is in defaukt coordinates: *)
    let m = Cairo.get_matrix cr in
    Cairo.set_matrix cr id;
    Cairo.stroke cr; (* no need to preserve the copy of the path *)
    Cairo.set_matrix cr m

  let fill_path_preserve cr p =
    Cairo.Path.clear cr;
    P.iter p (path_to_cairo cr);
    Cairo.fill cr

  let fill_with_color cr c =
    let source = Cairo.get_source cr in
    set_color cr c;
    let op = Cairo.get_operator cr in
    Cairo.set_operator cr Cairo.SOURCE;
    Cairo.fill cr;
    Cairo.set_operator cr op;
    Cairo.set_source cr source

  let select_font_face t slant weight family =
    (* Could be (unsafely) optimized *)
    let slant = match slant with
      | A.Backend.Upright -> Cairo.Upright
      | A.Backend.Italic -> Cairo.Italic
    and weight = match weight with
      | A.Backend.Normal -> Cairo.Normal
      | A.Backend.Bold -> Cairo.Bold in
    Cairo.select_font_face t ~slant ~weight family

  (* TODO: add an option for alignment to baseline (using font_extents)
     instead of text extents. *)
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
      | A.Backend.CC | A.Backend.CT | A.Backend.CB ->
          te.x_bearing +. 0.5 *. te.width
      | A.Backend.RC | A.Backend.RT | A.Backend.RB ->
          te.x_bearing
      | A.Backend.LC | A.Backend.LT | A.Backend.LB ->
          te.x_bearing +. te.width
    and y0 = match pos with
      | A.Backend.CC | A.Backend.RC | A.Backend.LC ->
          te.y_bearing +. 0.5 *. te.height
      | A.Backend.CT | A.Backend.RT | A.Backend.LT ->
          te.y_bearing +. te.height
      | A.Backend.CB | A.Backend.RB | A.Backend.LB ->
          te.y_bearing
    in
    Cairo.rel_move_to cr (-. x0) (-. y0);
    Cairo.show_text cr text;
    Cairo.stroke cr; (* without this, the current position is the end
                        of the text which is not desired. *)
    Cairo.restore cr

  let text_extents t text =
    Cairo.save t;
    Cairo.set_matrix t id;
    let te = Cairo.text_extents t text in
    Cairo.restore t;
    (*An extents is always expressed in current coordinates; however,
      show_text switches to device coordinates before "making the
      text". So we need to go to user coordinates.*)
    (*Note: The following transformations assume that the coordinates
      are orthogonal.*)
    let x,y = Cairo.device_to_user_distance t te.x_bearing te.y_bearing in
    let w,h = Cairo.device_to_user_distance t te.width te.height in
    (* FIXME: y text extents in Cairo are given with the bearing from
       origin to *topmost* part of the glyphs. However, in normalized
       coordinates (e.g: for Viewport), we want them to be expressed from
       the downmost part. *)
    { M.x = x; y = -. y; w = w; h = h }
end

let () =
  let module U = A.Backend.Register(B)  in ()



(* Local Variables: *)
(* compile-command: "make -C .. -k" *)
(* End: *)
