(* File: arrows.ml

   Copyright (C) 2009-2011

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     Pierre Hauweele <antegallya@gmail.com>
     Noemie Meunier <noemie_6462@hotmail.com>
     Fabian Pijcke <fabian.pijcke@gmail.com>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

module V = Viewport.Viewport

type style =
  | Unstyled
  | Simple
  | Double
  | Triple
  | Diamond
  | Circle
  | Custom of (Path.t -> unit)

let simple rel_move_to rel_line_to =
  rel_line_to (-1.) 0.5;
  rel_move_to 1. (-0.5);
  rel_line_to (-1.) (-0.5);
  rel_move_to 1. 0.5

let add_to_path path size alpha style =
  let rel_move_to x y =
    Path.rel_move_to ~rot:(-. alpha) path ~x:(x *. size) ~y:(y *. size) in
  let rel_line_to x y =
    Path.rel_line_to ~rot:(-. alpha) path ~x:(x *. size) ~y:(y *. size) in
  let simple () = simple rel_move_to rel_line_to in
  match style with
  | Unstyled -> ()
  | Simple -> simple ()
  | Double ->
      simple ();
      rel_move_to (-0.2) 0.;
      simple ();
      rel_move_to 0.2 0.;
  | Triple ->
      simple ();
      rel_move_to (-0.2) 0.;
      simple ();
      rel_move_to (-0.2) 0.;
      simple ();
      rel_move_to 0.4 0.
  | Diamond ->
      rel_line_to (-0.5) (-0.3);
      rel_line_to (-0.5) 0.3;
      rel_line_to 0.5 0.3;
      rel_line_to 0.5 (-0.3);
  | Circle -> (* TODO *) ()
  | (Custom f) -> f path

let path_line_to ?(size=0.01) ?(head=Simple) ?(tail=Unstyled) path x y =
  let x0, y0 = Path.current_point path in
  let alpha = atan2 (y -. y0) (x -. x0) in
  add_to_path path size alpha tail;
  Path.line_to path x y;
  add_to_path path size alpha head

let path_arc ?(size=0.01) ?(head=Simple) ?(tail=Unstyled) path r a1 a2 =
  let alpha = 0. in
  add_to_path path size alpha tail;
  Path.arc path r a1 a2;
  add_to_path path size alpha head

let line_direct ?(size=0.01) ?(head=Simple) ?(tail=Unstyled) vp x0 y0 x y () =
  let x0', y0' = V.ortho_from vp V.Data (x0, y0) in
  let x', y' = V.ortho_from vp V.Data (x, y) in
  let alpha = atan2 (y' -. y0') (x' -. x0') in
  (* line *)
  let path_line = Path.make_at x0 y0 in
  Path.line_to path_line x y;
  V.stroke_direct ~path:path_line vp V.Data ();
  (* head *)
  let path_head = Path.make_at x' y' in
  add_to_path path_head size alpha head;
  V.stroke_direct ~path:path_head vp V.Orthonormal ();
  (* tail *)
  let path_tail = Path.make_at x0' y0' in
  add_to_path path_tail size alpha tail;
  V.stroke_direct ~path:path_tail vp V.Orthonormal ()

let line ?(size=0.01) ?(head=Simple) ?(tail=Unstyled) vp x0 y0 x y =
  V.auto_fit vp x0 y0 x y;
  V.add_instruction (line_direct ~size ~head ~tail vp x0 y0 x y) vp

let arc ?(size=1.) ?(head=Simple) ?(tail=Unstyled) vp x0 y0 r a1 a2 =
  let x0', y0' = V.ortho_from vp V.Data (x0, y0) in
  let alpha0 = 0. in (* TODO *)
  let path_arc = Path.make_at x0 y0 in
  Path.arc path_arc r a1 a2;
  V.stroke ~path:path_arc vp V.Data;
  (* TODO *)
  ()
