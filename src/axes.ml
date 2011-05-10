(* File: viewport.ml

   Copyright (C) 2009-2015

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

module B = Backend
module V = Viewport.Viewport
module A = Viewport.Axes (* Corresponding to "dimensions" of a viewport *)

(* FIXME: offset might benefit from better variant names such as Data |
   Graph. And, it doesn't support other coord_names. Maybe we should use
   polymorphic variant types shared between Axes and Viewport instead of
   redefining local ones as Axes.Data and Axes.Graph. *)
type offset =
  | Relative of float
  | Absolute of float

type graph_axis = {
  tics: Tics.t;
  offset: offset;
  major_tics: string * float;
  minor_tics: string * float;
  mutable tics_values: Tics.tic list
}

(* returns the offset and wether the labels are over or under the axis *)
let axis_offset start range = function
  | Absolute y -> start +. range *. y, if y < 0.5 then -1. else 1.
  | Relative y -> y, if y < start +. range /. 2. then -1. else 1.

let tic vp x y (tic_type, tic_size) =
  V.set_rel_mark_size_direct vp tic_size ();
  V.mark_direct vp x y tic_type ()

let draw_tic tic majors minors text = function
  | Tics.Major (None, v) -> tic v majors
  | Tics.Major (Some label, v) -> tic v majors;
      text v label
  | Tics.Minor v -> tic v minors

let draw_x_axis major minor tics offset vp () =
  let tics_values = Tics.tics (V.xmin vp) (V.xmax vp) tics in
  let yrange = V.ymax vp -. V.ymin vp in
  let offset, pos = axis_offset (V.ymin vp) yrange offset in
  let path = Path.make_at (V.xmin vp) offset in
  Path.line_to path (V.xmax vp) offset;
  V.stroke_direct ~path vp V.Data ();
  let y = offset +. yrange *. 0.0375 *. pos in
  let tic x = tic vp x offset in
  let text x lbl = V.show_text_direct vp V.Data ~x ~y B.CC lbl () in
  List.iter (draw_tic tic major minor text) tics_values

let draw_y_axis major minor tics offset vp () =
  let tics_values = Tics.tics (V.ymin vp) (V.ymax vp) tics in
  let xrange = V.xmax vp -. V.xmin vp in
  let offset, pos = axis_offset (V.xmin vp) xrange offset in
  let path = Path.make_at offset (V.ymin vp) in
  Path.line_to path offset (V.ymax vp);
  V.stroke_direct ~path vp V.Data ();
  let x = offset +. xrange *. 0.0375 *. pos in
  let tic y = tic vp offset y in
  let text y lbl = V.show_text_direct vp V.Data ~x ~y B.CC lbl () in
  List.iter (draw_tic tic major minor text) tics_values

let add_x_axis ?(major=("tic_up", 5.)) ?(minor=("tic_up", 2.))
    ?(tics=Tics.Auto (Tics.Number 5)) ?(offset=Absolute 0.) vp =
  V.add_instruction (draw_x_axis major minor tics offset vp) vp

let add_y_axis ?(major=("tic_up", 5.)) ?(minor=("tic_up", 2.))
    ?(tics=Tics.Auto (Tics.Number 5)) ?(offset=Absolute 0.) vp =
  V.add_instruction (draw_y_axis major minor tics offset vp) vp

let box vp =
  add_x_axis ~offset:(Absolute 0.) vp;
  add_x_axis ~offset:(Absolute 1.) ~major:("tic_down", 5.)
    ~minor:("tic_down", 2.) vp;
  add_y_axis ~offset:(Absolute 0.) vp;
  add_y_axis ~offset:(Absolute 1.) ~major:("tic_left", 5.)
    ~minor:("tic_left", 2.) vp

let cross vp =
  add_x_axis ~offset:(Absolute 0.5) ~major:("|", 2.)
    ~minor:("|", 1.) vp;
  add_y_axis ~offset:(Absolute 0.5) ~major:("-", 2.)
    ~minor:("-", 1.) vp
