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

module V = Viewport

(* FIXME: offset might benefit from better variant names such as Data |
   Graph. And, it doesn't support other coord_names. Maybe we should use
   polymorphic variant types shared between Axes and Viewport instead of
   redefining local ones as Axes.Data and Axes.Graph. *)
type offset =
  | Relative of float
  | Absolute of float

(* returns the offset and wether the labels are over or under the axis *)
let axis_offset start range = function
  | Absolute y -> start +. range *. y, if y > 0.5 then 1. else -1.
  | Relative y -> y, if y > start +. range /. 2. then 1. else -1.

let tic vp x y (tic_type, tic_size) =
  V.set_rel_mark_size_direct vp tic_size ();
  V.mark_direct vp x y tic_type ()

let draw_tic tic majors minors text = function
  | Tics.Major (None, v) -> tic v majors
  | Tics.Major (Some label, v) -> tic v majors;
      text v label
  | Tics.Minor v -> tic v minors

let arrow_offset xrange arrow =
  if arrow = Arrows.Unstyled then 0.
  else xrange *. 0.03 (* FIXME: we should use arrow extends. *)

let grid_style vp =
  let color = V.get_color vp in
  let r, g, b = Color.get_rgb color in
  let r' = 1. -. (1. -. r) /. 4.
  and g' = 1. -. (1. -. g) /. 4.
  and b' = 1. -. (1. -. b) /. 4. in
  let color' = Color.rgb r' g' b' in
  V.set_color_direct vp color' ();
  (* Return a restore function *)
  let restore vp =
    V.set_color_direct vp color ()
  in
  restore

let draw_x_axis grid major minor start stop tics offset vp () =
  let xrange = V.xmax vp -. V.xmin vp
  and yrange = V.ymax vp -. V.ymin vp in
  let x1 = V.xmin vp -. arrow_offset xrange start
  and x2 = V.xmax vp +. arrow_offset xrange stop in
  let tics_values = Tics.tics (V.xlog vp) x1 x2 tics in
  let offset, pos = axis_offset (V.ymin vp) yrange offset in
  Arrows.line_direct ~head:stop ~tail:start vp x1 offset x2 offset ();
  let tic x = tic vp x offset in
  let text x lbl =
    let x, y = V.ortho_from vp V.Data (x, offset) in
    let y = y +. 0.0375 *. pos in
    let align = if pos < 0. then Backend.CB else Backend.CT in
    V.show_text_direct vp V.Orthonormal ~x ~y align lbl () in
  let grid_line = function
    | Tics.Major (_, x) -> let path = Path.make() in
                          Path.move_to path x (V.ymin vp);
                          Path.line_to path x (V.ymax vp);
                          V.stroke_direct vp ~path V.Data ()
    | Tics.Minor _ -> ()
  in
  if grid then begin
    let restore = grid_style vp in
    List.iter grid_line tics_values;
    restore vp
  end;
  List.iter (draw_tic tic major minor text) tics_values

let draw_y_axis grid major minor start stop tics offset vp () =
  let xrange = V.xmax vp -. V.xmin vp
  and yrange = V.ymax vp -. V.ymin vp in
  let y1 = V.ymin vp -. arrow_offset yrange start
  and y2 = V.ymax vp +. arrow_offset yrange stop in
  let tics_values = Tics.tics (V.ylog vp) (V.ymin vp) (V.ymax vp) tics in
  let offset, pos = axis_offset (V.xmin vp) xrange offset in
  Arrows.line_direct ~head:stop ~tail:start vp offset y1 offset y2 ();
  let tic y = tic vp offset y in
  let text y lbl =
    let x, y = V.ortho_from vp V.Data (offset, y) in
    let x = x +. 0.0375 *. pos in
    let align = if pos < 0. then Backend.LC else Backend.RC in
    V.show_text_direct vp V.Orthonormal ~x ~y align lbl () in
  let grid_line = function
    | Tics.Major (_, y) -> let path = Path.make() in
                          Path.move_to path (V.xmin vp) y;
                          Path.line_to path (V.xmax vp) y;
                          V.stroke_direct vp ~path V.Data ()
    | Tics.Minor _ -> ()
  in
  if grid then begin
    let restore = grid_style vp in
    List.iter grid_line tics_values;
    restore vp
  end;
  List.iter (draw_tic tic major minor text) tics_values

let add_x_axis ?(grid=false)
    ?(major=("tic_up", 3.)) ?(minor=("tic_up", 1.))
    ?(start=Arrows.Unstyled) ?(stop=Arrows.Simple)
    ?(tics=Tics.Auto (Tics.Number 5)) ?(offset=Absolute 0.) vp =
  V.save vp;
  V.add_instruction
    (draw_x_axis grid major minor start stop tics offset vp) vp;
  V.add_instruction (fun () -> Backend.show(V.get_backend vp)) vp;
  V.restore vp

let add_y_axis ?(grid=false)
    ?(major=("tic_right", 3.)) ?(minor=("tic_right", 1.))
    ?(start=Arrows.Unstyled) ?(stop=Arrows.Simple)
    ?(tics=Tics.Auto (Tics.Number 5)) ?(offset=Absolute 0.) vp =
  V.save vp;
  V.add_instruction
    (draw_y_axis grid major minor start stop tics offset vp) vp;
  V.add_instruction (fun () -> Backend.show(V.get_backend vp)) vp;
  V.restore vp

let box ?(grid=true) ?tics ?tics_alt vp =
  let tics_alt = match tics_alt with
  | None -> tics
  | Some x -> Some x
  in
  add_x_axis ~grid ~start:Arrows.Unstyled ~stop:Arrows.Unstyled
    ?tics ~offset:(Absolute 0.) vp;
  add_x_axis ~start:Arrows.Unstyled ~stop:Arrows.Unstyled ?tics:tics_alt
    ~offset:(Absolute 1.) ~major:("tic_down", 3.) ~minor:("tic_down", 1.) vp;
  add_y_axis ~grid ~start:Arrows.Unstyled ~stop:Arrows.Unstyled
    ?tics ~offset:(Absolute 0.) vp;
  add_y_axis ~start:Arrows.Unstyled ~stop:Arrows.Unstyled ?tics:tics_alt
    ~offset:(Absolute 1.) ~major:("tic_left", 3.) ~minor:("tic_left", 1.) vp

let cross ?tics vp =
  add_x_axis ?tics ~offset:(Relative 0.) ~major:("|", 2.)
    ~minor:("|", 1.) vp;
  add_y_axis ?tics ~offset:(Relative 0.) ~major:("-", 2.)
    ~minor:("-", 1.) vp
