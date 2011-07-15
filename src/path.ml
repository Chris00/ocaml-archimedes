(* File: path.ml

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

let fourth_pi = atan 1.
let two_pi = fourth_pi /. 2.

type data =
  | Move_to of float * float
  | Line_to of float * float
  | Rectangle of float * float * float * float
      (* RECTANGLE(x, y, width, height) *)
  | Curve_to of float * float * float * float * float * float * float * float
  | Close of float * float

type t = {
  mutable path: data list;
  mutable extents: Matrix.rectangle;
  mutable x: float;
  mutable y: float;
  mutable curr_pt: bool;
}

let make () =
  { path = [];
    extents = { Matrix.x = nan; y = nan; w = nan; h = nan };
    x = 0.;
    y = 0.;
    curr_pt = false }

let copy p =
  { path = List.map (fun x -> x) p.path;
    extents = { p.extents with Matrix.x = p.extents.Matrix.x };
    x = p.x;
    y = p.y;
    curr_pt = p.curr_pt }

let clear p =
  p.path <- [];
  p.extents <- { Matrix.x = nan; y = nan; w = nan; h = nan };
  p.x <- 0.;
  p.y <- 0.;
  p.curr_pt <- false

let extents p =
  { p.extents with Matrix.x = p.extents.Matrix.x }

let beginning_of_subpath p =
  let rec aux = function
    | [] -> failwith "Archimedes: No subpath"
    | Move_to (x, y) :: _ -> (x, y)
    | Close (x, y) :: _ -> (x, y)
    | Rectangle (x, y, _, _) :: _ -> (x, y)
    | (Curve_to _ | Line_to _) :: tl -> aux tl
  in
  aux p.path

let update_rectangle p x0 y0 x1 y1 =
  let e = p.extents in
  let x = min e.Matrix.x (min x0 x1)
  and y = min e.Matrix.y (min y0 y1)
  and x' = max (e.Matrix.x +. e.Matrix.w) (max x0 x1)
  and y' = max (e.Matrix.y +. e.Matrix.h) (max y0 y1) in
  p.extents <- { Matrix.x = x; y = y; w = x' -. x; h = y' -. y }

let move_to p ~x ~y =
  p.path <- Move_to (x, y) :: p.path;
  p.x <- x;
  p.y <- y;
  p.curr_pt <- true

let make_at x y =
  let p = make () in
  move_to p x y;
  p

let line_to p ~x ~y =
  if p.curr_pt then begin
    p.path <- Line_to (x, y) :: p.path;
    update_rectangle p p.x p.y x y;
    p.x <- x;
    p.y <- y
  end else move_to p ~x ~y

let rotate alpha x y =
  x *. cos alpha +. y *. sin alpha, y *. cos alpha -. x *. sin alpha

let rel_move_to ?(rot=0.) p ~x ~y =
  let x', y' = rotate rot x y in
  move_to p ~x:(p.x +. x') ~y:(p.y +. y')

let rel_line_to ?(rot=0.) p ~x ~y =
  let x', y' = rotate rot x y in
  line_to p ~x:(p.x +. x') ~y:(p.y +. y')

let rectangle p ~x ~y ~w ~h =
  p.path <- Rectangle (x, y, w, h) :: p.path;
  update_rectangle p x y (x +. w) (y +. h);
  p.x <- x;
  p.y <- y;
  p.curr_pt <- true

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

(** Return the smaller rectangle containing [r] and the Bezier curve
    given by the control points. *)
let update_curve e x0 y0 x1 y1 x2 y2 x3 y3 =
  let xmin, xmax = range_bezier x0 x1 x2 x3 in
  let xmin = min xmin e.Matrix.x in
  let w = max xmax (e.Matrix.x +. e.Matrix.w) -. xmin in
  let ymin, ymax = range_bezier y0 y1 y2 y3 in
  let ymin = min ymin e.Matrix.y in
  let h = max ymax (e.Matrix.y +. e.Matrix.h) -. ymin in
  { Matrix.x = xmin;  y = ymin; w = w; h = h }

let internal_curve_to p ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  let x0, y0 =
    if p.curr_pt then p.x, p.y
    else begin
      p.path <- Move_to(x1, y1) :: p.path;
      p.curr_pt <- true;
      (x1, y1)
    end
  in
  p.path <-
    Curve_to(x0, y0, x1, y1, x2, y2, x3, y3) :: p.path;
  (* Update the current point and extents *)
  p.extents <-
    update_curve p.extents x0 y0 x1 y1 x2 y2 x3 y3;
  p.x <- x3;
  p.y <- y3

let curve_to p ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  internal_curve_to p ~x1 ~y1 ~x2 ~y2 ~x3 ~y3

(* Constant to determine the control points so that the bezier curve
   passes by middle point of the arc. *)
let arc_control = 4. /. 3. (* (1 - cos(b))/(sin b),  b = (a1 - a2)/2 *)

let rec bezier_arc p x0 y0 r a1 a2 =
  let da = 0.5 *. (a2 -. a1) in
  if abs_float(da) <= fourth_pi then
    let k = arc_control *. (1. -. cos da) /. sin da in
    let rcos_a1 = r *. cos a1 and rsin_a1 = r *. sin a1 in
    let rcos_a2 = r *. cos a2 and rsin_a2 = r *. sin a2 in
    let x3 = x0 -. rcos_a1 +. rcos_a2
    and y3 = y0 -. rsin_a1 +. rsin_a2 in
    let x1 = x0 -. k *. rsin_a1
    and y1 = y0 +. k *. rcos_a1 in
    let x2 = x3 +. k *. rsin_a2
    and y2 = y3 -. k *. rcos_a2 in
    internal_curve_to p x1 y1 x2 y2 x3 y3;
    x3, y3
  else (* several Bezier curves are needed. *)
    let mid = 0.5 *. (a1 +. a2) in
    let x0, y0 = bezier_arc p x0 y0 r a1 mid in
    bezier_arc p x0 y0 r mid a2

let arc p ~r ~a1 ~a2 =
  (* Approximate the arc by Bezier curves to allow for arbitrary affine
     transformations. *)
  if not p.curr_pt then failwith "archimedes_graphics.arc: no current point";
  ignore (bezier_arc p p.x p.y r a1 a2)

let close p =
  if p.curr_pt then begin
    (* Search for the beginning of the current sub-path, if any *)
    let x, y = beginning_of_subpath p in
    p.path <- Close (x, y) :: p.path;
    p.x <- x;
    p.y <- y
  end

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

let do_on_backend limits (curx, cury) b = function
  (* FIXME limits are not respected for rectangles and curves *)
  | Move_to (x, y) -> Backend.move_to b x y; (x, y)
  | Line_to (x, y) ->
      let cx, cy, cx', cy' = clipped_segment limits curx cury x y in
      if cx = cx && cx' = cx' then begin
        if cx <> curx || cy <> cury then Backend.move_to b cx cy;
        Backend.line_to b cx' cy';
        if cx' <> x || cy' <> y then Backend.move_to b x y
      end else Backend.move_to b x y;
      (x, y)
  | Rectangle (x, y, w, h) -> Backend.rectangle b x y w h; x, y
  | Curve_to (_, _, x1, y1, x2, y2, x3, y3) ->
      Backend.curve_to b x1 y1 x2 y2 x3 y3; x3, y3
  | Close (x, y) ->
      (* Because of clipping, the beginning_of_subpath calculated by the
         backend may be wrong, we ensure it is correct with a line_to and
         a move_to and close the path to be sure to preserve the same
         behavior *)
      Backend.line_to b x y;
      Backend.move_to b x y;
      Backend.close_path b;
      0., 0.

let stroke_on_backend ?(limits=0., 1., 0., 1.) p b =
  let coords = ref (0., 0.) in
  Backend.clear_path b;
  List.iter (fun action -> coords := do_on_backend limits !coords b action)
    (List.rev p.path);
  Backend.stroke b

let fill_on_backend ?(limits=0., 1., 0., 1.) p b =
  let coords = ref (0., 0.) in
  Backend.clear_path b;
  List.iter (fun action -> coords := do_on_backend limits !coords b action)
    (List.rev p.path);
  Backend.fill b

let current_point p = p.x, p.y

let transform p f =
  let map_f = function
    | Move_to (x, y) ->
      let x, y = f (x, y) in
      Move_to (x, y)
    | Line_to (x, y) ->
      let x, y = f (x, y) in
      Line_to (x, y)
    | Rectangle (x, y, w, h) ->
      let x, y = f (x, y) in
      let x', y' = f (x +. w, y +. h) in
      Rectangle (x, y, x' -. x, y' -. y)
    | Curve_to (x0, y0, x1, y1, x2, y2, x3, y3) ->
      let x0, y0 = f (x0, y0)
      and x1, y1 = f (x1, y1)
      and x2, y2 = f (x2, y2)
      and x3, y3 = f (x3, y3) in
      Curve_to (x0, y0, x1, y1, x2, y2, x3, y3)
    | Close (x, y) ->
      let x, y = f (x, y) in
      Close (x, y)
  in
  let x, y = f (p.x, p.y) in
  {p with path = List.map map_f p.path;
    x = x; y = y}

let print_step = function
  | Move_to (x, y) -> Printf.printf "Move_to (%f, %f)\n" x y
  | Line_to (x, y) -> Printf.printf "Line_to (%f, %f)\n" x y
  | Rectangle (x, y, w, h) -> Printf.printf "Rectangle (%f, %f, %f, %f)\n" x y w h
  | Curve_to (x0, y0, x1, y1, x2, y2, x3, y3) -> Printf.printf "Curve_to (%f, %f, %f, %f, %f, %f, %f, %f)\n" x0 y0 x1 y1 x2 y2 x3 y3
  | Close (x, y) -> Printf.printf "Close (%f, %f)\n" x y

let print_path p = List.iter print_step (List.rev p.path)

let add p to_add = p.path <- to_add.path @ p.path
