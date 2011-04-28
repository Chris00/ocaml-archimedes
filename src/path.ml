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
    extents = { Matrix.x = 0.; y = 0.; w = 0.; h = 0. };
    x = 0.;
    y = 0.;
    curr_pt <- false }

let clear p =
  p.path <- [];
  p.extents <- { Matrix.x = 0.; y = 0.; w = 0.; h = 0. };
  x = 0.;
  y = 0.;
  curr_pt <- false

let extents p =
  Matrix.copy p.extents

let beginning_of_subpath p =
  let rec aux = function
    | [] -> failwith "Archimedes: No subpath"
    | Move_to (x, y) :: _ -> (x, y)
    | Close (x, y) :: _ -> (x, y)
    | Rectangle (x, y, _, _) :: _ -> (x, y)
    | (Curve_to _ | Line_to _) :: tl -> aux tl
  in
  aux p.path

let update_rectangle e x0 y0 x1 y1 =
  let x = min e.Matrix.x (min x0 x1)
  and y = min e.Matrix.y (min y0 y1)
  and x' = max (e.Matrix.x +. e.Matrix.w) (max x0 x1)
  and y' = max (e.Matrix.y +. e.Matrix.h) (max y0 y1) in
  { Matrix.x = x; y = y; w = x' -. x; h = y' -. y }

let move_to p ~x ~y =
  p.path <- Move_to (x, y) :: p.path;
  p.x <- x;
  p.y <- y;
  p.curr_pt <- true

let line_to p ~x ~y =
  if curr_pt then begin
    p.path <- Line_to (x, y) :: p.path;
    p.extents <- update_rectangle p.x p.y x y;
    p.x <- x;
    p.y <- y
  end else move_to p ~x ~y

let rel_move_to p ~x ~y =
  move_to p ~x:(p.x + x) ~y:(p.y + y)

let rel_line_to p ~x ~y =
  line_to p ~x:(p.x + x) ~y:(p.y + y)

let rectangle p ~x ~y ~w ~h =
  p.path <- Rectangle (x, y, w, h) :: p.path;
  p.extents <- update_rectangle x y (x +. w) (y +. h);
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

(** Return the smaller rectangle containing [r] and the Bézier curve
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
    if t.curr_pt then p.x, p.y
    else begin
      p.path <- Move_to(x1, y1) :: t.current_path;
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
  ignore(bezier_arc p st p.x p.y r a1 a2)

let close p =
  if p.curr_pt then begin
    (* Search for the beginning of the current sub-path, if any *)
    let x, y = beginning_of_subpath p.current_path in
    p.current_path <- CLOSE_PATH(x, y) :: p.current_path;
    p.x <- x;
    p.y <- y
  end

let stroke_on_backend p b =
  Backend.clear_path b;
  List.iter begin function
  | Move_to (x, y) -> Backend.move_to b x y
  | Line_to (x, y) -> Backend.line_to b x y
  | Rectangle (x, y, w, h) -> Backend.rectangle b x y w h
  | Curve_to (_, _, x1, y1, x2, y2, x3, y3) ->
      Backend.curve_to b x1 y1 x2 y2 x3 y3
  | Close_path (x, y) -> Backend.close_path b x y
  end (List.rev p.current_path);
  Backend.stroke b
