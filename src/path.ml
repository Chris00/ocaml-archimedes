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

open Printf
open Bigarray
type vec = (float, float64_elt, fortran_layout) Array1.t

let fourth_pi = atan 1.

(* Helper functions
 ***********************************************************************)

let is_infinite x = 1. /. x = 0.
let min a b = if (a:float) < b then a else b (* assume no NaN *)
let max a b = if (a:float) > b then a else b

let fourth_pi = atan 1.
let pi = 4. *. fourth_pi
let two_pi = 2. *. pi

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


(* Path definition
 ***********************************************************************)

(* Even though the paths contains mutable structures, these will not
   be modified.  Thus, from the point of view of path, [data] can be
   considered immutable. *)
type data =
  | Move_to of float * float
  | Line_to of float * float
  | Curve_to of float * float * float * float * float * float * float * float
  (* Curve_to(x0, y0, x1, y1, x2, y2, x3, y3): Bézier curve, the
     intial point (x0,y0) is the current point (or (x1,y1) if no
     current point exists) and is kept to ease the drawing of the curve. *)
  | Close of float * float

  (* Optimizations for some specific data structures that are used
     for caching data points.  These can continue [Curve_to] and
     [Line_to] subpaths. *)
  | Array of float array * float array
    (* Array(x, y, pt), the x and y indices increase along the path.
       [x] and [y] have the same length and are not empty. *)
  | Fortran of vec * vec

type t = {
  (* Instructions of the path. *)
  mutable path: data Queue.t;
  mutable extents: Matrix.rectangle;
  mutable x: float; (* the X-coord of the current point (if [curr_pt]) *)
  mutable y: float; (* the Y-coord of the current point (if [curr_pt]) *)
  mutable curr_pt: bool; (* whether the current point is set *)
}

let is_empty p = Queue.is_empty p.path
let data p = p.path
let extents p = p.extents

let make () =
  { path = Queue.create();
    extents = { Matrix.x = nan; y = nan; w = 0.; h = 0. };
    x = 0.;
    y = 0.;
    curr_pt = false }

let copy p =
  { path = Queue.copy p.path;
    extents = p.extents; (* immutable *)
    x = p.x;
    y = p.y;
    curr_pt = p.curr_pt }

let clear p =
  Queue.clear p.path;
  p.extents <- { Matrix.x = nan; y = nan; w = 0.; h = 0. };
  p.x <- 0.;
  p.y <- 0.;
  p.curr_pt <- false

let current_point p =
  if p.curr_pt then p.x, p.y
  else failwith "Archimedes.Path.current_point"

let move_to p ~x ~y =
  Queue.add (Move_to(x, y)) p.path;
  (* move_to only updates the current point but not the path extents
     (one can move elsewhere without drawing anything). *)
  p.x <- x;
  p.y <- y;
  p.curr_pt <- true

let rel_move_to p ~x ~y =
  move_to p (p.x +. x) (p.y +. y)

let line_to p ~x ~y =
  (* Note: if there's no current point then line_to behaves as move_to. *)
  if p.curr_pt then (
    Queue.add (Line_to(x, y)) p.path;
    (* Need to include (p.x, p.y) because the previous instruction
       possibly was a move_to which did not update the extents. *)
    p.extents <- update_rectangle p.extents p.x p.y x y;
    p.x <- x;
    p.y <- y
  )
  else move_to p ~x ~y

let rel_line_to p ~x ~y = line_to p (p.x +. x) (p.y +. y)


let unsafe_line_of_array p x y =
  Queue.add (Array(x, y)) p.path;
  (* FIXME: update extents *)
  let lastx = Array.length x - 1 in
  p.x <- x.(lastx);
  p.y <- y.(lastx);
  p.curr_pt <- true

let line_of_array p ?(const_x=false) x ?(const_y=false) y =
  let lenx = Array.length x in
  if lenx <> Array.length y then
    failwith "Archimedes.Path.line_of_array: x and y have different lengths";
  if lenx <> 0 then (
    let x = if const_x then x else Array.copy x in
    let y = if const_y then y else Array.copy y in
    unsafe_line_of_array p x y
  )

let unsafe_line_of_fortran p (x: vec) (y: vec) =
  Queue.add (Fortran(x, y)) p.path;
  (* FIXME: update extents *)
  let lastx = Array1.dim x in
  p.x <- x.{lastx};
  p.y <- y.{lastx};
  p.curr_pt <- true

let ba_copy x =
  let x' = Array1.create (Array1.kind x) (Array1.layout x) (Array1.dim x) in
  Array1.blit x x';
  x'

let line_of_fortran p ?(const_x=false) x ?(const_y=false) y =
  let dimx = Array1.dim x in
  if dimx <> Array1.dim y then
    failwith "Archimedes.Path.line_of_array: x and y have different lengths";
  if dimx <> 0 then (
    let x = if const_x then x else ba_copy x in
    let y = if const_y then y else ba_copy y in
    unsafe_line_of_fortran p x y
  )


let rectangle p ~x ~y ~w ~h =
  let x1 = x +. w and y1 = y +. h in
  Queue.add (Move_to(x,  y)) p.path;
  Queue.add (Line_to(x1, y)) p.path;
  Queue.add (Line_to(x1, y1)) p.path;
  Queue.add (Line_to(x,  y1)) p.path;
  Queue.add (Close(x, y)) p.path;
  p.extents <- update_rectangle p.extents x y x1 y1;
  p.x <- x;
  p.y <- y;
  p.curr_pt <- true

let append p1 p2 =
  Queue.transfer p2.path p1.path;
  let e2 = p2.extents in
  p1.extents <-
    update_rectangle p1.extents e2.Matrix.x e2.Matrix.y e2.Matrix.w e2.Matrix.h;
  p1.x <- p2.x;
  p1.y <- p2.y;
  clear p2

let curve_to_with_curr_pt p ~x0 ~y0 ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  Queue.add (Curve_to(x0, y0, x1, y1, x2, y2, x3, y3)) p.path;
  p.extents <- update_curve p.extents x0 y0 x1 y1 x2 y2 x3 y3;
  p.x <- x3;
  p.y <- y3

let curve_to p ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  let x0, y0 =
    if p.curr_pt then p.x, p.y
    else begin
      Queue.add (Move_to(x1, y1)) p.path;
      p.curr_pt <- true;
      (x1, y1)
    end in
  curve_to_with_curr_pt p ~x0 ~y0 ~x1 ~y1 ~x2 ~y2 ~x3 ~y3

(* Constant to determine the control points so that the bezier curve
   passes by middle point of the arc. *)
let arc_control = 4. /. 3. (* (1 - cos(b))/(sin b),  b = (a1 - a2)/2 *)

let rec internal_bezier_of_arc p curve_to_with_curr_pt x0 y0 ~r ~a1 ~a2 =
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
    curve_to_with_curr_pt p ~x0 ~y0 ~x1 ~y1 ~x2 ~y2 ~x3 ~y3;
    x3, y3
  else (* several Bezier curves are needed. *)
    let mid = 0.5 *. (a1 +. a2) in
    let x0, y0 =
      internal_bezier_of_arc p curve_to_with_curr_pt x0 y0 r a1 mid in
    internal_bezier_of_arc p curve_to_with_curr_pt x0 y0 r mid a2

let bezier_of_arc p curve_to_with_curr_pt ~x0 ~y0 ~r ~a1 ~a2 =
  ignore(internal_bezier_of_arc p curve_to_with_curr_pt x0 y0 ~r ~a1 ~a2)

(* FIXME: Do we want to add an Arc primitive to the path so we can
   rely on the backend if it handles affine transformations? *)
let arc p ~r ~a1 ~a2 =
  (* Approximate the arc by Bezier curves to allow for arbitrary affine
     transformations. *)
  if not p.curr_pt then failwith "archimedes_graphics.arc: no current point";
  bezier_of_arc p curve_to_with_curr_pt ~x0:p.x ~y0:p.y ~r ~a1 ~a2

exception Beginning_of_subpath of (float * float)

(* FIXME: conditions for Array, Fortran *)
let find_beginning_of_subpath = function
  | Move_to(x, y) | Close(x, y) ->
    raise(Beginning_of_subpath(x, y))
  | Array(x, y) -> 
    (* | Array(x, y) :: (Move_to _ | Close _ | Rectangle _) :: _ -> *)
    raise(Beginning_of_subpath(x.(0), y.(0)))
  | Fortran(x, y) -> 
    (* | Fortran(x, y) :: (Move_to _ | Close _ | Rectangle _) :: _ -> *)
    raise(Beginning_of_subpath(x.{0}, y.{0}))
  | (Curve_to _ | Line_to _ | Array _ | Fortran _) -> ()

let beginning_of_subpath p =
  try
    Queue.iter find_beginning_of_subpath p.path;
    failwith "Archimedes.close: no subpath to close"
  with Beginning_of_subpath pt -> pt

let close p =
  if p.curr_pt then begin
    (* Search for the beginning of the current sub-path, if any *)
    let x, y = beginning_of_subpath p in
    Queue.add (Close(x, y)) p.path;
    p.x <- x;
    p.y <- y
  end


let map_data_to f p' = function
  | Move_to (x, y) ->
    let x, y = f (x, y) in
    Queue.add (Move_to (x, y)) p'
  | Line_to (x, y) ->
    let x, y = f (x, y) in
    Queue.add (Line_to (x, y)) p'
  | Curve_to (x0, y0, x1, y1, x2, y2, x3, y3) ->
    let x0, y0 = f (x0, y0)
    and x1, y1 = f (x1, y1)
    and x2, y2 = f (x2, y2)
    and x3, y3 = f (x3, y3) in
    Queue.add (Curve_to (x0, y0, x1, y1, x2, y2, x3, y3)) p'
  | Close (x, y) ->
    let x, y = f(x, y) in
    Queue.add (Close (x, y)) p'
  | Array(x, y) ->
    let len = Array.length x in
    let x' = Array.make len 0. and y' = Array.make len 0. in
    for i = 0 to len - 1 do
      let fx, fy = f (x.(i), y.(i)) in
      x'.(i) <- fx;
      y'.(i) <- fy;
    done;
    Queue.add (Array(x', y')) p'
  | Fortran(x, y) ->
    let len = Array1.dim x in
    let x' = Array1.create float64 fortran_layout len
    and y' = Array1.create float64 fortran_layout len in
    for i = 1 to len do
      let fx, fy = f (x.{i}, y.{i}) in
      x'.{i} <- fx;
      y'.{i} <- fy;
    done;
    Queue.add (Fortran(x', y')) p'
;;
let map p f =
  let p' = make () in
  Queue.iter (map_data_to f p'.path) p.path;
  let x', y' = f(p.x, p.y) in
  p'.x <- x';
  p'.y <- y';
  (* FIXME: recompute the extent. *)
  p'


let transform m p =
  let p = map p (fun (x,y) -> Matrix.transform_point m x y) in
  (* FIXME: recompute the extent. *)
  p


let print_step = function
  | Move_to (x, y) -> printf "Move_to (%f, %f)\n" x y
  | Line_to (x, y) -> printf "Line_to (%f, %f)\n" x y
  | Curve_to (x0, y0, x1, y1, x2, y2, x3, y3) ->
    printf "Curve_to (%f, %f, %f, %f, %f, %f, %f, %f)\n" x0 y0 x1 y1 x2 y2 x3 y3
  | Close (x, y) -> printf "Close (%f, %f)\n" x y
  | Array(x, y) ->
    printf "Array(x, y) with\n  x = [|";
    Array.iter (fun x -> printf "%g; " x) x;
    printf "|]\n  y = [|";
    Array.iter (fun y -> printf "%g; " y) y;
    printf "|]\n"
  | Fortran(x, y) ->
    printf "Fortran(x, y) with\n  x = {|";
    for i = 1 to Array1.dim x do printf "%g; " x.{i} done;
    printf "|}\n  y = {|";
    for i = 1 to Array1.dim x do printf "%g; " y.{i} done;
    printf "|}\n"

let print_path p = Queue.iter print_step p.path

(* Local Variables: *)
(* compile-command: "make -C .. -k" *)
(* End: *)
