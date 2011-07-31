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
  mutable x: float; (* the X-coord of the current point (if [curr_pt]) *)
  mutable y: float; (* the Y-coord of the current point (if [curr_pt]) *)
  mutable curr_pt: bool; (* whether the current point is set *)
  mutable sub_x: float; (* beginning of subpath *)
  mutable sub_y: float;
  mutable sub: bool; (* beginning of subpath set *)
  (* Instructions of the path. *)
  path_with_extents: data Queue.t; (* Portion of the path for which
                                      the extents was computed. *)
  path: data Queue.t; (* portion of the path for which
                         no extents was computed yet *)
  mutable e_curr_x: float; (* current point of path_with_extents,
                              needed if its last instruction is a move_to *)
  mutable e_curr_y: float;
  (* Extents, rectangle of diagonal (x0,y0) -- (x1,y1).
     Invariant: x0 <= x1, y0 <= y1 (unless empty) *)
  mutable x0: float;
  mutable y0: float;
  mutable x1: float;
  mutable y1: float;
}

let is_empty p =
  Queue.is_empty p.path && Queue.is_empty p.path_with_extents

let iter p f =
  Queue.iter f p.path_with_extents;
  Queue.iter f p.path

let make () =
  { x = 0.;
    y = 0.;
    curr_pt = false;
    sub_x = nan;
    sub_y = nan;
    sub = false;
    path = Queue.create();
    path_with_extents = Queue.create();
    e_curr_x = nan;  e_curr_y = nan;
    (* Default values so we are sure they will be updated when
       computing the extents *)
    x0 = infinity;  y0 = infinity;
    x1 = neg_infinity;  y1 = neg_infinity }

let copy p =
  { p with
    path = Queue.copy p.path;
    path_with_extents = Queue.copy p.path_with_extents }

let clear p =
  p.x <- 0.;
  p.y <- 0.;
  p.curr_pt <- false;
  p.sub_x <- nan;
  p.sub_y <- nan;
  p.sub <- false;
  Queue.clear p.path;
  Queue.clear p.path_with_extents;
  p.e_curr_x <- nan;  p.e_curr_y <- nan;
  p.x0 <- infinity;  p.y0 <- infinity;
  p.x1 <- neg_infinity;  p.y1 <- neg_infinity

let current_point p =
  if p.curr_pt then p.x, p.y
  else failwith "Archimedes.Path.current_point"

(* Compute extents
 ***********************************************************************)

(** Update [p] extents to include the point (x,y). *)
let update_point p x y =
  if x < p.x0 then p.x0 <- x
  else if x > p.x1 then p.x1 <- x;
  if y < p.y0 then p.y0 <- y
  else if y > p.y1 then p.y1 <- y

(** Updaye [p] to contain the Bézier curve given by the control
    points. *)
let update_curve p x0 y0 x1 y1 x2 y2 x3 y3 =
  let xmin, xmax = range_bezier x0 x1 x2 x3 in
  let ymin, ymax = range_bezier y0 y1 y2 y3 in
  update_point p xmin ymin;
  update_point p xmax ymax

let update_extents_data p = function
  | Move_to(x, y) ->
    (* move_to only updates the current point but not the path extents
       (one can move elsewhere without drawing anything). *)
    p.e_curr_x <- x;
    p.e_curr_y <- y
  | Line_to(x, y) ->
    (* Need to include (e_curr_x, e_curr_y) because the previous
       instruction possibly was a move_to which did not update the
       extents. *)
    update_point p p.e_curr_x p.e_curr_y;
    update_point p x y;
    p.e_curr_x <- x;
    p.e_curr_y <- y
  | Curve_to(x0, y0, x1, y1, x2, y2, x3, y3) ->
    update_curve p x0 y0 x1 y1 x2 y2 x3 y3;
    p.e_curr_x <- x3;
    p.e_curr_y <- y3
  | Close(x, y) ->
    p.e_curr_x <- x;
    p.e_curr_y <- y
  | Array(x, y) ->
    update_point p p.e_curr_x p.e_curr_y;
    let last = Array.length x - 1 in
    for i = 0 to last do update_point p x.(i) y.(i) done;
    p.e_curr_x <- x.(last);
    p.e_curr_y <- y.(last)
  | Fortran(x, y) ->
    update_point p p.e_curr_x p.e_curr_y;
    let last = Array1.dim x in
    for i = 1 to last do update_point p x.{i} y.{i} done;
    p.e_curr_x <- x.{last};
    p.e_curr_y <- y.{last}

let update_extents p =
  Queue.iter (update_extents_data p) p.path;
  Queue.transfer p.path p.path_with_extents

let extents p =
  if not(Queue.is_empty p.path) then update_extents p;
  { Matrix.x = p.x0;  y = p.y0;
    w = p.x1 -. p.x0;  h = p.y1 -. p.y0 }

(* Creating paths
 ***********************************************************************)

let move_to p ~x ~y =
  Queue.add (Move_to(x, y)) p.path;
  p.x <- x;
  p.y <- y;
  p.curr_pt <- true;
  p.sub_x <- x;
  p.sub_y <- y;
  p.sub <- true

let rel_move_to p ~x ~y =
  if not p.curr_pt then failwith "Archimedes.Path.rel_move_to";
  move_to p (p.x +. x) (p.y +. y)

let line_to p ~x ~y =
  (* Note: if there's no current point then line_to behaves as move_to. *)
  if p.curr_pt then (
    Queue.add (Line_to(x, y)) p.path;
    p.x <- x;
    p.y <- y
  )
  else move_to p ~x ~y

let rel_line_to p ~x ~y =
  if not p.curr_pt then failwith "Archimedes.Path.rel_line_to";
  line_to p (p.x +. x) (p.y +. y)


let unsafe_line_of_array p x y =
  if not p.curr_pt then (
    (* No current point, so line_to(x.(0), y.(0)) behaves like move_to *)
    p.sub_x <- x.(0);
    p.sub_y <- y.(0);
    p.sub <- true;
  );
  Queue.add (Array(x, y)) p.path;
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
  if not p.curr_pt then (
    (* No current point, so line_to(x.{1}, y.{1}) behaves like move_to *)
    p.sub_x <- x.{1};
    p.sub_y <- y.{1};
    p.sub <- true;
  );
  Queue.add (Fortran(x, y)) p.path;
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
  p.x <- x;
  p.y <- y;
  p.curr_pt <- true;
  p.sub <- false

let curve_to_with_curr_pt p ~x0 ~y0 ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  Queue.add (Curve_to(x0, y0, x1, y1, x2, y2, x3, y3)) p.path;
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


let close p =
  if p.curr_pt then begin
    if not p.sub then failwith "Archimedes.close: no subpath to close";
    Queue.add (Close(p.sub_x, p.sub_y)) p.path;
    p.x <- p.sub_x;
    p.y <- p.sub_y;
    p.sub <- false
  end


let append p1 p2 =
  if p2.curr_pt then ( (* if p2 is empty, do not change p1 *)
    p1.x <- p2.x;
    p1.y <- p2.y;
    p1.curr_pt <- true;
    p1.sub_x <- p2.sub_x;
    p1.sub_y <- p2.sub_y;
    p1.sub <- p2.sub;
  );
  (* If p1 contains a part for which the extents was not computed one
     must update the extents of p1 or append p2 to p1 and loose its
     extents.  We use a naïve heuristic (does not take in account the
     number of operations of< embedded arrays, a better cost could be
     added to the path type) *)
  if Queue.is_empty p1.path_with_extents
    || Queue.length p1.path > 2 * Queue.length p2.path_with_extents then (
      Queue.transfer p2.path_with_extents p1.path;
      Queue.transfer p2.path p1.path;
    )
  else (
    update_extents p1; (* => p1.path empty *)
    Queue.transfer p2.path_with_extents p1.path_with_extents;
    Queue.transfer p2.path p1.path;
    update_point p1 p2.x0 p2.y0;
    update_point p1 p2.x1 p2.y1;
    p1.e_curr_x <- p2.e_curr_x;
    p1.e_curr_y <- p2.e_curr_y;
  );
  clear p2


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

let map p f =
  let p' = make () in
  (* Keep p'.path_with_extents empty because the extents need to be
     recomputed. *)
  iter p (map_data_to f p'.path);
  let x', y' = f(p.x, p.y) in
  p'.x <- x';
  p'.y <- y';
  p'.curr_pt <- p.curr_pt;
  let x', y' = f(p.sub_x, p.sub_y) in
  p'.sub_x <- x';
  p'.sub_y <- y';
  p'.sub <- p.sub;
  p'


let transform m p =
  map p (fun (x,y) -> Matrix.transform_point m x y)

let print_data out = function
  | Move_to (x, y) -> fprintf out "Move_to (%f, %f)\n" x y
  | Line_to (x, y) -> fprintf out "Line_to (%f, %f)\n" x y
  | Curve_to (x0, y0, x1, y1, x2, y2, x3, y3) ->
    fprintf out "Curve_to (%f, %f, %f, %f, %f, %f, %f, %f)\n"
      x0 y0 x1 y1 x2 y2 x3 y3
  | Close (x, y) -> fprintf out "Close (%f, %f)\n" x y
  | Array(x, y) ->
    fprintf out "Array(x, y) with\n  x = [|";
    Array.iter (fun x -> fprintf out "%g; " x) x;
    fprintf out "|]\n  y = [|";
    Array.iter (fun y -> fprintf out "%g; " y) y;
    fprintf out "|]\n"
  | Fortran(x, y) ->
    fprintf out "Fortran(x, y) with\n  x = {|";
    for i = 1 to Array1.dim x do fprintf out "%g; " x.{i} done;
    fprintf out "|}\n  y = {|";
    for i = 1 to Array1.dim x do fprintf out "%g; " y.{i} done;
    fprintf out "|}\n"

let fprint out p = iter p (print_data out)

(* Local Variables: *)
(* compile-command: "make -C .. -k" *)
(* End: *)
