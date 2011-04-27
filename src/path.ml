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
  mutable current_point: float * float;
}

let empty () = {
  path = [];
  extents = { Matrix.x = 0.; y = 0.; w = 0.; h = 0. };
  current_point = (0., 0.)
}

let clear p =
  p.path <- [];
  p.extents <- { Matrix.x = 0.; y = 0.; w = 0.; h = 0. };
  p.current_point <- (0., 0.) (* TODO Support current_point in below functions *)

let beginning_of_subpath p =
  let rec aux = function
    | [] -> failwith "Archimedes: No subpath"
    | Move_to (x, y) :: _ -> (x, y)
    | Close (x, y) :: _ -> (x, y)
    | Rectangle (x, y, _, _) :: _ -> (x, y)
    | (Curve_to _ | Line_to _) :: tl -> aux tl
  in
  aux p.path

let update_extents e x0 y0 x1 y1 =
  let x = min e.Matrix.x (min x0 x1)
  and y = min e.Matrix.y (min y0 y1)
  and x' = max (e.Matrix.x +. e.Matrix.w) (max x0 x1)
  and y' = max (e.Matrix.y +. e.Matrix.h) (max y0 y1) in
  { Matrix.x = x; y = y; w = x' -. x; h = y' -. y }

let move_to p ~x ~y =
  p.path <- Move_to (x, y) :: p.path

let line_to p ~x ~y =
  let x0, y0 = beginning_of_subpath p in
  p.path <- Line_to (x, y) :: p.path;
  p.extents <- update_extents p x0 y0 x y

let rel_move_to p ~x ~y =
  let x', y' = beginning_of_subpath p in
  move_to p ~x:(x' + x) ~y:(y' + y)

let rel_line_to p ~x ~y =
  let x', y' = beginning_of_subpath p in
  line_to p ~x:(x' + x) ~y:(y' + y)

let rectangle p ~x ~y ~w ~h =
  p.path <- Rectangle (x, y, w, h) :: p.path;
  p.extents <- update_extents x y (x +. w) (y +. h)

let internal_curve_to p ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  let x0, y0 =
    if 


let curve_to p ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  internal
