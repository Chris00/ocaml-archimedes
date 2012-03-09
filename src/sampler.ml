(* File: sampler.ml

   Copyright (C) 2009-2011

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     Pierre Hauweele <pierre@hauweele.net>
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

(* Ideas taken from Graphics Gems vol. 5, page 173: Adaptive Sampling
   of Parametric Curves. *)

open Utils
module PQ = PriorityQueue

type strategy = float -> float -> float
type cost =
  Matrix.rectangle ->float -> float -> float -> float -> float -> float -> float

let strategy_midpoint t1 t2 =
  (t1 +. t2) *. 0.5

let () = Random.self_init()

let strategy_random t1 t2 =
  t1 +. Random.float (t2 -. t1)

let strategy_center_random t1 t2 =
  0.5 *. (t1 +. t2) +. Random.float((t2 -. t1) *. 0.05)

let strategy_default = strategy_center_random

let pi = 4. *. atan 1.

let cost_angle _ x1 y1 xm ym x2 y2 =
  let dx1m = x1 -. xm and dy1m = y1 -. ym in
  let dx2m = x2 -. xm and dy2m = y2 -. ym in
  let dx12 = x1 -. x2 and dy12 = y1 -. y2 in
  let sq_d1m = dx1m *. dx1m +. dy1m *. dy1m
  and sq_d2m = dx2m *. dx2m +. dy2m *. dy2m
  and sq_d12 = dx12 *. dx12 +. dy12 *. dy12 in
  let cos_m = (sq_d1m +. sq_d2m -. sq_d12) /. (2. *. sqrt sq_d1m *. sqrt sq_d2m) in
  (* cos_m +. 0.999 (\* ~ π - acos cos_m; 0.999 instead of 1. so as to allow *)
  (*                   a small error *\) *)
  pi -. acos cos_m

let cost_angle_dist bb x1 y1 xm ym x2 y2 =
  let dx1m = x1 -. xm and dy1m = y1 -. ym in
  let dx2m = x2 -. xm and dy2m = y2 -. ym in
  let dx12 = x1 -. x2 and dy12 = y1 -. y2 in
  let sq_d1m = dx1m *. dx1m +. dy1m *. dy1m
  and sq_d2m = dx2m *. dx2m +. dy2m *. dy2m
  and sq_d12 = dx12 *. dx12 +. dy12 *. dy12 in
  let cos_m = (sq_d1m +. sq_d2m -. sq_d12) /. (2. *. sqrt sq_d1m *. sqrt sq_d2m) in
  let hw = bb.Matrix.w +. bb.Matrix.h in
  let rel_dist = (sq_d1m +. sq_d2m) /. (hw *. hw) in
  (* rel_dist *. sqrt(cos_m +. 1.) (\* ~ π - acos cos_m *\) *)
  rel_dist *. (pi -. acos cos_m)

let cost_area _ x1 y1 xm ym x2 y2 =
  let dx1m = x1 -. xm and dy1m = y1 -. ym in
  let dx2m = x2 -. xm and dy2m = y2 -. ym in
  0.5 *. abs_float(dx1m *. dy2m -. dy1m *. dx2m)

let cost_circum _ x1 y1 xm ym x2 y2 =
  let dx1m = x1 -. xm and dy1m = y1 -. ym in
  let dx2m = x2 -. xm and dy2m = y2 -. ym in
  let dx12 = x1 -. x2 and dy12 = y1 -. y2 in
  let sq_d1m = dx1m *. dx1m +. dy1m *. dy1m
  and sq_d2m = dx2m *. dx2m +. dy2m *. dy2m
  and sq_d12 = dx12 *. dx12 +. dy12 *. dy12 in
  let cos_m = (sq_d1m +. sq_d2m -. sq_d12) /. (2. *. sqrt(sq_d1m *. sq_d2m)) in
  let circum_r = sqrt(sq_d12 /. (1. -. cos_m *. cos_m)) in
  (* If the circum radius is small, no need to bother *)
  (cos_m +. 1.) *. sqrt circum_r

let cost_angle_log xlog ylog _ x1 y1 xm ym x2 y2 =
  let id x = x in
  let tx = if xlog then log10 else id
  and ty = if ylog then log10 else id in
  let sqdist x1 y1 x2 y2 =
    tx (x2 -. x1) *. tx (x2 -. x1) +. ty (y2 -. y1) *. ty (y2 -. y1)
  in
  let side1 = sqdist x1 y1 xm ym in
  let side2 = sqdist xm ym x2 y2 in
  let side_hyp = sqdist x1 y1 x2 y2 in
  pi -. acos ((side1 +. side2 -. side_hyp)
        /. (2. *. sqrt side1 *. sqrt side2))

let cost_default = cost_angle_dist

(* Sampling
 ***********************************************************************)

type interval = {
  t0: float; x0: float;  y0: float; (* t0 = left point;  (x0, y0) = f t0 *)
  t1: float; x1: float;  y1: float; (* t1 = right point; (x1, y1) = f t1 *)
  side: float; (* = 0 if [(x0,y0)] and [(x1,y1)] are finite;
                  < 0 if [(x0,y0)] is finite but not the other one;
                  > 0 if [(x1,y1)] is finite but not the other one.  *)
  scale: float; (* cost scaling factor, decreasing as one divides the
                   interval *)
}
(* [t0] and [t1] will always be finite (they are constructed by the
   functions below).  The fact that [side] is a float is to allow the
   float optimization for records to kick in.  *)

let pt_is_finite x y = is_finite x && is_finite y

(** Return a priority queue containing the sampling of [f] on the
    interval [[a,b]] with [n0] equidistant evals.  The initial cost of
    all intervals is infinity to make sure they are processed each at least once. *)
let initial_sampling n0 f a b =
  let q = PQ.make() in
  let dt = (b -. a) /. float(n0-1) in
  (* Compute the bounding box so it can be used to dertermine when two
     points are close (in relative error) — which can be useful for
     the cost function.  Store the eval of [f] which may be costly. *)
  let xmin = ref infinity and xmax = ref neg_infinity
  and ymin = ref infinity and ymax = ref neg_infinity in
  let x_a, y_a = f a in
  let t_prev = ref a and x_prev = ref x_a and y_prev = ref y_a in
  let finite_prev = ref(pt_is_finite x_a y_a) in
  for i = 1 to n0 - 1 do
    let t = a +. float i *. dt in
    let x, y = f t in
    xmin := float_min !xmin x;  xmax := float_max !xmax x;
    ymin := float_min !ymin y;  ymax := float_max !ymax y;
    let finite_xy = pt_is_finite x y in
    if !finite_prev then
      PQ.add q max_float { t0 = !t_prev;  x0 = !x_prev;  y0 = !y_prev;
                           t1 = t;  x1 = x;  y1 = y;
                           side = if finite_xy then 0. else -1.;
                           scale = 0.5 }
    else if finite_xy then
      PQ.add q max_float { t0 = !t_prev;  x0 = !x_prev;  y0 = !y_prev;
                           t1 = t;  x1 = x;  y1 = y;
                           side = 1.;
                           scale = 0.5 };
    (* If both points are not-finite, then drop the interval. *)
    t_prev := t;
    x_prev := x;
    y_prev := y;
    finite_prev := finite_xy;
  done;
  let bb = { Matrix.x = !xmin; y = !ymin;
             w = !xmax -. !xmin;  h = !ymax -. !ymin } in
  q, bb

(* FIXME: need to manage NaNs *)
let xy ?tlog ?(fn0=0.1) ?(n=100) ?(strategy=strategy_default) ?(cost=cost_default) f a b =
  if n < 2 then
    invalid_arg "Archimedes.Sampler.xy: must at least evaluate 3 points \
      to sensibly graph a function";
  if not(is_finite a && is_finite b) then
    invalid_arg "Archimedes.Sampler.xy: bounds of the interval must be finite";
  if not (0. < fn0 && fn0 <= 1.) then
    invalid_arg "Archimedes.Sampler.xy: fn0 must be between 0 < fn0 <= 1";
  let n0 = max 2 (truncate(fn0 *. float n)) in
  let q, bb = initial_sampling n0 f a b in
  (* Add points (intervals) until the number [n] of evaluations is
     exhausted or all costs are <= 0. *)
  let n = ref(n - n0) in
  if PQ.is_empty q then
    failwith "Archimedes.Sampler.xy: initial sampling only returned nan";
  while !n > 0 && PQ.max_priority q > 0. do
    let c0 = PQ.max_priority q in
    let i = PQ.delete_max q in
    (* Scale after the interval is divided *)
    let s = i.scale *. i.scale in
    let tm = strategy i.t0 i.t1 in
    let xm, ym = f tm in
    decr n;
    let finite_m = pt_is_finite xm ym in
    if i.side = 0. then                 (* (x0,y0), (x1,y1) both finite *)
      if finite_m then (
        let c = cost bb i.x0 i.y0 xm ym i.x1 i.y1 *. s in
        let c = if is_finite c then c else c0 *. s in
        PQ.add q c { i with t1 = tm; x1 = xm; y1 = ym; scale = s; side = 0. };
        PQ.add q c { i with t0 = tm; x0 = xm; y0 = ym; scale = s; side = 0. };
      )
      else (
        let c = c0 *. s in
        PQ.add q c { i with t1 = tm; x1 = xm; y1 = ym; scale = s; side = -1. };
        PQ.add q c { i with t0 = tm; x0 = xm; y0 = ym; scale = s; side = 1. };
      )
    else if i.side < 0. then            (* (x0,y0) finite *)
      let c = c0 *. s in
      if finite_m then (
        PQ.add q c { i with t1 = tm; x1 = xm; y1 = ym; scale = s; side = 0. };
        PQ.add q c { i with t0 = tm; x0 = xm; y0 = ym; scale = s; side = -1. }
      )
      else
        PQ.add q c { i with t1 = tm; x1 = xm; y1 = ym; scale = s; side = -1. }
    else (* i.side > 0.  i.e.  (x1,y1) finite *)
      let c = c0 *. s in
      if finite_m then (
        PQ.add q c { i with t1 = tm; x1 = xm; y1 = ym; scale = s; side = 1. };
        PQ.add q c { i with t0 = tm; x0 = xm; y0 = ym; scale = s; side = 0. }
      )
      else
        PQ.add q c { i with t0 = tm; x0 = xm; y0 = ym; scale = s; side = 1. }
  done;
  (* Sort the interval in order the same order a b was given returns
     the points.  There may be less eval than allowed if [cost] say
     one is happy with the result. *)
  let qt = PQ.make() in
  let ni = ref 0 in
  if a <= b then PQ.iter q (fun i -> incr ni; PQ.add qt (-. i.t0) i)
  else PQ.iter q (fun i -> incr ni; PQ.add qt i.t0 i);
  let n = !ni + 1 in
  let x = Array.make n nan
  and y = Array.make n nan in
  (* We were careful above to make sure that if there is a gap,
     intervals around the gap will have a not-finite endoint.
     FIXME: double check. *)
  if n <> 1 then (
    let first_i = PQ.max qt in
    x.(0) <- first_i.x0;
    y.(0) <- first_i.y0;
    for k = 1 to !ni do
      let i = PQ.delete_max qt in
      x.(k) <- i.x1;
      y.(k) <- i.y1;
    done
  ) else warning "sampling a function with no finite point !";
  x, y
;;

let x ?tlog ?fn0 ?(n=100)
    ?(strategy=strategy_center_random) ?(cost=cost_default) f a b =
  xy ?tlog ?fn0 ~n ~strategy ~cost (fun x -> (x, f x)) a b


(* Local Variables: *)
(* compile-command: "make -C .." *)
(* End: *)
