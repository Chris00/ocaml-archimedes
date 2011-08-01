(* File: sampler.ml

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

(* Ideas taken from Graphics Gems vol. 5, page 173: Adaptive Sampling
   of Parametric Curves. *)

open Utils
module PQ = PriorityQueue

type strategy = float -> float -> float
type cost = float -> float -> float -> float -> float -> float -> float

let strategy_midpoint t1 t2 =
  (t1 +. t2) *. 0.5

let () = Random.self_init()

let strategy_random t1 t2 =
  t1 +. Random.float (t2 -. t1)

let strategy_center_random t1 t2 =
  0.5 *. (t1 +. t2) +. Random.float((t2 -. t1) *. 0.1)

let pi = 4. *. atan 1.

let cost_angle x1 y1 xm ym x2 y2 =
  let dx1m = x1 -. xm and dy1m = y1 -. ym in
  let dx2m = x2 -. xm and dy2m = y2 -. ym in
  let dx12 = x1 -. x2 and dy12 = y1 -. y2 in
  let sq_d1m = dx1m *. dx1m +. dy1m *. dy1m
  and sq_d2m = dx2m *. dx2m +. dy2m *. dy2m
  and sq_d12 = dx12 *. dx12 +. dy12 *. dy12 in
  let cos_m = (sq_d1m +. sq_d2m -. sq_d12) /. (2. *. sqrt sq_d1m *. sqrt sq_d2m) in
  cos_m +. 0.999 (* ~ π - acos cos_m; 0.999 instead of 1. so as to allow
                    a small error *)

let cost_area x1 y1 xm ym x2 y2 =
  let dx1m = x1 -. xm and dy1m = y1 -. ym in
  let dx2m = x2 -. xm and dy2m = y2 -. ym in
  0.5 *. abs_float(dx1m *. dy2m -. dy1m *. dx2m)

let cost_circum x1 y1 xm ym x2 y2 =
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

let cost_angle_log xlog ylog x1 y1 xm ym x2 y2 =
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

let cost_default = cost_angle

(* Sampling
 ***********************************************************************)

type interval = {
  t0: float; x0: float;  y0: float; (* t0 = left point;  (x0, y0) = f t0 *)
  t1: float; x1: float;  y1: float; (* t1 = right point; (x1, y1) = f t1 *)
  tm: float; xm: float;  ym: float; (* mid point *)
  scale: float; (* scale, decreasing as one divides the interval *)
}

(* FIXME: need to manage NaNs *)
let xy ?tlog ?(n=100)
    ?(strategy=strategy_center_random) ?(cost=cost_default) f a b =
  if not(is_finite a && is_finite b) then
    invalid_arg "Archimedes.Sampler.xy: bounds of the function must be finite";
  let q = PQ.make() in
  (* Rough sampling with 31 points, including middle points *)
  let dt = (b -. a) /. 15. in
  let x0, y0 = f a in
  let x0 = ref x0 and y0 = ref y0 in
  for i = 0 to 14 do
    let t0 = a +. float i *. dt in
    let t1 = t0 +. dt in
    let x1, y1 = f t1 in
    let tm = strategy t0 t1 in
    let xm, ym = f tm in
    PQ.add q (cost !x0 !y0 xm ym x1 y1) { t0 = t0; x0 = !x0; y0 = !y0;
                                          t1 = t1; x1 = x1; y1 = y1;
                                          tm = tm; xm = xm; ym = ym;
                                          scale = 0.5 };
    x0 := x1;  y0 := y1;  (* do not eval f again *)
  done;
  (* Add points (intervals) until the evaluations are exhausted or all
     costs are 0. *)
  let n = ref(n - 31) in
  let continue = ref true in
  while !continue do
    if !n <= 0 || PQ.max_priority q <= 0. then continue := false
    else (
      let i = PQ.delete_max q in
      (* Scale after the interval is divided *)
      let s = i.scale *. i.scale in
      let tm1 = strategy i.t0 i.tm in
      let xm1, ym1 = f tm1 in
      PQ.add q (cost i.x0 i.y0 xm1 ym1 i.xm i.ym *. s)
        { i with
          tm = tm1;  xm = xm1;  ym = ym1;
          t1 = i.tm; x1 = i.xm; y1 = i.ym;
          scale = s };
      decr n;
      if !n > 0 then (
        let tm2 = strategy i.tm i.t1 in
        let xm2, ym2 = f tm2 in
        PQ.add q (cost i.xm i.ym xm2 ym2 i.x1 i.y1 *. s)
          { i with
            t0 = i.tm; x0 = i.xm; y0 = i.ym;
            tm = tm2;  xm = xm2;  ym = ym2;
            scale = s };
        decr n;
      )
    )
  done;
  (* Sort the interval in order the same order a b was given returns
     the points. *)
  let qt = PQ.make() in
  let ni = ref 0 in
  if a <= b then PQ.iter q (fun i -> incr ni; PQ.add qt (-. i.tm) i)
  else PQ.iter q (fun i -> incr ni; PQ.add qt i.tm i);
  let n = 2 * !ni + 1 in
  let x = Array.make n 0.
  and y = Array.make n 0. in
  let first_i = PQ.max qt in
  x.(0) <- first_i.x0;
  y.(0) <- first_i.y0;
  for k = 1 to !ni do
    let i = PQ.delete_max qt in
    let k1 = 2 * k in
    let km = k1 - 1 in
    x.(km) <- i.xm;  y.(km) <- i.ym;
    x.(k1) <- i.x1;  y.(k1) <- i.y1;
  done;
  x, y
;;

let x ?tlog ?(n=100)
    ?(strategy=strategy_center_random) ?(cost=cost_default) f a b =
  xy ?tlog ~n ~strategy ~cost (fun x -> (x, f x)) a b
