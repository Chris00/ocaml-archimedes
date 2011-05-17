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

type strategy = float -> float -> float
type criterion = float -> float -> float -> float -> float -> float -> bool

let strategy_midpoint t1 t2 =
  (t1 +. t2) /. 2.

let strategy_random t1 t2 =
  Random.self_init ();
  t1 +. Random.float (t2 -. t1)

let strategy_center_random t1 t2 =
  let mid = strategy_midpoint t1 t2 in
  let interval = (t2 -. t1) /. 20. in
  strategy_random (mid -. interval) (mid +. interval)

let criterion_none _ _ _ _ _ _ = true

let criterion_angle ?(threshold=3.1) x1 y1 xm ym x2 y2 =
  let sqdist x1 y1 x2 y2 =
    (x2 -. x1) *. (x2 -. x1) +. (y2 -. y1) *. (y2 -. y1)
  in
  let side1 = sqdist x1 y1 xm ym in
  let side2 = sqdist xm ym x2 y2 in
  let side_hyp = sqdist x1 y1 x2 y2 in
  acos ((side1 +. side2 -. side_hyp)
        /. (2. *. sqrt side1 *. sqrt side2)) > threshold

let criterion_angle_log xlog ylog ?(threshold=3.1) x1 y1 xm ym x2 y2 =
  let id x = x in
  let tx = if xlog then log10 else id
  and ty = if ylog then log10 else id in
  let sqdist x1 y1 x2 y2 =
    tx (x2 -. x1) *. tx (x2 -. x1) +. ty (y2 -. y1) *. ty (y2 -. y1)
  in
  let side1 = sqdist x1 y1 xm ym in
  let side2 = sqdist xm ym x2 y2 in
  let side_hyp = sqdist x1 y1 x2 y2 in
  acos ((side1 +. side2 -. side_hyp)
        /. (2. *. sqrt side1 *. sqrt side2)) > threshold

(*let rec refine min_step strategy criterion f
    ?next t1 x1 y1 t2 x2 y2 =
  if abs_float (t1 -. t2) < min_step then (x2, y2, next)
  else
    let m = strategy t1 t2 in
    let xm, ym = f m in
    if criterion x1 y1 xm ym x2 y2 then (x2, y2, next)
    else
      let r_factor = refine min_step strategy criterion f in
      let next' () = r_factor ?next m xm ym t2 x2 y2 in
      r_factor ~next:next' t1 x1 y1 m xm ym*)

let rec refine min_step strategy criterion xylist f t1 x1 y1 t2 x2 y2 =
  if abs_float (t1 -. t2) < min_step then xylist
  else
    let m = strategy t1 t2 in
    let xm, ym = f m in
    if criterion x1 y1 xm ym x2 y2 then xylist
    else
      let r = refine min_step strategy criterion in
      let xylist = r xylist f t1 x1 y1 m xm ym in
      let xylist = (xm, ym) :: xylist in
      r xylist f m xm ym t2 x2 y2

let next tlog t1 t2 nintervals =
  if tlog then t1 *. (t2 /. t1) ** (1. /. nintervals)
  else t1 +. (t2 -. t1) /. nintervals

let samplefxy ?(tlog=false) ?(min_step=1E-9) ?(nsamples=100)
    ?(strategy=strategy_midpoint) ?(criterion=criterion_none) f t1 t2 =
  let rec aux nsamples t1 x1 y1 t2 x2 y2 xylist =
    if nsamples < 2 then List.rev xylist
    else
      let m = next tlog t1 t2 (float (nsamples - 1)) in
      let xm, ym = f m in
      let pts =
        refine min_step strategy criterion xylist f t1 x1 y1 m xm ym in
      aux (nsamples - 1) m xm ym t2 x2 y2 ((xm, ym) :: pts)
  in
  let x1, y1 = f t1
  and x2, y2 = f t2 in
  aux nsamples t1 x1 y1 t2 x2 y2 [(x1, y1)]

let samplefxy_adaptive =
  let strategy = strategy_center_random in
  let criterion = criterion_angle ~threshold:3.14 in
  samplefxy ~nsamples:2 ~strategy ~criterion

module type Common = sig
  type strategy = float -> float -> float
  type criterion = float -> float -> float -> float -> float -> float -> bool
end

module Common = struct
  type strategy = float -> float -> float
  type criterion = float -> float -> float -> float -> float -> float -> bool
end

(* Temporary *)
module FIterator = struct
  include Common

  type d = {
    tlog: bool;
    min_step: float;
    nsamples: int;
    strategy: strategy;
    criterion: criterion;
    f: float -> float;
    a: float;
    b: float
  }

  type data = d

  type t = Iterator.List2.t

  let of_data f =
    let data = samplefxy ~tlog:f.tlog ~min_step:f.min_step
      ~nsamples:f.nsamples ~strategy:f.strategy ~criterion:f.criterion
      (fun x -> x, f.f x) f.a f.b in
    Iterator.List2.of_data data

  let create ?(tlog=false) ?(min_step=1E-9) ?(nsamples=100)
      ?(strategy=strategy_midpoint) ?(criterion=criterion_none) f a b =
    let data = samplefxy ?tlog ?min_step ?nsamples ?strategy ?criterion
      (fun x -> x, f x) a b in
    Iterator.List2.of_data data

  let next = Iterator.List2.next
  let reset = Iterator.List2.reset
end

module FIterator2 = struct
  include Common

  type d = {
    tlog: bool;
    min_step: float;
    nsamples: int;
    strategy: strategy;
    criterion: criterion;
    f: float -> (float * float);
    t1: float;
    t2: float
  }

  type data = d

  type t = Iterator.List2.t

  let create ?(tlog=false) ?(min_step=1E-9) ?(nsamples=100)
      ?(strategy=strategy_midpoint) ?(criterion=criterion_none) f t1 t2 =
    let data = samplefxy ?tlog ?min_step ?nsamples ?strategy ?criterion
      f t1 t2 in
    Iterator.List2.of_data data

  let of_data f =
    let data = samplefxy ~tlog:f.tlog ~min_step:f.min_step
      ~nsamples:f.nsamples ~strategy:f.strategy ~criterion:f.criterion
      f.f f.t1 f.t2 in
    Iterator.List2.of_data data

  let next = Iterator.List2.next
  let reset = Iterator.List2.reset

  let iter = Iterator.iterate next
end
