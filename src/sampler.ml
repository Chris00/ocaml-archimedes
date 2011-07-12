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
  (t1 +. t2) *. 0.5

let strategy_random t1 t2 =
  Random.self_init ();
  t1 +. Random.float (t2 -. t1)

let strategy_center_random t1 t2 =
  let mid = strategy_midpoint t1 t2 in
  let interval = (t2 -. t1) *. 0.05 in
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

type data = {
  tlog: bool;
  min_step: float;
  nsamples: int;
  strategy: strategy;
  criterion: criterion;
  f: float -> (float * float);
  t1: float;
  t2: float
}

(* data contains the function to plot and the parameters associated
   (strategy, no more refinement criterion, etc.), p contains the next
   point to give to the user, and next contains an OCaml function that
   will update both p and next to get a new "next point" and a new "next
   function". *)
type t = {
  data: data;
  mutable p: (float * float) option;
  mutable next: unit -> unit
}

let rec refine iter t1 x1 y1 t2 x2 y2 next =
  if abs_float (t1 -. t2) < iter.data.min_step then begin
    iter.p <- Some (x2, y2);
    iter.next <- next
  end else
    let m = iter.data.strategy t1 t2 in
    let xm, ym = iter.data.f m in
    if iter.data.criterion x1 y1 xm ym x2 y2 then begin
      iter.p <- Some (x2, y2);
      iter.next <- next
    end else
      let next' () = refine iter m xm ym t2 x2 y2 next in
      refine iter t1 x1 y1 m xm ym next'

let rec sample_interval iter nsamples t1 x1 y1 t2 x2 y2 =
  if nsamples < 2 then begin
    iter.p <- None;
    iter.next <- (fun () -> ())
  end else
    let m = if iter.data.tlog
    then t1 *. (t2 /. t1) ** (1. /. (float (nsamples - 1)))
    else t1 +. (t2 -. t1) /. (float (nsamples - 1)) in
    let xm, ym = iter.data.f m in
    let next' () = sample_interval iter (nsamples - 1) m xm ym t2 x2 y2 in
    refine iter t1 x1 y1 m xm ym next'

let reset iter =
  iter.p <- Some (iter.data.f iter.data.t1);
  let t1, t2 = iter.data.t1, iter.data.t2 in
  let x1, y1 = iter.data.f t1
  and x2, y2 = iter.data.f t2 in
  let next' () = sample_interval iter iter.data.nsamples t1 x1 y1 t2 x2 y2 in
  iter.next <- next'

let next iter =
  let v = iter.p in
  iter.next ();
  v

let create ?(tlog=false) ?(min_step=1E-9) ?(nsamples=100)
    ?(strategy=strategy_midpoint) ?(criterion=criterion_none) f t1 t2 =
  let d = {tlog=tlog; min_step=min_step; nsamples=nsamples;
           strategy=strategy; criterion=criterion; f=f; t1=t1; t2=t2} in
  let iter = {data = d; p = None; next = (fun () -> ())} in
  reset iter;
  iter
