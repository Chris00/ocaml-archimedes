(* File: archimedes.ml

   Copyright (C) 2011

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

include Archimedes_impl

let init = Viewport.init

let close = Viewport.close

let fx ?tlog ?(strategy=Sampler.strategy_midpoint)
    ?(criterion=Sampler.criterion_angle ~threshold:3.1) ?min_step ?nsamples
    ?(fill=false) ?fill_base ?fillcolor ?pathstyle vp f a b =
  (* FIXME: For now, pathstyles of Plot.Impulses/Boxes or Interval will
     give maybe unexpected behaviors. We should limit the use of the
     pathstyle here to Lines Points and Linespoints. Question : should we
     use a polymorphic variant type for pathstyle ? Or refactor that
     somehow else. *)
  let sampling =
    Plot.Function.sampling ?tlog ~strategy ~criterion ?min_step ?nsamples f a b
  in
  if fill then begin
    let base = match fill_base with
      | None -> None
      | Some g -> Some (Plot.Function.sampling
                          ?tlog ~strategy ~criterion ?min_step ?nsamples g a b)
    in
    Plot.Function.fill vp ?fillcolor ?base sampling;
    match base with
    | None -> ()
    | Some g -> Plot.Function.x vp ?pathstyle g
  end;
  Plot.Function.x vp ?pathstyle sampling;
