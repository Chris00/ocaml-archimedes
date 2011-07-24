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


let fx vp ?fill ?fillcolor ?pathstyle f a b =
  (* FIXME: sampler adapted to 1D function?  To avoid duplicate code,
     macro for speed? *)
  (* FIXME: how can we know if we said we want log scale? *)
  let it = Iterator.of_function (fun x -> (x, f x)) a b in
  Plot.xy vp it ?fill ?fillcolor ?pathstyle
