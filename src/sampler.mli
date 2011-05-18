(* File: sampler_public.mli

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

val strategy_midpoint : strategy
val strategy_random : strategy
val strategy_center_random : strategy

val criterion_none : criterion
val criterion_angle : ?threshold:float -> criterion
val criterion_angle_log : bool -> bool -> ?threshold:float -> criterion

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

type t = {
  data: data;
  mutable p: (float * float) option;
  mutable next: unit -> unit
}

val create : ?tlog:bool -> ?min_step:float -> ?nsamples:int ->
  ?strategy:strategy -> ?criterion:criterion ->
  (float -> float * float) -> float -> float -> t

val of_data : data -> t
val reset : t -> unit
val next : t -> (float * float) option
