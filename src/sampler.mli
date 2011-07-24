(* File: sampler.mli

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

(** Some utils to sample a function. The main routine, samplefxy, is
    mainly based on an article from Graphics Gems vol. 5, page 173:
    Adaptive Sampling of Parametric Curves. *)

type strategy = float -> float -> float
(** A strategy is a function [f t1 t2] that returns an internal
    point tm between t1 and t2 which will be used to decide if we
    need to increment precision or not. *)

type criterion = float -> float -> float -> float -> float -> float -> bool
(** A criterion is a function [f x1 y1 xm ym x2 y2] which returns true
    if we need to increment precision. *)

val strategy_midpoint : strategy
(** The default strategy: choose the middle point *)

val strategy_random : strategy
(** A strategy that avoids aliasing sampling, but not efficient:
    chooses randomly a point between t1 and t2 *)

val strategy_center_random : strategy
(** A more efficient strategy that avoids aliasing sampling: chooses
    randomly a points between t1 and t2 in its 10% center interval *)

val criterion_none : criterion
(** A criterion that tells to never increment precision *)

val criterion_angle : ?threshold:float -> criterion
(** A criterion that tells to increment precision only if the angle
    xMy is leather than threshold

    @param threshold the minimal angle under which no more precision
    is needed
*)

val criterion_angle_log : bool -> bool -> ?threshold:float -> criterion
(** Same criterion as criterion_angle, but one can tell that the x/y
    axis is logarithmic, and increment precision in a more wise way *)

type t
(** A sampler *)

val create : ?tlog:bool -> ?min_step:float -> ?nsamples:int ->
  ?strategy:strategy -> ?criterion:criterion ->
  (float -> float * float) -> float -> float -> t
(** [create f t1 t2] samples the parametric function f from t1 to t2,
    returning a list of the points in the sample.

    @param tlog do we need to step in a logarithmic way ?

    @param min_step don't increment precision more than this threshold

    @param nsamples base number of samples wanted (cut the space
    between t1 and t2 in nsamples fragments of equivalent size,
    depending on tlog)

    @param strategy a customized strategy, which can be chosen among
    those in this module

    @param criterion a customized criterion, which can be chosen among
    those in this module
*)

val reset : t -> unit
(** [reset s] Resets the sampler *)

val next : t -> (float * float) option
(** [next s] Returns the next point of the sampling *)

