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

(** Adaptative sampling of functions. *)

type strategy = float -> float -> float
(** A strategy is a function [f t1 t2] that returns an internal
    point tm between t1 and t2 which will be used to decide if we
    need to increment precision or not. *)

type cost = float -> float -> float -> float -> float -> float -> float
(** A cost [f x0 y0 xm ym x1 y1] which returns the cost measuring how
    much the three points [(x0, y0)], [(xm, ym)], and [(x1, y1)]
    differ from a straight line.  A cost of [0.] means one is
    satisfied with it. *)

val xy : ?tlog:bool -> ?n:int -> ?strategy:strategy -> ?cost:cost ->
  (float -> float * float) -> float -> float -> float array * float array
(** [create f t1 t2] samples the parametric function f from t1 to t2,
    returning a list of the points in the sample.

    @param tlog do we need to step in a logarithmic way ?

    @param min_step don't increment precision more than this threshold

    @param n is a maximum number of evaluation of [f] we allow.
             Default: [100].
    @param strategy a customized strategy.
    @param cost a customized cost.
*)

val x : ?tlog:bool -> ?n:int -> ?strategy:strategy -> ?cost:cost ->
  (float -> float) -> float -> float -> float array * float array


val strategy_midpoint : strategy
(** The default strategy: choose the middle point *)

val strategy_random : strategy
(** A strategy that avoids aliasing sampling, but not efficient:
    chooses randomly a point between t1 and t2 *)

val strategy_center_random : strategy
(** A more efficient strategy that avoids aliasing sampling: chooses
    randomly a points between t1 and t2 in its 10% center interval *)


val cost_angle : cost
(** A criterion that tells to increment precision only if the angle
    xMy is leather than threshold. *)

val cost_angle_log : bool -> bool -> cost
(** Same criterion as criterion_angle, but one can tell that the x/y
    axis is logarithmic, and increment precision in a more wise way *)

