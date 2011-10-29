(* File: sampler.mli

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

(** Adaptative sampling of functions. *)

type strategy = float -> float -> float
(** A strategy is a function [f t1 t2] that returns an internal point
    tm between [t1] and [t2] which will be used to decide if we need
    to increment precision or not.  The given [t1] and [t2] will
    always be finite. *)

type cost = Matrix.rectangle ->
  float -> float -> float -> float -> float -> float -> float
(** A cost [f bb x0 y0 xm ym x1 y1] which returns the cost measuring
    how much the three points [(x0, y0)], [(xm, ym)], and [(x1, y1)]
    differ from a straight line.  [bb] is a rough bounding box of the
    set of points that can be used to determine whether two points are
    close (in relative measure).  A cost [<= 0.] means one is satisfied
    with drawing straight lines connecting the three points. *)

val xy : ?tlog:bool -> ?n:int -> ?strategy:strategy -> ?cost:cost ->
  (float -> float * float) -> float -> float -> float array * float array
(** [xy f t1 t2] samples the parametric function [f] on the
    interval going from [t1] to [t2].  Returns a list of the points in
    the sample.

    @param tlog do we need to step in a logarithmic way ?

    @param min_step don't increment precision more than this threshold

    @param n is a maximum number of evaluations of [f] that are allowed.
    Default: [100].
    @param strategy a customized strategy.
    @param cost a customized cost.
*)

val x : ?tlog:bool -> ?n:int -> ?strategy:strategy -> ?cost:cost ->
  (float -> float) -> float -> float -> float array * float array
(** [x f x1 x2] same as {!Sampler.xy} but for the scalar function [f]
    on the interval going from [x1] to [x2]. *)

val strategy_midpoint : strategy
(** The default strategy: choose the middle point *)

val strategy_random : strategy
(** A strategy that avoids aliasing sampling, but not efficient:
    chooses randomly a point between t1 and t2 *)

val strategy_center_random : strategy
(** A more efficient strategy that avoids aliasing sampling: chooses
    randomly a points between t1 and t2 in its 10% center interval *)


val cost_angle : cost
(** Measures the angle at the middle point [(xm, ym)], the flatter the
    angle, the lower the cost. *)

val cost_angle_dist : cost
(** Measures the angle at the middle point [(xm, ym)], the flatter the
    angle, the better and combines it with the distance of the points
    [(x0, y0)] and [(x1, y1)] to the middle point, the smaller the
    distance, the better. *)

val cost_angle_log : bool -> bool -> cost
(** Same criterion as {!cost_angle} suitable for logarithmic
    cordinates. *)

