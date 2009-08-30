(* File: archimedes.ml

   Copyright (C) 2009

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

(* Helper module *)
module String_utils =
struct
  INCLUDE "string_utils.ml"
end

module Color =
struct
  INCLUDE "color.ml"
end

type matrix = { mutable xx: float; mutable yx: float;
                mutable xy: float; mutable yy: float;
                mutable x0: float; mutable y0: float; }

module Matrix =
struct
  INCLUDE "matrix.ml"
end

module Backend =
struct
  INCLUDE "backend.ml"
end

module Coordinate =
struct
  INCLUDE "coordinate.ml"
end

module Pointstyle =
struct
  INCLUDE "pointstyle.ml"
end

module Axes =
struct
  INCLUDE "axes.ml"
end

module Functions =
struct
  INCLUDE "functions.ml"
end

module Iterator =
struct
  INCLUDE "iterator.ml"
end

module Handle =
struct
  INCLUDE "handle.ml"
end
