(* File: axes_public.mli

   Copyright (C) 2009-2015

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

module type T = sig
  type offset =
    | Relative of float
    | Absolute of float

  val add_x_axis : ?major:(string * float) -> ?minor:(string * float) ->
    ?start:Arrows.style -> ?stop:Arrows.style ->
    ?tics:Tics.t -> ?offset:offset -> Viewport.Viewport.t -> unit

  val add_y_axis : ?major:(string * float) -> ?minor:(string * float) ->
    ?start:Arrows.style -> ?stop:Arrows.style ->
    ?tics:Tics.t -> ?offset:offset -> Viewport.Viewport.t -> unit

  val box : ?tics:Tics.t -> ?tics_alt:Tics.t -> Viewport.Viewport.t -> unit
  val cross : ?tics:Tics.t -> Viewport.Viewport.t -> unit
end
