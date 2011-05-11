(* File: arrows_public.mli

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

module type T = sig
  type arrow_style =
    | Simple
    | Double
    | Triple
    | Diamond
    | Circle
    | Custom of (Path.t -> unit)

  val path_line_to : ?size:float -> ?head:arrow_style -> ?tail:arrow_style ->
    Path.t -> float -> float -> unit

  val path_arc : ?size:float -> ?head:arrow_style -> ?tail:arrow_style ->
    Path.t -> float -> float -> float -> unit

  val line : ?size:float -> ?head:arrow_style -> ?tail:arrow_style ->
    Viewport.Viewport.t -> float -> float -> float -> float -> unit

  val arc : ?size:float -> ?head:arrow_style -> ?tail:arrow_style ->
    Viewport.Viewport.t -> float -> float -> float -> float -> float -> unit
end
