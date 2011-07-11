(* File: iterator_public.mli

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

(** Iterations on points (internal module). *)

module type T = sig
  type ff = float * float
  type t
  type t2

  exception EOI

  val of_list : ff list -> t
  val of_list2 : ff list -> ff list -> t2
  val of_array : ff array -> t
  val of_array2 : ff array -> ff array -> t2
  val of_function : (float -> float * float) -> float -> float -> t
  val of_function2 : (float -> float * float) ->
    (float -> float * float) -> float -> float -> t2

  val next : t -> ff
  val next2 : t2 -> ff * ff

  val reset : t -> unit
  val reset2 : t2 -> unit
end
