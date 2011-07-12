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
  type t
  exception EOI

  val of_list : float list -> t
  val of_array : float array -> t
  val of_c : (float, Bigarray.float64_elt, Bigarray.c_layout)
    Bigarray.Array1.t -> t
  val of_fortran : (float, Bigarray.float64_elt, Bigarray.fortran_layout)
    Bigarray.Array1.t -> t

  val of_list2 : (float * float) list -> t
  val of_array2 : (float * float) array -> t
  val of_c2 : (float, Bigarray.float64_elt, Bigarray.c_layout)
    Bigarray.Array2.t -> t
  val of_fortran2 : (float, Bigarray.float64_elt, Bigarray.fortran_layout)
    Bigarray.Array2.t -> t
  val of_function : (float -> float * float) -> float -> float -> t

  val next : t -> float * float
  val reset : t -> unit
end
