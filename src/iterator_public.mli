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
  module type Iterator = sig
    type t
    type data

    val of_data : data -> t

    val next : t -> (float * float) option
    val reset : t -> unit
    val iter : t -> (float -> float -> unit) -> unit
  end

  module List : sig
    include Iterator with type data = float list
  end

  module List2 : sig
    include Iterator with type data = (float * float) list
  end

  module Array : sig
    include Iterator with type data = float array
  end

  module Array2 : sig
    include Iterator with type data = (float * float) array
  end

  module Bigarray_C : sig
    open Bigarray

    include Iterator with
      type data = (float, float64_elt, c_layout) Array1.t
  end

  module Bigarray2_C : sig
    open Bigarray

    include Iterator with
      type data = (float, float64_elt, c_layout) Array2.t
  end

  module Bigarray_Fortran : sig
    open Bigarray

    include Iterator with
      type data = (float, float64_elt, fortran_layout) Array1.t
  end

  module Bigarray2_Fortran : sig
    open Bigarray

    include Iterator with
      type data = (float, float64_elt, fortran_layout) Array2.t
  end
end
