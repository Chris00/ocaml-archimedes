(* File: iterator.mli

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


(** Iterations on points (internal module). *)

type t

val of_list: (float * float) list -> t

val of_array: (float * float) array -> t

val of_bigarray2: ?clayout:bool -> (float, 'b, 'c)  Bigarray.Array2.t -> t

val of_lists: float list -> float list -> t

val of_arrays: float array -> float array -> t

val of_bigarrays:
  ?xclayout:bool -> (float, 'b, 'c)Bigarray.Array1.t ->
  ?yclayout:bool -> (float, 'b, 'c)Bigarray.Array1.t -> t

val from_sampling :
  (float -> float * float) ->
  ?min_step:float -> ?nsamples:int ->
  float -> float -> t

val next: t -> (float * float) option

val reset : t -> unit

val nb_data : t -> int

val extents : t -> Axes.fixed_ranges
