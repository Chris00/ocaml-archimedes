(* File: iterator.mli

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

type t
(** An iterator, created by a "of_*" function, and manipulated
    through next, reset, iter and iter_cache (see below) *)

exception EOI
(** End Of Iterator, the exception raised when next is called on a
    terminated iterator *)

val of_list : float list -> t
(** [of_list l] Transforms a float list into an iterator which will
    return values (position in [l], value at that position) *)
val of_array : float array -> t
(** [of_array a] Transforms a float array into an iterator which will
    return values (position in [a], value at that position) *)
val of_c : (float, Bigarray.float64_elt, Bigarray.c_layout)
  Bigarray.Array1.t -> t
(** [of_c b] Transforms a bigarray with a C layout into an iterator
    which will return values (position in [b], value at that
    position) *)
val of_fortran : (float, Bigarray.float64_elt, Bigarray.fortran_layout)
  Bigarray.Array1.t -> t
(** [of_fortran b] Transforms a bigarray with a Fortran layout into an
    iterator which will return values (position in [b], value at that
    position) *)

val of_list2 : (float * float) list -> t
(** [of_list2 l] Transforms a list of float couples into an iterator
    returning those couples *)
val of_array2 : (float * float) array -> t
(** [array_array2 a] Transforms an array of float couples into an
    iterator returning those couples *)
val of_c2 : (float, Bigarray.float64_elt, Bigarray.c_layout)
  Bigarray.Array2.t -> t
(** [of_c2 b] Transforms a bigarray of float couples with a C layout
    into an iterator returning those couples *)
val of_fortran2 : (float, Bigarray.float64_elt, Bigarray.fortran_layout)
  Bigarray.Array2.t -> t
(** [of_fortran2 b] Transforms a bigarray of float couples with a
    Fortran layout into an iterator returning those couples *)

val of_last : (float * float -> float * float) -> float * float -> t
(** [of_last f start] Create an iterator which creates its next
    element from the last he has computed using the function [f], starting
    from [start] *)

val next : t -> float * float
(** [next iter] returns the next value of [iter] *)
val reset : t -> unit
(** [reset iter] put back [iter] to its initial state *)
val iter : (float * float -> unit) -> t -> unit
(** [iter f iter] apply the function [f] to all values left in the
    iterator (the iterator won't be resetted !) *)

val constant_iterator : float -> t
(** [constant_iterator c] Creates an iterator which starts at (0.,
    [c]) and just increment the x value *)
val zero_iterator : unit -> t
(** [zero_iterator ()] Alias for (constant_iterator 0.) *)

(**/**)

val iter_cache : (float * float -> unit) -> t -> (float * float) list
(** [iter_cache f iter] apply the function [f] to all values left in
    the iterator. Those values are stored reversed in a list which is
    returned.

    FIXME: this is not the signature of an iterator! *)
