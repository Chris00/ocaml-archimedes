(* File: priorityQueue.mli

   Copyright (C) 2011

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

type 'a t
(** Mutable maximum priority queue, with float priority. *)

val make : unit -> 'a t
(** [make()] returns a new empty priotity queue. *)

val is_empty : 'a t -> bool
(** [is_empty q] tells whether the queue [q] is empty. *)

val add : 'a t -> float -> 'a -> unit
(** [add q p x] adds the elements [x] to the queue [q] with priority [p].

    @raise Invalid_argument if [p] is NaN. *)

val max : 'a t -> 'a
(** [max q] returns an element of [q] with maximum priority.  [q]
    itself is unchanged. *)

val max_priority : 'a t -> float
(** [max_priority q] returns the maximum priority of elements in [q]
    or [neg_infinity] if [q] is empty.  [q] itself is unchanged. *)

val delete_max : 'a t -> 'a
(** [delete_min q] delete an element with maximum priority of [q] and
    return it. *)

val iter : 'a t -> ('a -> unit) -> unit
(** [iter q f] iterates the function [f] on all elements present in
    the queue [q] (which is unchanged).  The order in which elements
    are passed is unspecified. *)
