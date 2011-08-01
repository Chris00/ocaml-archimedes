(* File: priorityQueue.ml

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

(* Maximum priority queue.
   Implemented as a Pairing heap (http://en.wikipedia.org/wiki/Pairing_heap).

   FIXME: Is it interesting to implement it as a Fibonacci heap? *)


type 'a pairing_heap =
| Empty
| Heap of float * 'a * 'a pairing_heap list
(* Heap(priority, el, _) *)

type 'a t = 'a pairing_heap ref


let make () = ref Empty

let is_empty q = !q = Empty

let max q = match !q with
  | Empty -> failwith "PriorityQueue.max: empty"
  | Heap(_, x, _) -> x

let max_priority q = match !q with
  | Empty -> neg_infinity
  | Heap(p, _, _) -> p

let merge q1 q2 =
  match q1, q2 with
  | Empty, _ -> q2
  | _, Empty -> q1
  | Heap(p1, e1, h1), Heap(p2, e2, h2) ->
    if p1 > p2 then Heap(p1, e1, q2 :: h1)
    else Heap(p2, e2, q1 :: h2)

let add q p x =
  q := merge (Heap(p, x, [])) !q

let rec merge_pairs = function
  | [] -> Empty
  | [h] -> h
  | h1 :: h2 :: tl -> merge (merge h1 h2) (merge_pairs tl)

let delete_max q =
  match !q with
  | Empty -> failwith "PriorityQueue.delete_max: empty"
  | Heap(_, x, []) -> q := Empty; x
  | Heap(_, x, [h]) -> q := h; x
  | Heap(_, x, hs) -> q := merge_pairs hs; x


let rec iter_ph f = function
  | Empty -> ()
  | Heap(_, x, hs) -> f x;  List.iter (fun h -> iter_ph f h) hs

let iter q f = iter_ph f !q
