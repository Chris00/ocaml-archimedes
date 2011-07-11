(* File: iterator.ml

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

type ff = float * float

type which =
  | List of ff list * ff list
  | Array of ff array
  (* Le code des itérateurs sur fonctions n'est pas dans ce module mais
     dans le module Sampler. Le but ici est simplement de fournir une
     interface unifiée pour tous les types d'itérateurs. Par ailleurs,
     l'itération sur deux fonctions en même temps reste à définir: Que
     faire lorsque sur un sous-ensemble de R, la première fonction est
     définie mais pas la seconde ? *)
  | Function of Sampler.t
  | C
  | Fortran

type which2 =
  | List2 of ff list * ff list * ff list * ff list
  | Array2 of ff array * ff array
  | Function2 of Sampler.t * Sampler.t
  | C2
  | Fortran2

exception EOI

type t = {
  mutable data : which;
  mutable pos : int;
}

type t2 = {
  mutable data2 : which2;
  mutable pos2 : int;
}

let of_list l = {data = List (l, l); pos = 0}
let of_array a = {data = Array a; pos = 0}
let of_function f a b = {data = Function (Sampler.create f a b); pos = 0}

let of_list2 l l' = {data2 = List2 (l, l, l', l'); pos2 = 0}
let of_array2 a a' = {data2 = Array2 (a, a'); pos2 = 0}
let of_function2 f f' a b = {data2 = Function2 (Sampler.create f a b,
                                               Sampler.create f' a b);
                             pos2 = 0}

let next iter = match iter.data with
  | List (x, v :: l) ->
      iter.data <- List (x, l);
      v
  | List _ -> raise EOI
  | Array a ->
      if iter.pos = Array.length a then raise EOI;
      iter.pos <- succ iter.pos;
      a.(pred iter.pos)
  | Function f -> begin match Sampler.next f with
    | None -> raise EOI
    | Some v -> v
    end
  | C -> raise EOI
  | Fortran -> raise EOI

let next2 iter = match iter.data2 with
  | List2 (x, v :: l, x', v' :: l') ->
      iter.data2 <- List2 (x, l, x', l');
      v, v'
  | List2 _ -> raise EOI
  | Array2 (a, a') ->
      if iter.pos2 >= min (Array.length a) (Array.length a') then raise EOI;
      iter.pos2 <- succ iter.pos2;
      a.(pred iter.pos2), a'.(pred iter.pos2)
  | Function2 _ -> raise EOI (* Voir le commentaire du début *)
  | C2 -> raise EOI
  | Fortran2 -> raise EOI

let reset iter = match iter.data with
  | List (l, _) -> iter.data <- List(l, l)
  | Array _ -> iter.pos <- 0
  | Function f -> Sampler.reset f
  | C  -> ()
  | Fortran -> ()

let reset2 iter = match iter.data2 with
  | List2 (l, _, l', _) -> iter.data2 <- List2 (l, l, l', l')
  | Array2 _ -> iter.pos2 <- 0
  | Function2 _ -> () (* Voir le commentaire du début *)
  | C2 -> ()
  | Fortran2 -> ()

let () =
  let i = of_list [(1., 4.); (4., 2.); (4., 2.)] in
  let a, b = next i in
  let c, d = next i in
  let e, f = next i in
  Printf.printf "%f %f %f %f %f %f\n" a b c d e f;
  reset i;
  let g, h = next i in
  let k, l = next i in
  let m, n = next i in
  Printf.printf "%f %f %f %f %f %f\n" g h k l m n


