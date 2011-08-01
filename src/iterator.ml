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

module B = Bigarray

exception EOI

type which =
  | List of float list * float list ref
  | Array of float array
  | C of (float, B.float64_elt, B.c_layout) B.Array1.t
  | Fortran of (float, B.float64_elt, B.fortran_layout) B.Array1.t
  | List2 of (float * float) list * (float * float) list ref
  | Array2 of (float * float) array
  | C2 of (float, B.float64_elt, B.c_layout) B.Array2.t
  | Fortran2 of (float, B.float64_elt, B.fortran_layout) B.Array2.t
  | From_last of ((float * float -> float * float) * (float * float) ref)
  (* | From_2last of ((float * float) * (float * float) -> float * float *)

type t = {
  data : which;
  mutable pos : int;
}

(* Iterators over data giving only y, using iterator's position as x *)
let of_list l = {data = List (l, ref l); pos = 0}
let of_array a = {data = Array a; pos = 0}
let of_c b = {data = C b; pos = 0}
let of_fortran b = {data = Fortran b; pos = 0}

(* Iterators over data giving x and y *)
let of_list2 l = {data = List2 (l, ref l); pos = 0}
let of_array2 a = {data = Array2 a; pos = 0}
let of_c2 b = {data = C2 b; pos = 0}
let of_fortran2 b = {data = Fortran2 b; pos = 0}

let of_last f start = {data = From_last (f, ref start); pos = 0}

let constant_iterator c = of_last (fun (x, y) -> (x +. 1., y)) (0., c)
let zero_iterator () = constant_iterator 0.

let next iter =
  let ret = match iter.data with
    | List (_, {contents = []}) -> raise EOI
    | List (_, ({contents = v :: l'} as l)) -> l := l'; float iter.pos, v
    | Array a ->
        if iter.pos = Array.length a then raise EOI;
        float iter.pos, a.(iter.pos)
    | C b ->
        if iter.pos = B.Array1.dim b then raise EOI;
        float iter.pos, b.{iter.pos}
    | Fortran b -> (* Warning: In Fortran layout, the first index is 1 ! *)
        if succ iter.pos > B.Array1.dim b then raise EOI;
        float iter.pos, B.Array1.get b (succ iter.pos)
    | List2 (_, {contents = []}) -> raise EOI
    | List2 (_, ({contents = p :: l'} as l)) -> l := l'; p
    | Array2 a ->
        if iter.pos = Array.length a then raise EOI;
        a.(iter.pos)
    | C2 b ->
        if iter.pos = B.Array2.dim1 b then raise EOI;
        b.{iter.pos, 0}, b.{iter.pos, 1}
    | Fortran2 b -> (* Warning: In Fortran layout, the first index is 1 ! *)
        if iter.pos > B.Array2.dim1 b then raise EOI;
        b.{(succ iter.pos), 0}, b.{(succ iter.pos), 1}
    | From_last (f, ({contents = p} as last)) ->
        let v = f p in
        last := v;
        v
  in
  iter.pos <- succ iter.pos;
  ret

let reset iter =
  iter.pos <- 0;
  match iter.data with
  | List (l, l') -> l' := l
  | List2 (l, l') -> l' := l
  | Array _ | C _ | Fortran _ | Array2 _ | C2 _ | Fortran2 _ -> ()
  | From_last _ -> ()

let iter f iter =
  try
    while true do f (next iter) done
  with
  | EOI -> ()

let iter_cache f iter =
  let cache = ref [] in
  begin
    try
      while true do
        let (x, y) = next iter in
        f (x, y);
        cache := (x, y) :: !cache
      done
    with
    | EOI -> ()
  end;
  !cache
