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

module type Iterator = sig
  type t
  type data

  val of_data : data -> t

  val next : t -> (float * float) option
  val reset : t -> unit
  val iter : (float * float -> unit) -> t -> unit
  val iter_cache : (float * float -> unit) -> t -> (float * float) list
end

let rec iterate next f iter =
  match next iter with
  | None -> ()
  | Some p -> f p; iterate next f iter

let rec iterate_cache next starting_list f iter =
  match next iter with
  | None -> starting_list
  | Some p -> f p; iterate_cache next (p :: starting_list) f iter

module List = struct
  type data = float list
  type t = {data:data; mutable data_curr:data; mutable pos:int}

  let of_data l = {
    data = l;
    data_curr = l;
    pos = -1 (* that way, x starts from 0 *)
  }

  let reset iter =
    iter.data_curr <- iter.data;
    iter.pos <- -1

  let next iter = match iter.data_curr with
    | [] -> None
    | x :: tl ->
        iter.data_curr <- tl;
        iter.pos <- succ iter.pos;
        Some (float iter.pos, x)

  let iter = iterate next
  let iter_cache = iterate_cache next []
end

module List2 = struct
  type data = (float * float) list
  type t = {data:data; mutable data_curr:data}

  let of_data l = {
    data = l;
    data_curr = l
  }

  let reset iter =
    iter.data_curr <- iter.data

  let next iter = match iter.data_curr with
    | [] -> None
    | p :: tl ->
        iter.data_curr <- tl;
        Some p

  let iter = iterate next
  let iter_cache = iterate_cache next []
end

module A = Array

module Array = struct
  type data = float array
  type t = {data:data; mutable pos:int}

  let of_data a = {
    data = a;
    pos = -1 (* that way, x starts from 0 *)
  }

  let reset iter =
    iter.pos <- -1

  let next iter =
    if iter.pos >= (A.length iter.data) then
      None
    else begin
      iter.pos <- succ iter.pos;
      Some (float iter.pos, A.get iter.data iter.pos)
    end

  let iter = iterate next
  let iter_cache = iterate_cache next []
end

module Array2 = struct
  type data = (float * float) array
  type t = {data:data; mutable pos:int}

  let of_data a = {
    data = a;
    pos = -1 (* that way, x starts from 0 *)
  }

  let reset iter =
    iter.pos <- -1

  let next iter =
    if iter.pos >= (A.length iter.data) then
      None
    else begin
      iter.pos <- succ iter.pos;
      Some (A.get iter.data iter.pos)
    end

  let iter = iterate next
  let iter_cache = iterate_cache next []
end

module Bigarray_ = struct
  open Bigarray

  type 'a datai = (float, float64_elt, 'a) Array1.t
  type 'a ti = {data:'a datai; mutable pos:int}

  let of_data ba = {
    data = ba;
    pos = -1 (* that way, x starts from 0 *)
  }

  let reset iter =
    iter.pos <- -1

  let next iter =
    if iter.pos >= Array1.dim iter.data then None
    else begin
      iter.pos <- succ iter.pos;
      Some (float iter.pos, iter.data.{iter.pos})
    end
end

module Bigarray_C = struct
  include Bigarray_
  type data = Bigarray.c_layout datai
  type t = Bigarray.c_layout ti

  let iter = iterate next
  let iter_cache = iterate_cache next []
end

module Bigarray_Fortran = struct
  include Bigarray_
  type data = Bigarray.fortran_layout datai
  type t = Bigarray.fortran_layout ti

  let iter = iterate next
  let iter_cache = iterate_cache next []
end

module Bigarray2_ = struct
  open Bigarray

  type 'a datai = (float, float64_elt, 'a) Array2.t
  type 'a ti = {data:'a datai; mutable pos:int}

  let of_data ba = {
    data = ba;
    pos = -1 (* that way, x starts from 0 *)
  }

  let reset iter =
    iter.pos <- -1

  let next iter =
    if iter.pos >= Array2.dim1 iter.data then None
    else begin
      iter.pos <- succ iter.pos;
      Some (iter.data.{iter.pos, 0}, iter.data.{iter.pos, 1})
    end
end

module Bigarray2_C = struct
  include Bigarray2_
  type data = Bigarray.c_layout datai
  type t = Bigarray.c_layout ti

  let iter = iterate next
  let iter_cache = iterate_cache next []
end

module Bigarray2_Fortran = struct
  include Bigarray2_
  type data = Bigarray.fortran_layout datai
  type t = Bigarray.fortran_layout ti

  let iter = iterate next
  let iter_cache = iterate_cache next []
end

module Function = struct
  include Sampler

  let iter = iterate next
  let iter_cache = iterate_cache next []
end
