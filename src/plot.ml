(* File: plot.ml

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

module V = Viewport

module Insets = struct
  type insets = {
    mutable xmin : float; mutable xmax : float;
    mutable ymin : float; mutable ymax : float
  }

  let make () = {
    xmin = infinity; xmax = neg_infinity;
    ymin = infinity; ymax = neg_infinity
  }

  let includepoint insets (x, y) =
    insets.xmin = min insets.xmin x;
    insets.xmax = max insets.xmax x;
    insets.ymin = min insets.ymin y;
    insets.ymax = max insets.ymax y

  let fit vp insets =
    V.auto_fit vp insets.xmin insets.ymin insets.xmax insets.ymax
end

type pathstyle =
  | Lines
  | Points of string
  | Linespoints of string
  | Impulses
  | Boxes of float (* Width in Data coordinates (usually what we want) *)
  | Interval of float (* Width in Data coordinates (to be consistent) *)

let is_finite x = 1. /. x <> 0.

let rec draw_data ?(base=0.) pathstyle path (x, y) =
  if is_finite y then match pathstyle with
  | Lines | Linespoints _ -> Path.line_to path ~x ~y
  | Impulses -> draw_data ~base (Boxes 0.) path (x, y)
  | Points _ -> ()
  | Boxes w ->
      Path.rectangle path ~x:(x -. w *. 0.5) ~y:base ~w ~h:(y -. base)
  | Interval size ->
      Path.move_to path ~x ~y:base;
      Arrows.path_line_to ~size ~head:Arrows.Stop ~tail:Arrows.Stop path x y
  else Path.close path

let close_data ?(base=0.) pathstyle path (x, y) =
  if is_finite y then match pathstyle with
  | Lines | Linespoints _ -> Path.line_to path ~x ~y
  | Impulses | Points _ | Boxes _ | Interval _ -> ()
  else Path.close path

let draw_point pathstyle vp (x, y) =
  if is_finite y then
    match pathstyle with
    | Lines | Impulses | Boxes _ | Interval _ -> ()
    | Points m | Linespoints m -> V.mark vp ~x ~y m

let fillpath vp path color =
  V.save vp;
  V.set_color vp color;
  V.fill ~path vp V.Data;
  V.restore vp

(* Factorizes the x function in most submodules (except Function) *)
let x ?(fill=false) ?(fillcolor=Color.red) ?(pathstyle=Lines)
    ?(base=Iterator.zero_iterator ()) vp iterator =
  let path = Path.make () in
  let closingpath = ref [] in
  let insets = Insets.make () in
  let f p =
    Insets.includepoint insets p;
    let bx, by = Iterator.next base in
    draw_data pathstyle path ~base:by p;
    if fill then closingpath := (bx, by) :: !closingpath
  in
  let data_rev = Iterator.iter_cache f iterator in
  Insets.fit vp insets;
  if fill then begin
    let path = Path.copy path in
    List.iter (close_data pathstyle path) !closingpath;
    fillpath vp path fillcolor
  end;
  V.stroke ~path vp V.Data;
  List.iter (draw_point pathstyle vp) data_rev

(* Factorizes the xy function in most submodules (except Function) *)
let xy ?(fill=false) ?(fillcolor=Color.red) ?(pathstyle=Lines)
    vp iterator =
  let insets = Insets.make () in
  let path = Path.make () in
  let f p =
    Insets.includepoint insets p;
    draw_data pathstyle path p
  in
  let data_rev = Iterator.iter_cache f iterator in
  Insets.fit vp insets;
  if fill then fillpath vp path fillcolor;
  V.stroke ~path vp V.Data;
  List.iter (draw_point pathstyle vp) data_rev

(* Factorizes the stack function in most submodules (except Function) *)
(* TODO choose a better default list of colors / fillcolors *)
let stack ?(colors=[|Color.red; Color.blue|]) ?(fillcolors=[||])
    ?(pathstyle=Boxes 0.5) vp iterators =
  let fill = fillcolors <> [||] in
  let m = Array.length iterators in
  let insets = Insets.make () in
  let curx = ref neg_infinity in
  let curbase = ref 0. in
  let paths = Array.init m (fun _ -> Path.make ()) in
  let backpaths = Array.init (succ m) (fun _ -> []) in
  let stacking i iterator =
    let (x, y) = Iterator.next iterator in
    Insets.includepoint insets (x, y);
    if i = 0 then begin
      curx := x;
      curbase := 0.;
    end
    else assert (!curx = x);
    draw_data ~base:(!curbase) pathstyle paths.(i) (x, y);
    backpaths.(i) <- (x, !curbase) :: backpaths.(i);
    curbase := !curbase +. y;
    if i = pred m then backpaths.(m) <- (x, !curbase) :: backpaths.(m)
  in
  try while true do Array.iteri stacking iterators done
  with Iterator.EOI ->
    Insets.fit vp insets;
    V.save vp;
    let draw i path =
      if fill then begin
        let path = Path.copy path in
        List.iter (close_data pathstyle path) backpaths.(i);
        fillpath vp path fillcolors.(i mod (Array.length fillcolors))
      end;
      (* TODO setcolor *)
      V.stroke ~path vp V.Data;
      List.iter (draw_point pathstyle vp) backpaths.(succ i)
    in
    Array.iteri draw paths;
    V.restore vp

(* The following functions simplify the implementation of x, xy and stack
   in standard submodules *)
let basex transform base ?fill ?fillcolor ?pathstyle vp data =
  x ?fill ?fillcolor ?pathstyle ~base vp (transform data)
let basexy transform ?fill ?fillcolor ?pathstyle vp data =
  xy ?fill ?fillcolor ?pathstyle vp (transform data)
let basestack transform ?colors ?fillcolors ?pathstyle vp datas =
  stack ?colors ?fillcolors ?pathstyle vp (Array.map transform datas)

module type Common = sig
  type data
  type data2

  val x : ?base:data -> ?fill:bool -> ?fillcolor:Color.t ->
    ?pathstyle:pathstyle -> Viewport.t -> data -> unit

  val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    Viewport.t -> data2 -> unit

  val stack : ?colors:(Color.t array) -> ?fillcolors:(Color.t array) ->
    ?pathstyle:pathstyle -> Viewport.t -> data array -> unit
end


module Array = struct
  type data = float array
  type data2 = (float * float) array

  let x ?(base=[||]) = basex Iterator.of_array (Iterator.of_array base)
  let xy = basexy Iterator.of_array2
  let stack = basestack Iterator.of_array
end

module List = struct
  type data = float list
  type data2 = (float * float) list

  let x ?(base=[]) = basex Iterator.of_list (Iterator.of_list base)
  let xy = basexy Iterator.of_list2
  let stack = basestack Iterator.of_list
end

module Fortran = struct
  open Bigarray

  type data = (float, float64_elt, fortran_layout) Array1.t
  type data2 = (float, float64_elt, fortran_layout) Array2.t

  let base = Array1.create float64 fortran_layout 0

  let x ?(base=base) = basex Iterator.of_fortran (Iterator.of_fortran base)
  let xy = basexy Iterator.of_fortran2
  let stack = basestack Iterator.of_fortran
end

module C = struct
  open Bigarray

  type data = (float, float64_elt, c_layout) Array1.t
  type data2 = (float, float64_elt, c_layout) Array2.t

  let base = Array1.create float64 c_layout 0

  let x ?(base=base) = basex Iterator.of_c (Iterator.of_c base)
  let xy = basexy Iterator.of_c2
  let stack = basestack Iterator.of_c
end

module Function = struct
  type 'a sampling = {
    strategy: Sampler.strategy;
    criterion: Sampler.criterion;
    min_step: float;
    nsamples: int;
    fct: (float -> 'a);
    t0: float;
    tend: float;
    mutable data: (float * float) list
  }

  let sampling ?(strategy=Sampler.strategy_midpoint)
      ?(criterion=Sampler.criterion_none) ?(min_step=1E-9) ?(nsamples=100)
      f a b =
    { strategy = strategy;
      criterion = criterion;
      min_step = min_step;
      nsamples = nsamples;
      fct = f;
      t0 = a;
      tend = b;
      data = [] }

  let x ?pathstyle vp sampling =
    ()

  let xy ?fill ?fillcolor ?pathstyle vp sampling =
    ()

  let fill ?fillcolor vp ?base sampling =
    ()
end
