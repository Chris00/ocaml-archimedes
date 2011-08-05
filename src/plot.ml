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

open Printf
open Utils
module V = Viewport

type style =
[ `Lines
| `Points of string
| `Linespoints of string
| `Impulses
| `Boxes of float ]

let default_fillcolor = Color.white_smoke


(* Iterators -> arrays
 ***********************************************************************)
(* For maximum flexibility, the user will be able to pass an iterator
   for his particular datastructure.  The iterator will be executed
   once (per plot) and the values will be cached in arrays.  This is
   safer because the iterator may query a mutable data structure.
   This should also be faster as the values will be contiguous in
   memory. *)

let array_of_iterator1D iter =
  let i = ref 0 in
  let len = ref 128 in
  let y = ref(Array.make !len 0.) in
  iter (fun yi ->
    if !i >= !len then (
      (* Reallocate the array to a larger one. *)
      let new_len = (3 * !len) / 2 in
      let new_y = Array.make new_len 0. in
      Array.blit !y 0 new_y 0 !len;
      len := new_len;
      y := new_y
    );
    !y.(!i) <- yi;
    incr i;
  );
  !y, !i (* length *)

let array_of_iterator2D iter =
  let i = ref 0 in
  let len = ref 128 in
  let x = ref(Array.make !len 0.)
  and y = ref(Array.make !len 0.) in
  iter (fun xi yi ->
    if !i >= !len then (
    (* Reallocate the array to a larger one. *)
      let new_len = (3 * !len) / 2 in
      let new_x = Array.make new_len 0.
      and new_y = Array.make new_len 0. in
      Array.blit !x 0 new_x 0 !len;
      Array.blit !y 0 new_y 0 !len;
      len := new_len;
      x := new_x;
      y := new_y;
    );
    !x.(!i) <- xi;
    !y.(!i) <- yi;
    incr i;
  );
  !x, !y, !i (* length *)
;;

(* Array plotting functions
 ***********************************************************************)

module PlotArray =
struct
  type t = float array;;
  DEFINE MOD = "Archimedes.Array";;
  DEFINE CREATE(len) = Array.make len 0.;;
  DEFINE GET(m, i) = m.(i);;
  DEFINE SET(m, i, v) = m.(i) <- v;;
  DEFINE FIRST = 0;;
  DEFINE LAST(n) = n - 1;;
  DEFINE DIM(m) = Array.length m;;
  DEFINE COPY(m) = Array.copy m;;
  DEFINE LINE_OF_ARRAY(path, x, y, i0, i1) =
    Path.unsafe_line_of_array path x y i0 i1;;

  INCLUDE "src/plot_arr.ml"
end

(* Generic Plotting functions
 ***********************************************************************)

exception Enough_elements

let y vp ?base ?fill ?fillcolor ?style iter =
  let y, n = array_of_iterator1D iter in
  let x = PlotArray.index_array n in
  let base = match base with
    | None -> None
    | Some iterb ->
      (* We know how many elements we need, do not allocate more *)
      let b = Array.make n 0. in
      let i = ref 0 in
      try
        iterb (fun y ->
          b.(!i) <- y;
          incr i;
          if !i >= n then raise Enough_elements);
        invalid_arg "Archimedes.Plot.y: the base iterator does not provide \
          enough elements"
      with Enough_elements -> Some b in
  PlotArray.unsafe_y vp ?base ?fill ?fillcolor ?style x y n

let xy vp ?fill ?fillcolor ?style iter =
  let x, y, n = array_of_iterator2D iter in
  PlotArray.unsafe_xy vp ?fill ?fillcolor ?style x y n


module PlotList = struct
  (* Do we want to write special code not to copy the list into arrays
     (list being immutable)?  arrays will be faster but maybe it is a
     waste of memory? *)

  let y vp ?base ?fill ?fillcolor ?style yl =
    let yiter f = List.iter f yl in
    let base = match base with
      | None -> None
      | Some l -> Some(fun f -> List.iter f l) in
    y vp ?base ?fill ?fillcolor ?style yiter


  let rec iter2D xyl f =
    match xyl with
    | [] -> ()
    | (x, y) :: tl -> f x y; iter2D tl f

  let xy vp ?fill ?fillcolor ?style xyl =
    xy vp ?fill ?fillcolor ?style (iter2D xyl)
end


module Vec = struct
  open Bigarray
  type t = (float, float64_elt, fortran_layout) Array1.t
  ;;
  DEFINE MOD = "Archimedes.Vec";;
  DEFINE CREATE(len) = Array1.create float64 fortran_layout len;;
  DEFINE GET(m, i) = m.{i};;
  DEFINE SET(m, i, v) = m.{i} <- v;;
  DEFINE FIRST = 1;;
  DEFINE LAST(n) = n;;
  DEFINE DIM(m) = Array1.dim m;;
  DEFINE COPY(m) = ba_copy m;;
  DEFINE LINE_OF_ARRAY(path, x, y, i0, i1) =
    Path.unsafe_line_of_vec path x y i0 i1;;

  INCLUDE "src/plot_arr.ml"
end

module CVec = struct
  open Bigarray
  type t = (float, float64_elt, c_layout) Array1.t
  ;;
  DEFINE MOD = "Archimedes.CVec";;
  DEFINE CREATE(len) = Array1.create float64 c_layout len;;
  DEFINE GET(m, i) = m.{i};;
  DEFINE SET(m, i, v) = m.{i} <- v;;
  DEFINE FIRST = 0;;
  DEFINE LAST(n) = n - 1;;
  DEFINE DIM(m) = Array1.dim m;;
  DEFINE COPY(m) = ba_copy m;;
  DEFINE LINE_OF_ARRAY(path, x, y, i0, i1) =
    Path.unsafe_line_of_cvec path x y i0 i1;;

  INCLUDE "src/plot_arr.ml"
end


(* Functions
 ***********************************************************************)

(* FIXME: tlog to be removed and automatically detected from the
   viewport axes).  In case the viewport axes switch to logscale, a
   resampling should be done. *)
let fx vp ?tlog ?n ?strategy ?cost ?(style=`Lines) ?base
    ?(fill=false) ?(fillcolor=default_fillcolor) f a b =
  let x, y = Sampler.x ?tlog ?n ?strategy ?cost f a b in
  (* FIXME: this is similar to Array.x except that the base may have
     its own sampling. *)
  let path = Path.make () in
  Path.unsafe_line_of_array path x y 0 (Array.length x - 1);
  V.fit vp (Path.extents path);
  (* Fill *)
  if fill then (
    let path_fill = Path.copy path in
    (match base with
    | None ->
      Path.line_to path_fill b 0.;
      Path.line_to path_fill a 0.
    | Some g ->
      (* Notice that the sampling is in reversed order: *)
      let bx, by = Sampler.x ?tlog ?n ?strategy ?cost g b a in
      (* FIXME: fill_samplings needs to be rewritten and moved (along
         with this code) to Path. *)
      Path.unsafe_line_of_array path_fill bx by 0 (Array.length bx - 1)
    );
    Path.close path_fill;
    let color = V.get_color vp in
    V.set_color vp fillcolor;
    (* Do not fit on its extents, because we don't want to fit
       [base]. *)
    V.fill ~path:path_fill ~fit:false vp V.Data;
    V.set_color vp color;
  );
  (match style with
  | `Lines | `Linespoints _ -> V.stroke ~path vp V.Data
  | `Points _ -> ()); (* Do not usually make sense but convenient
                        so see which data points where chosen. *)
  PlotArray.draw_marks vp style x y (Array.length x)


let xyf vp ?tlog ?n ?strategy ?cost ?(style=`Lines) ?(fill=false)
    ?(fillcolor=default_fillcolor) f a b =
  let x, y = Sampler.xy ?tlog ?n ?strategy ?cost f a b in
  let n = Array.length x in
  PlotArray.unsafe_xy vp ~fill ~fillcolor ~style x y n




(* Avoid module name clash. *)
module List  = PlotList
module Array = PlotArray

(* Local Variables: *)
(* compile-command: "make -k -C .." *)
(* End: *)
