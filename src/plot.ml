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

(* Integer arrays
 ***********************************************************************)
(* When following a discrete structure, one must create an array of
   integer floats for the X axis.  These arrays being never modified,
   they can be shared by all functions. *)

let x_index = Array.init 1024 float

(* FIXME: allow offset? *)
let index_array n =
  if n < 1024 then x_index
  else Array.init n float (* FIXME: do we wan to replace x_index ?? *)


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
  let lines_y vp ~fill ?base ~fillcolor x y n =
    let path = Path.make() in
    Path.unsafe_line_of_array path x y 0 (n - 1);
    V.fit vp (Path.extents path);
    if fill then (
      let path_fill = Path.copy path in
      (match base with
      | None ->
        Path.line_to path (float(n - 1)) 0.;
        Path.line_to path 0. 0.
      | Some b ->
        if Array.length b <> n then
          invalid_arg "Archimedes.Plot.Array.y: wrong length for \"base\"";
        Path.unsafe_line_of_array path_fill x (Array.copy b) (n - 1) 0;
        V.fit vp (Path.extents path_fill); (* update for base *)
      );
      Path.close path_fill;
      let color = V.get_color vp in
      V.set_color vp fillcolor;
      V.fill ~path:path_fill vp V.Data ~fit:false;
      V.set_color vp color;
    );
    path

  let boxes vp ~fill ?base ~fillcolor x y n w =
    let path = Path.make() in
    (match base with
    | None ->
      for i = 0 to n - 1 do
        Path.rectangle path ~x:(x.(i) -. w *. 0.5) ~y:0. ~w ~h:y.(i)
      done
    | Some b ->
      if Array.length b <> n then
        invalid_arg "Archimedes.Plot.Array.y: wrong length for \"base\"";
      for i = 0 to n - 1 do
        Path.rectangle path
          ~x:(x.(i) -. w *. 0.5) ~y:b.(i) ~w ~h:y.(i)
      done);
    (* For boxes, one certainly wants to see everything *)
    V.fit vp (Path.extents path);
    if fill then (
      let color = V.get_color vp in
      V.set_color vp fillcolor;
      V.fill ~path vp V.Data ~fit:false;
      V.set_color vp color;
    );
    (* Draw (for boxes, marks do not make any sense). *)
    V.stroke ~path vp V.Data

  let draw_marks vp style x y n =
    match style with
    | `Lines | `Impulses | `Boxes _ -> ()
    | `Points m | `Linespoints m ->
      for i = 0 to n - 1 do
        V.mark vp x.(i) y.(i) m
      done

  let unsafe_y vp ?base ?(fill=false) ?(fillcolor=default_fillcolor)
      ?(style=`Points "O") x y n =
    match style with
    | `Lines ->
      let path = lines_y vp ~fill ?base ~fillcolor x y n in
      V.stroke ~path vp V.Data ~fit:false
    | `Points mark ->
      ignore(lines_y vp ~fill ?base ~fillcolor x y n);
      draw_marks vp style x y n
    | `Linespoints mark ->
      let path = lines_y vp ~fill ?base ~fillcolor x y n in
      V.stroke vp ~path V.Data ~fit:false;
      draw_marks vp style x y n
    | `Boxes w ->
      boxes vp ~fill ?base ~fillcolor x y n w
    | `Impulses ->
      boxes vp ~fill ?base ~fillcolor x y n 0.

  let y vp ?base ?fill ?fillcolor ?style ?(const=false) ydata =
    let y = if const then ydata else Array.copy ydata in
    let n = Array.length y in
    let x = index_array n in
    unsafe_y vp ?base ?fill ?fillcolor ?style x y n

  (* FIXME: better selection of default colors *)
  let default_fillcolors =
    [| default_fillcolor; Color.thistle; Color.misty_rose; Color.old_lace;
       Color.linen; Color.plum |]

  let stack vp ?colors ?(fill=true) ?(fillcolors=[| |])
      ?(style=`Boxes 0.5) yvecs =
    if Array.length yvecs > 0 then (
      let fillcolors =
        if Array.length fillcolors = 0 then default_fillcolors
        else fillcolors in
      let nc = Array.length fillcolors in
      let n = Array.length yvecs.(0) in
      let x = index_array n in
      let y0 = Array.copy yvecs.(0) in
      unsafe_y vp ~fill ~fillcolor:fillcolors.(0) ~style x y0 n;
      let base = Array.copy y0 in
      for i = 1 to Array.length yvecs - 1 do
        if Array.length yvecs.(i) < n then
          invalid_arg(sprintf "Archimedes.Array.stack: length yvec.(%i) < %i"
                        i n);
        let yi = Array.copy yvecs.(i) in
        let fillcolor = fillcolors.(i mod nc) in
        unsafe_y vp ~base:base ~fill ~fillcolor ~style x yi n;
        (* [base] is saved in the path, it can be overwritten  *)
        for i = 0 to n - 1 do base.(i) <- base.(i) +. yi.(i) done
      done
    )

  let lines_xy vp ~fill ~fillcolor x y n =
    let path = Path.make() in
    Path.unsafe_line_of_array path x y 0 (n - 1);
    V.fit vp (Path.extents path);
    if fill then (
      let path_fill = Path.copy path in
      Path.close path_fill;
      let color = V.get_color vp in
      V.set_color vp fillcolor;
      V.fill ~path:path_fill vp V.Data ~fit:false;
      V.set_color vp color;
    );
    path

  let unsafe_xy vp ?(fill=false) ?(fillcolor=default_fillcolor)
      ?(style=`Points "O") x y n =
    match style with
    | `Lines ->
      let path = lines_xy vp ~fill ~fillcolor x y n in
      V.stroke ~path vp V.Data ~fit:false
    | `Points mark ->
      ignore(lines_xy vp ~fill ~fillcolor x y n);
      draw_marks vp style x y n
    | `Linespoints mark ->
      let path = lines_xy vp ~fill ~fillcolor x y n in
      V.stroke vp ~path V.Data ~fit:false;
      draw_marks vp style x y n

  let xy vp ?fill ?fillcolor ?style
      ?(const_x=false) xdata ?(const_y=false) ydata =
    let n = Array.length xdata in
    if n <> Array.length ydata then
      invalid_arg "Archimedes.Array.xy: arrays do not have the same length";
    let x = if const_x then xdata else Array.copy xdata in
    let y = if const_y then ydata else Array.copy ydata in
    unsafe_xy vp ?fill ?fillcolor ?style x y n
end

(* Generic Plotting functions
 ***********************************************************************)

exception Enough_elements

let y vp ?base ?fill ?fillcolor ?style iter =
  let y, n = array_of_iterator1D iter in
  let x = index_array n in
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


module Fortran = struct
  open Bigarray
  type vec = (float, float64_elt, fortran_layout) Array1.t

  let x_fortran = Array1.create float64 fortran_layout 1024
  let () = for i = 1 to 1024 do x_fortran.{i} <- float i done

  let index n =
    if n <= 1024 then x_fortran
    else failwith "FIXME"


  let y vp ?base ?fill ?fillcolor ?style ?(const=false) ydata =
    let y = if const then ydata else ba_copy ydata in
    let n = Array1.dim y in
    let x = index n in
    failwith "FIXME"

  let xy vp ?fill ?fillcolor ?style
      ?(const_x=false) xdata ?(const_y=false) ydata =
    let n = Array1.dim xdata in
    if n <> Array1.dim ydata then
      invalid_arg "Archimedes.Fortran.xy: vectors do not have the same dim";
    let x = if const_x then xdata else ba_copy xdata in
    let y = if const_y then ydata else ba_copy ydata in
    failwith "FIXME"
end

module C = struct
  open Bigarray
  type vec = (float, float64_elt, c_layout) Array1.t


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
