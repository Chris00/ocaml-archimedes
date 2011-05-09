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

module type COMMON = sig
  type fill = In | Out
  type filledcurves = Color.t * Color.t (* f1 > f2, f2 < f1 *)

  val f : V.t -> ?nsamples:int -> ?fill:fill -> (float -> float) ->
    float -> float -> unit

  val xy_param : V.t -> ?nsamples:int -> ?fill:fill ->
    (float -> float * float) -> float -> float -> unit

  (* TODO we want to have control over the stroke properties for each curve *)
  val filledcurves : V.t -> ?nsamples:int -> ?fill:filledcurves ->
    (float -> float) -> (float -> float) -> float -> float -> unit
end

module Common =
struct

  let f vp ?(nsamples=100) ?fill f a b =
    let logarithmic = vp.V.axes_system.Axes.x.Axes.log in
    let step = if logarithmic then (b /. a) ** (1. /. nsamples)
    else (b -. a) /. nsamples in




  let plot_f vp ?nsamples ?(fill=false) f a b fill0 fill1 =
    let do_with, finish =
      if fill then
        let x_last = ref nan
        and y_last = ref nan in
        let first = ref true in
        ((fun p (x,y) ->
            if !first then (
              let x, y = fill0 x y in
              V.move_to p x y;
              first := false
            )
            else
              V.line_to p x y;
            x_last := x;
            y_last := y),
         (fun p ->
            let x, y = fill1 !x_last !y_last in
            V.line_to p x y;
            V.fill p;
            first := true; (* this set of functions may be run several times *)
         ))
      else
        let first = ref true in
        ((fun p (x,y) ->
            if !first then (
              V.move_to p x y;
              first := false
            )
            else
              V.line_to p x y),
         (fun p ->
            V.stroke p;
            first := true)) in
    V.xyf p.h ?color ?nsamples ~do_with ~finish f a b;
    (* Add marks if requested *)
    match mark with
    | None -> ()
    | Some mark ->
        let do_with p (x,y) =
          V.move_to p x y;
          V.render p mark
        and finish _ = () in
        V.xyf p.h ?color ?nsamples ~do_with ~finish f a b
  ;;

  let id x y = (x, y)
  let xyf p ?color ?nsamples ?mark ?(fill=false) f a b =
    plot_f p ?color ?nsamples ?mark ~fill f a b id id

  let pr_x x _ = (x, 0.)
  let f p ?color ?nsamples ?mark ?(fill=false) f a b =
    plot_f p ?color ?nsamples ?mark ~fill (fun x -> (x, f x)) a b pr_x pr_x

  let set_font_size p w = V.set_global_font_size p.h w
  let text p ?(rotate=0.) ~x ~y ?(pos=CC) txt =
    V.show_text p.h ~rotate ~x ~y pos txt
end

(************************************************************************)

module Array =
struct
  include Common

  let x p ?color ?mark ?(n0=0) x =
    let n = Array.init (Array.length x) (fun i -> float(n0 + i)) in
    let iter = Iterator.of_arrays n x in
    (*draw_axes p;*)
    V.xy p.h ?color ?mark iter

  let xy p ?color ?mark x y =
    let iter = Iterator.of_arrays x y in
    (*draw_axes p;*)
    V.xy p.h ?color ?mark iter
end

module L = List
module List =
struct
  include Common

  let x p ?color ?mark ?(n0=0) x =
    let i = ref (n0-1) in
    let n = List.map (fun _ -> incr i; float(!i)) x in
    let iter = Iterator.of_lists n x in
    (*draw_axes p;*)
    V.xy p.h ?color ?mark iter

  let xy p ?color ?mark x y =
    let iter = Iterator.of_lists x y in
    (*draw_axes p;*)
    V.xy p.h ?color ?mark iter
end

module Generic =
struct
  include Common

  (* Ugly, temporary solution *)
  let x p ?color ?mark ?(n0=0) iter data =
    let x = ref [] in
    iter (fun xi -> x := xi :: !x) data;
    List.x p ?color ?mark ~n0 (L.rev !x)

  let xy p ?color ?mark iter data =
    let x = ref [] in
    let y = ref [] in
    iter (fun xi yi -> x := xi :: !x; y := yi :: !y) data;
    List.xy p ?color ?mark (L.rev !x) (L.rev !y)
end

module Fortran =
struct
  include Common
  type vec = (float, float64_elt, fortran_layout) Array1.t

  let x p ?color ?mark ?(n0=1) x =
    let dim = Array1.dim x in
    let n = Array1.create float64 fortran_layout dim in
    for i = 1 to dim do
      n.{i} <- float(n0 + i);
    done;
    let iter = Iterator.of_bigarrays n x ~xclayout:false ~yclayout:false in
    (*draw_axes p;*)
    V.xy p.h ?color ?mark iter

  let xy p ?color ?mark x y =
    let iter = Iterator.of_bigarrays x y ~xclayout:false ~yclayout:false in
    (*draw_axes p;*)
    V.xy p.h ?color ?mark iter
end

module C =
struct
  include Common
  type vec = (float, float64_elt, c_layout) Array1.t

  let x p ?color ?mark ?(n0=1) x =
    let dim = Array1.dim x in
    let n = Array1.create float64 c_layout dim in
    for i = 1 to dim do
      n.{i} <- float(n0 + i);
    done;
    let iter = Iterator.of_bigarrays n x ~xclayout:true ~yclayout:true in
    (*draw_axes p;*)
    V.xy p.h ?color ?mark iter

  let xy p ?color ?mark x y =
    let iter = Iterator.of_bigarrays x y ~xclayout:true ~yclayout:true in
    (*draw_axes p;*)
    V.xy p.h ?color ?mark iter

end
