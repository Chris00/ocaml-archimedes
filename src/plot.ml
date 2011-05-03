(* File: plot.ml

   Copyright (C) 2009

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

open Bigarray
module VP = Viewport

type line_cap = Backend.line_cap = BUTT | ROUND | SQUARE

type line_join = Backend.line_join = JOIN_MITER | JOIN_ROUND | JOIN_BEVEL

type text_position = Backend.text_position =
 CC | LC | RC | CT | CB | LT | LB | RT | RB

type slant = Backend.slant = Upright | Italic
type weight = Backend.weight = Normal | Bold

module type COMMON = sig
  val f : VP.t -> ?color:Color.t -> ?nsamples:int -> ?mark:string ->
    ?fill:bool -> (float -> float) -> float -> float -> unit

  val xyf : VP.t -> ?color:Color.t -> ?nsamples:int -> ?mark:string ->
    ?fill:bool -> (float -> float * float) -> float -> float -> unit

  val text : VP.t -> ?rotate:float -> x:float -> y:float ->
    ?pos:text_position -> string -> unit
end

module Common =
struct
  type viewport = Handle.viewport

  let plot_f p ?color ?nsamples ?mark ?(fill=false) f a b fill0 fill1 =
    draw_axes p;
    let do_with, finish =
      if fill then
        let x_last = ref nan
        and y_last = ref nan in
        let first = ref true in
        ((fun p (x,y) ->
            if !first then (
              let x, y = fill0 x y in
              Handle.move_to p x y;
              first := false
            )
            else
              Handle.line_to p x y;
            x_last := x;
            y_last := y),
         (fun p ->
            let x, y = fill1 !x_last !y_last in
            Handle.line_to p x y;
            Handle.fill p;
            first := true; (* this set of functions may be run several times *)
         ))
      else
        let first = ref true in
        ((fun p (x,y) ->
            if !first then (
              Handle.move_to p x y;
              first := false
            )
            else
              Handle.line_to p x y),
         (fun p ->
            Handle.stroke p;
            first := true)) in
    Handle.xyf p.h ?color ?nsamples ~do_with ~finish f a b;
    (* Add marks if requested *)
    match mark with
    | None -> ()
    | Some mark ->
        let do_with p (x,y) =
          Handle.move_to p x y;
          Handle.render p mark
        and finish _ = () in
        Handle.xyf p.h ?color ?nsamples ~do_with ~finish f a b
  ;;

  let id x y = (x, y)
  let xyf p ?color ?nsamples ?mark ?(fill=false) f a b =
    plot_f p ?color ?nsamples ?mark ~fill f a b id id

  let pr_x x _ = (x, 0.)
  let f p ?color ?nsamples ?mark ?(fill=false) f a b =
    plot_f p ?color ?nsamples ?mark ~fill (fun x -> (x, f x)) a b pr_x pr_x

  let set_font_size p w = Handle.set_global_font_size p.h w
  let text p ?(rotate=0.) ~x ~y ?(pos=CC) txt =
    Handle.show_text p.h ~rotate ~x ~y pos txt
end

(************************************************************************)

module Array =
struct
  include Common

  let x p ?color ?mark ?(n0=0) x =
    let n = Array.init (Array.length x) (fun i -> float(n0 + i)) in
    let iter = Iterator.of_arrays n x in
    (*draw_axes p;*)
    Handle.xy p.h ?color ?mark iter

  let xy p ?color ?mark x y =
    let iter = Iterator.of_arrays x y in
    (*draw_axes p;*)
    Handle.xy p.h ?color ?mark iter
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
    Handle.xy p.h ?color ?mark iter

  let xy p ?color ?mark x y =
    let iter = Iterator.of_lists x y in
    (*draw_axes p;*)
    Handle.xy p.h ?color ?mark iter
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
    Handle.xy p.h ?color ?mark iter

  let xy p ?color ?mark x y =
    let iter = Iterator.of_bigarrays x y ~xclayout:false ~yclayout:false in
    (*draw_axes p;*)
    Handle.xy p.h ?color ?mark iter
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
    Handle.xy p.h ?color ?mark iter

  let xy p ?color ?mark x y =
    let iter = Iterator.of_bigarrays x y ~xclayout:true ~yclayout:true in
    (*draw_axes p;*)
    Handle.xy p.h ?color ?mark iter

end
