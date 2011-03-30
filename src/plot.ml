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

(* FIXME: IMHO, Handle should eventually disappear and everything
   should be done in this module. *)
type t = { h: Handle.t;
           mutable axes_set: bool; (* whether the user set axes *)
         }


type line_cap = Backend.line_cap = BUTT | ROUND | SQUARE

type line_join = Backend.line_join = JOIN_MITER | JOIN_ROUND | JOIN_BEVEL

type text_position = Backend.text_position =
    CC | LC | RC | CT | CB | LT | LB | RT | RB

type slant = Backend.slant = Upright | Italic
type weight = Backend.weight = Normal | Bold

module type COMMON = sig
  val make : ?dirs:string list -> string -> float -> float -> t
  val close : t -> unit

  val set_line_width : t -> float -> unit
  val set_mark_size : t -> float -> unit
  val set_color : t -> Color.t -> unit

  val xrange : t -> float -> float -> unit
  val yrange : t -> float -> float -> unit

  val f : t -> ?color: Color.t -> ?nsamples: int -> ?mark:string -> ?fill:bool ->
    (float -> float) -> float -> float -> unit
  val xyf : t -> ?color: Color.t -> ?nsamples: int -> ?mark:string ->
    ?fill:bool ->
    (float -> float * float) -> float -> float -> unit

  val set_font_size : t -> float -> unit

  val text : t -> ?rotate:float ->
    x:float -> y:float -> ?pos:text_position -> string -> unit


  type viewport
  module Viewport : sig
    val make : t -> xmin:float -> xmax:float -> ymin:float -> ymax:float
      -> viewport
    val columns : t -> int -> viewport array
    val rows : t -> int -> viewport array
    val grid : t -> int -> int -> viewport array array
  end
  val viewport : viewport -> unit
end


module Common =
struct
  type viewport = Handle.viewport

  let make ?(dirs=[Conf.plugins_dir]) backend w h =
    let h = Handle.make ?dirs backend w h in
    if !Sys.interactive then Handle.immediate h true;
    Handle.set_color h (Color.rgb 0.8 0. 0.);
    { h = h;  axes_set = false }
  let close p = Handle.close p.h

  module Viewport = struct
    let make p ~xmin ~xmax ~ymin ~ymax =
      Handle.Viewport.make p.h ~xmin ~xmax ~ymin ~ymax
    let columns p n = Handle.Viewport.columns p.h n
    let rows p n = Handle.Viewport.rows p.h n
    let grid p n m = Handle.Viewport.grid p.h n m
  end
  let viewport = Handle.use

  (* Rationale: since we took care of having uniform decisions so that
     various viewports look coherent, one must offer functions that do
     not brake that effort. *)
  let set_line_width p w = Handle.set_global_line_width p.h w
  let set_mark_size p w = Handle.set_global_mark_size p.h w

  let set_color p c = Handle.set_color p.h c

  let xrange p x1 x2 = Handle.xrange p.h x1 x2
  let yrange p y1 y2 = Handle.yrange p.h y1 y2

  let draw_axes p =
    if p.axes_set then ( (* FIXME: todo *)
    )
    else (
      let x = Handle.make_xaxis (`P "|") `Number CB (`P "tic_up") `Auto_linear
      and y = Handle.make_yaxis (`P "-") `Number LC (`P "tic_left") `Auto_linear
      in
      let axes = Axes.make (`Rectangle(true,true)) x y in
      Handle.axes p.h axes
    )

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
