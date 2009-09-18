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


(* FIXME: IMHO, Handle should eventually disappear and everything
   should be done in this module. *)
type t = { h: Handle.t;
           mutable axes_set: bool; (* wether the user set exes *)
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

  val f : t -> ?color: Color.t -> ?nsamples: int -> ?mark:string ->
    (float -> float) -> float -> float -> unit
  val xyf : t -> ?color: Color.t -> ?nsamples: int -> ?mark:string ->
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


module Common : COMMON =
struct
  type viewport = Handle.viewport

  let make ?(dirs=[]) backend w h = {
    h = Handle.make ~dirs backend w h;
    axes_set = false }
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

  let f p ?color ?nsamples ?mark f a b =
    let do_with = match mark with
      | Some mark ->
          (fun p (x,y) ->
             Handle.render p mark;
             Handle.line_to p x y)
      | None -> (fun p (x,y) -> Handle.line_to p x y) in
    let finish = match mark with
      | Some mark -> (fun p ->
                       (* final point *)
                       Handle.render p mark;
                       Handle.stroke p)
      | None -> Handle.stroke in
    if p.axes_set then () (* FIXME: todo *)
    else (
      let x = Handle.make_xaxis (`P "|") `Number CB (`P "tic_up") `Auto_linear
      and y = Handle.make_yaxis (`P "-") `Number LC (`P "tic_left") `Auto_linear
      in
      let axes = Handle.make_axes (`Rectangle(true,true)) x y in
      (* FIXME: The ranges determination must be false *)
      let _, ranges, _ = Functions.samplefxy (fun t -> (t,f t)) ?nsamples b a in
      Handle.update_coords p.h ranges.Axes.xmin ranges.Axes.ymin;
      Handle.update_coords p.h ranges.Axes.xmax ranges.Axes.ymax;
      let r = { Axes.x1 = ranges.Axes.xmin; x2 = ranges.Axes.xmax;
                y1 = ranges.Axes.ymin; y2 = ranges.Axes.ymax } in
      ignore(Handle.print_axes p.h axes r)
    );
    Handle.f p.h ?color ?nsamples ~do_with ~finish f a b


  let xyf p ?color ?nsamples ?(mark="") f a b =
    let do_with h x y =
      Handle.render h mark;
      Handle.line_to h x y in
    let iter = Iterator.from_sampling f ?nsamples a b in
    Backend.save (Handle.backend p.h);
    (match color with
     | Some c -> Backend.set_color (Handle.backend p.h) c
     | None -> ());
    Handle.xy p.h ~do_with iter;
    Backend.restore (Handle.backend p.h)


  let set_font_size p w = Handle.set_global_font_size p.h w
  let text p ?(rotate=0.) ~x ~y ?(pos=CC) txt =
    Handle.show_text p.h ~rotate ~x ~y pos txt
end

(************************************************************************)
module Generic =
struct
  include Common

  let x p ?color ?(mark="") ?(n0=0) ~iter data =
    let n = ref n0 in
    let plot x =
      if !n = n0 then ( (* 1st point *)
        Handle.move_to p.h (float n0) x;
        Handle.render p.h mark;
      )
      else (
        Handle.line_to p.h (float !n) x;
        Handle.render p.h mark;
      );
      incr n;
    in
    iter plot data

  let xy p ?color ?(mark="") ~iter data =
    let first = ref true in
    let plot x y =
      if !first then (
        Handle.move_to p.h x y;
        Handle.render p.h mark;
        first := false;
      )
      else (
        Handle.line_to p.h x y;
        Handle.render p.h mark;
      );
    in
    iter plot data
end

module Array =
struct
  include Common

end

module List =
struct
  include Common

end

module Fortran =
struct
  include Common

end

module C =
struct
  include Common

end
