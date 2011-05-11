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

module V = Viewport.Viewport

module type Common = sig
  type pathstyle =
    | Lines
    | Points of string
    | Linespoints of string
    | Impulses
  type filledcurves = Color.t * Color.t (* f1 > f2, f2 < f1 *)

  val fx : ?xlog:bool -> ?ylog:bool -> ?min_step:float ->
    ?max_yrange:float -> ?nsamples:int ->
    ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    V.t -> (float -> float) -> float -> float -> unit

  val xy_param : ?min_step:float -> ?nsamples:int -> ?fill:fill ->
    ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    V.t -> (float -> float * float) -> float -> float -> unit

(* TODO we want to have control over the stroke properties for each curve *)
(*val filledcurves : V.t -> ?nsamples:int -> ?fill:filledcurves ->
  (float -> float) -> (float -> float) -> float -> float -> unit*)
end

module Common =
struct

  type pathstyle =
    | Lines
    | Points of string
    | Linespoints of string
    | Impulses
  type filledcurves = Color.t * Color.t (* f1 > f2, f2 < f1 *)

  let f_line_to vp (x, y) = V.line_to vp x y
  let f_finish vp = V.stroke vp V.Data

  let draw_data pathstyle path (x, y) = match pathstyle with
    | Lines -> Path.line_to path ~x ~y
    | Linespoints _ -> Path.line_to path ~x ~y
    | Impulses ->
        Path.move_to path ~x ~y:0.;
        Path.line_to path ~x ~y
    | Points _ -> ()

  let fx ?xlog ?ylog ?min_step ?max_yrange ?nsamples ?(fill=false)
      ?(fillcolor=Color.red) ?(pathstyle=Lines) vp f a b =
    let _, (ymin, ymax), data =
      Functions.samplefx ?xlog ?ylog ?nsamples ?min_step ?max_yrange f b a
    in
    V.auto_fit vp a ymin b ymax;
    let path = Path.make_at a (f a) in
    List.iter (draw_data pathstyle path) data;
    let pathcopy = Path.copy path in
    if fill then begin
      Path.line_to path b 0.;
      Path.line_to path a 0.;
      V.set_global_color vp fillcolor;
      V.fill ~path vp V.Data;
      V.set_global_color vp Color.black
    end;
    V.stroke ~path:pathcopy vp V.Data;
    (match pathstyle with
     | Linespoints m | Points m ->
         List.iter (fun (x, y) -> V.mark vp ~x ~y m) data
     | _ -> ())

  let xy_param ?min_step ?nsamples ?(fill=false) ?(fillcolor=Color.red)
      ?(pathstyle=Lines) vp f a b =
    let _, r, data = Functions.samplefxy ?nsamples ?min_step f b a in
    let x1 = r.Matrix.x and y1 = r.Matrix.y in
    let x2 = x1 +. r.Matrix.w and y2 = y1 +. r.Matrix.h in
    V.auto_fit vp x1 y1 x2 y2;
    let x, y = f a in
    let path = Path.make_at x y in
    List.iter (draw_data pathstyle path) data;
    let pathcopy = Path.copy path in
    if fill then begin
      V.set_global_color vp fillcolor;
      V.fill ~path vp V.Data;
      V.set_global_color vp Color.black
    end;
    V.stroke ~path:pathcopy vp V.Data;
    (match pathstyle with
     | Linespoints m | Points m ->
         List.iter (fun (x, y) -> V.mark vp ~x ~y m) data
     | _ -> ())

end

(************************************************************************)
(*
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
*)
