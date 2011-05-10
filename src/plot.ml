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
  type fill = In | Out
  type filledcurves = Color.t * Color.t (* f1 > f2, f2 < f1 *)

  val fx : ?min_step:float -> ?nsamples:int -> ?fill:fill ->
    ?pathstyle:pathstyle -> V.t -> (float -> float) -> float -> float -> unit

(*  val xy_param : V.t -> ?nsamples:int -> ?fill:fill ->
    (float -> float * float) -> float -> float -> unit

  (* TODO we want to have control over the stroke properties for each curve *)
  val filledcurves : V.t -> ?nsamples:int -> ?fill:filledcurves ->
    (float -> float) -> (float -> float) -> float -> float -> unit*)
end

module Common =
struct

  type pathstyle =
    | Lines
    | Points of string
    | Linespoints of string
    | Impulses
  type fill = In | Out
  type filledcurves = Color.t * Color.t (* f1 > f2, f2 < f1 *)

  let f_line_to vp (x, y) = V.line_to vp x y
  let f_finish vp = V.stroke vp V.Data

  let xyf ?nsamples ?min_step ?(do_with=f_line_to) ?(finish=f_finish)
      vp f a b =
    let _, (y0, y1), fct = Functions.samplefx f ?nsamples ?min_step b a in
    V.auto_fit vp a y0 b y1;
    let f () =
      fct (fun () -> do_with vp) ();
      finish vp
    in
    V.add_instruction f vp

  let fx ?min_step ?nsamples ?fill ?(pathstyle=Lines) vp f a b =
    let path = Path.make () in
    let draw x y = match pathstyle with
      | Lines -> Path.line_to path ~x ~y
      | Points m -> V.mark vp ~x ~y m
      | Linespoints m -> Path.line_to path ~x ~y; V.mark vp ~x ~y m
      | Impulses -> Path.move_to path ~x ~y:0.; Path.line_to path ~x ~y
    in
    let do_with, finish =
      (*if fill then
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
      else*)
        let first = ref true in
        let do_with vp (x, y) =
          if !first then begin
            Path.move_to path ~x ~y;
            first := false
          end
          else draw x y
        in
        let finish vp = V.stroke ~path vp V.Data; first := true in
        (do_with, finish)
    in
    xyf ?min_step ?nsamples ~do_with ~finish vp f a b
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
