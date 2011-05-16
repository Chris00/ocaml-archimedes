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
    | Boxes of float (* Width in Data coordinates (usually what we want) *)
    | Interval of float (* Width in Data coordinates (to be consistent) *)

  type filledcurves = Color.t * Color.t (* f1 > f2, f2 < f1 *)

  val fx : ?strategy:Sampler.strategy -> ?criterion:Sampler.criterion ->
    ?min_step:float -> ?max_yrange:float -> ?nsamples:int ->
    ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    ?g:(float -> float) -> V.t -> (float -> float) -> float -> float -> unit

  val xy_param : ?min_step:float -> ?nsamples:int -> ?fill:bool ->
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
    | Boxes of float
    | Interval of float

  type filledcurves = Color.t * Color.t (* f1 > f2, f2 < f1 *)

  let f_line_to vp (x, y) = V.line_to vp x y
  let f_finish vp = V.stroke vp V.Data

  let draw_data ?(base=0.) pathstyle path (x, y) = match pathstyle with
    | Lines -> Path.line_to path ~x ~y
    | Linespoints _ -> Path.line_to path ~x ~y
    | Impulses ->
        Path.move_to path ~x ~y:base;
        Path.line_to path ~x ~y
    | Points _ -> ()
    | Boxes f ->
        Path.move_to path ~x:(x +. f /. 2.) ~y:base;
        Path.line_to path ~x:(x +. f /. 2.) ~y;
        Path.line_to path ~x:(x -. f /. 2.) ~y;
        Path.line_to path ~x:(x -. f /. 2.) ~y:base
    | Interval size ->
        Path.move_to path ~x ~y:base;
        Arrows.path_line_to ~size ~head:Arrows.Stop ~tail:Arrows.Stop path x y

  let close_data pathstyle path (x, y) = match pathstyle with
    | Lines | Linespoints _ -> Path.line_to path ~x ~y
    | Impulses | Points _ | Boxes _ | Interval _ -> ()

  let draw_point pathstyle vp (x, y) = match pathstyle with
    | Lines | Impulses | Boxes _ | Interval _ -> ()
    | Points m | Linespoints m -> V.mark vp ~x ~y m

  let fx ?strategy ?criterion ?min_step ?max_yrange ?nsamples ?(fill=false)
      ?(fillcolor=Color.red) ?(pathstyle=Lines) ?(g=fun _ -> 0.) vp f a b =
    let sampler = Sampler.samplefxy ?strategy ?criterion ~tlog:(V.xlog vp) in
    let data, (_, _, ymin, ymax) = sampler (fun x -> x, f x +. g x) a b in
    V.auto_fit vp a ymin b ymax;
    let path = Path.make_at a (f a +. g a) in
    List.iter
      (fun (hx, hy) -> draw_data pathstyle path ~base:(g hx) (hx, hy)) data;
    if fill then begin
      let pathcopy = Path.copy path in
      Path.line_to pathcopy b (g b);
      let data_g, _ = sampler (fun x -> x, g x) b a in
      List.iter (close_data pathstyle pathcopy) data_g;
      V.save vp;
      V.set_global_color vp fillcolor;
      V.fill ~path:pathcopy vp V.Data;
      V.restore vp;
    end;
    V.stroke ~path vp V.Data;
    List.iter (draw_point pathstyle vp) data

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
    List.iter (draw_point pathstyle vp) data

end

(************************************************************************)

module Array =
struct
  include Common

  let x ?(fill=false) ?(fillcolor=Color.red) ?(pathstyle=Boxes 0.4) vp data =
    let ymin = ref data.(0)
    and ymax = ref data.(0) in
    let n = Array.length data in
    let path = Path.make_at 0. data.(0) in
    for x = 0 to n - 1 do
      let y = data.(x) in
      ymin := min !ymin y;
      ymax := max !ymax y;
      draw_data pathstyle path (float x, y)
    done;
    V.auto_fit vp 0. !ymin (float n) !ymax;
    if fill then begin
      let pathcopy = Path.copy path in
      Path.line_to pathcopy (float (n - 1)) 0.;
      Path.line_to pathcopy 0. 0.;
      let instruction () =
        let c = V.get_color vp in
        V.set_color_direct vp fillcolor ();
        V.fill_direct ~path:pathcopy vp V.Data ();
        V.set_color_direct vp c ()
      in
      V.add_instruction instruction vp;
    end;
    V.stroke ~path vp V.Data;
    (match pathstyle with
     | Linespoints m | Points m ->
         Array.iteri (fun i y -> V.mark vp ~x:(float i) ~y m) data
     | _ -> ())

  let xy ?(fill=false) ?(fillcolor=Color.red) ?(pathstyle=Boxes 0.4)
      vp data_x data_y =
    let n = Array.length data_x in
    assert (n = Array.length data_y);
    let xmin = ref data_x.(0)
    and xmax = ref data_x.(0)
    and ymin = ref data_y.(0)
    and ymax = ref data_y.(0) in
    let path = Path.make_at data_x.(0) data_y.(0) in
    for i = 0 to n - 1 do
      let x, y = data_x.(i), data_y.(i) in
      xmin := min !xmin x;
      xmax := max !xmax x;
      ymin := min !ymin y;
      ymax := max !ymax y;
      draw_data pathstyle path (x, y)
    done;
    V.auto_fit vp !xmin !ymin !xmax !ymax;
    if fill then begin
      let instruction () =
        let c = V.get_color vp in
        V.set_color_direct vp fillcolor ();
        V.fill_direct ~path vp V.Data ();
        V.set_color_direct vp c ()
      in
      V.add_instruction instruction vp;
    end;
    V.stroke ~path vp V.Data;
    (match pathstyle with
     | Linespoints m | Points m ->
         Array.iteri (fun i x -> V.mark vp ~x ~y:(data_y.(i)) m) data_x
     | _ -> ())

  let stack ?(color=[|Color.rgb 0.8 0.8 0.8|])
      ?(fillcolor=[|Color.rgb 0.95 0.95 0.95|])
      ?(pathstyle=Boxes 0.4) vp data =
    let m = Array.length data in
    let n = Array.length data.(0) in
    let xmax = float (n - 1) in
    let ymin = ref data.(0).(0)
    and ymax = ref data.(0).(0) in
    (* Stacking *)
    let curdata = Array.copy data.(0) in
    let stacking i j x =
      if x < 0. then begin
        print_string "Warning: stack containing negative values,\n";
        print_string "this may produce unexpected results\n"
      end;
      curdata.(j) <- if i = 0 then x else x +. curdata.(j);
      ymin := min !ymin curdata.(j);
      ymax := max !ymax curdata.(j);
      curdata.(j)
    in
    let data = Array.init m (fun i -> Array.mapi (stacking i) data.(i)) in
    V.auto_fit vp (-1.) !ymin (xmax +. 1.) (!ymax +. abs_float !ymax *. 0.02);
    (* Drawing *)
    V.save vp;
    for i = m - 1 downto 0 do
      let path = Path.make_at 0. data.(i).(0) in
      for j = 0 to n - 1 do
        let base = if i = 0 then 0. else data.(i-1).(j) in
        draw_data ~base pathstyle path (float j, data.(i).(j))
      done;
      if fillcolor != [||] then begin
        let pathcopy = Path.copy path in
        for j = n - 1 downto 0 do
          let y = if i = 0 then 0. else data.(i-1).(j) in
          close_data pathstyle pathcopy (float j, y)
        done;
        V.set_global_color vp fillcolor.(i mod (Array.length fillcolor));
        V.fill ~path:pathcopy vp V.Data;
      end;
      V.set_global_color vp color.(i mod (Array.length color));
      V.stroke ~path vp V.Data;
    done;
    V.restore vp (* TODO points *)

end

(*module L = List
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
