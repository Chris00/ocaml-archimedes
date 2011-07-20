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

module type Common = sig
  type pathstyle =
    | Lines
    | Points of string
    | Linespoints of string
    | Impulses
    | Boxes of float (* Width in Data coordinates (usually what we want) *)
    | Interval of float (* Width in Data coordinates (to be consistent) *)

  type filledcurves = Color.t * Color.t (* f1 > f2, f2 < f1 *)

  val fill_samplings : Viewport.t -> Color.t ->
    (float * float) list -> (float * float) list -> unit

  val fx : ?strategy:Sampler.strategy -> ?criterion:Sampler.criterion ->
    ?min_step:float -> ?nsamples:int ->
    ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    ?g:(float -> float) -> V.t -> (float -> float) -> float -> float -> unit

  val xy_param : ?min_step:float -> ?nsamples:int -> ?fill:bool ->
    ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    V.t -> (float -> float * float) -> float -> float -> unit
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

  let draw_data ?(base=0.) pathstyle path (x, y) =
    if not (Functions.is_nan y) && not (Functions.is_inf y) then
      match pathstyle with
      | Lines | Linespoints _ -> Path.line_to path ~x ~y
      | Impulses ->
        Path.move_to path ~x ~y:base;
        Path.line_to path ~x ~y
      | Points _ -> ()
      | Boxes w ->
        Path.rectangle path ~x:(x -. w *. 0.5) ~y:base ~w ~h:(y -. base)
      | Interval size ->
        Path.move_to path ~x ~y:base;
        Arrows.path_line_to ~size ~head:Arrows.Stop ~tail:Arrows.Stop path x y

  let close_data pathstyle path (x, y) =
    if not (Functions.is_nan y) && not (Functions.is_inf y) then
      match pathstyle with
      | Lines | Linespoints _ -> Path.line_to path ~x ~y
      | Impulses | Points _ | Boxes _ | Interval _ -> ()

  let draw_point pathstyle vp (x, y) =
    if not (Functions.is_nan y) && not (Functions.is_inf y) then
      match pathstyle with
      | Lines | Impulses | Boxes _ | Interval _ -> ()
      | Points m | Linespoints m -> V.mark vp ~x ~y m

  let fill_data fillcolor pathstyle vp path iter =
    let path = Path.copy path in
    Iterator.iter (close_data pathstyle path) iter;
    V.save vp;
    V.set_color vp fillcolor;
    V.fill ~path vp V.Data;
    V.restore vp

  (** [advance_samplings swap_up_down] advance in samplings, doing what is
      told by [f_do] and [g_do], until a condition is satisfied and then
      call [next] or stop when there won't be anymore fill regions and
      finally call [last] in that case.

      [swap_up_down] tells wheter or not [f] and [g] samples are swapped
      (used in recursive calls).

      In the following, "up" and "down" refer to the order of advancement,
      not to the vertical alignment. The "up" samples are the samples in
      which we're advancing, and "down" are the fixed ones.

      [uy'] sample means y coordinate of the last "up" sample.

      That roughly ressemble that :
      ----------------(ux', uy')=advance=>(ux,uy)--------------
      ---(dx', dy')---------------------------------(dx, dy)---

      @param until condition for going to [next] advancing scheme. Is
      called as [until uy' uy dy]. *)
  let rec advance_samplings ~u_samples ~d_samples ~acc
      ~u_do ~d_do ~ux'' ~uy'' ~ux' ~uy' ~dx'' ~dy'' ~dx' ~dy'
      ~until ~next ~last swap_up_down =
    Printf.printf "advance_samplings ~ux':%f ~uy':%f ~dx':%f ~dy':%f  swap:%b\n%!" ux' uy' dx' dy' swap_up_down;
    let advance_up_sample u_samples d_samples u_do d_do
        ux'' uy'' ux' uy' dx'' dy'' dx' dy' swap_up_down =
      match u_samples with
      | [] -> (* No more samples, thus no more region. *)
        last ~acc ~ux' ~uy' ~dx'' ~dy'' ~dx' ~dy' swap_up_down
      | (ux, uy) :: utl ->
        Printf.printf "advance_up_sample ux':%f uy':%f ux:%f uy:%f dx':%f dy':%f swap:%b\n%!" ux' uy' ux uy dx' dy' swap_up_down;
        if until ~ux' ~uy' ~ux ~uy ~dx' ~dy' then begin
          Printf.printf "until verified.\n%!";
          next ~u_samples ~d_samples ~acc
            ~ux'' ~uy'' ~ux' ~uy' ~ux ~uy ~dx'' ~dy'' ~dx' ~dy' swap_up_down
        end else begin
          let acc = u_do acc ux' uy' in
          advance_samplings ~u_samples:utl ~d_samples ~acc
            ~u_do ~d_do
            ~ux'':ux' ~uy'':uy' ~ux':ux ~uy':uy
            ~dx'' ~dy'' ~dx' ~dy'
            ~until ~next ~last swap_up_down
        end
    in
    if ux' < dx' then
      advance_up_sample u_samples d_samples u_do d_do
        ux'' uy'' ux' uy' dx'' dy'' dx' dy' swap_up_down
    else
      advance_up_sample d_samples u_samples d_do u_do
        dx'' dy'' dx' dy' ux'' uy'' ux' uy' (not swap_up_down)

  let fill_samplings vp fillcolor f_samples g_samples =
    let path = Path.make () in
    let rec advance_out_of_region ~u_samples ~d_samples ~acc:_
        ~ux'' ~uy'' ~ux' ~uy' ~ux ~uy ~dx'' ~dy'' ~dx' ~dy' swap_up_down =
      Printf.printf "===Advance out of region\n%!";
      let oriented_advance_samplings =
        if swap_up_down then
          advance_samplings
            ~u_samples:d_samples ~d_samples:u_samples ~acc:[]
            ~u_do:(fun _ _ _ -> []) ~d_do:(fun _ _ _-> [])
            ~ux'':dx'' ~uy'':dy'' ~ux':dx' ~uy':dy'
            ~dx'':ux'' ~dy'':uy'' ~dx':ux' ~dy':uy'
        else
          advance_samplings
            ~u_samples ~d_samples ~acc:[]
            ~u_do:(fun _ _ _ -> []) ~d_do:(fun _ _ _-> [])
            ~ux'' ~uy'' ~ux' ~uy'
            ~dx'' ~dy'' ~dx' ~dy'
      in
      oriented_advance_samplings
        ~until:(fun ~ux' ~uy' ~ux ~uy ~dx' ~dy' ->
          not (Functions.is_nan uy') && not (Functions.is_nan uy)
          && not (Functions.is_nan dy')
          && ux >= dx')
        ~next:advance_in_region
        ~last:(fun ~acc:_ ~ux':_ ~uy':_ ~dx'':_ ~dy'':_ ~dx':_ ~dy':_ _ -> ())
        false
    and advance_in_region ~u_samples ~d_samples ~acc:_
        ~ux'' ~uy'' ~ux' ~uy' ~ux ~uy ~dx'' ~dy'' ~dx' ~dy' swap_up_down =
      Printf.printf "===Advance in region\n%!";
      let interp_uy = uy' +. (uy -. uy') /. (ux -. ux') *. (dx' -. ux') in
      let oriented_advance_samplings =
        let u_do acc fx' fy' =
          Printf.printf "line_to %f %f\n%!" fx' fy';
          Path.line_to path fx' fy';
          acc
        and d_do acc gx' gy' =
          (gx', gy') :: acc
        in
        if swap_up_down then
          advance_samplings
            ~u_samples:d_samples ~d_samples:u_samples ~acc:[(dx', interp_uy)]
            ~u_do:d_do ~d_do:u_do
            ~ux'':dx'' ~uy'':dy'' ~ux':dx' ~uy':dy'
            ~dx'':ux' ~dy'':uy' ~dx':ux ~dy':uy
        else
          advance_samplings
            ~u_samples ~d_samples ~acc:[(dx', dy')]
            ~u_do ~d_do
            ~ux'' ~uy'' ~ux':dx' ~uy':interp_uy
            ~dx'' ~dy'' ~dx' ~dy'
      in
      oriented_advance_samplings
        ~until:(fun ~ux':_ ~uy':_ ~ux:_ ~uy ~dx':_ ~dy':_ -> Functions.is_nan uy)
        ~next:close_region_and_advance
        ~last:close_region
        swap_up_down
    and close_region ~acc ~ux' ~uy' ~dx'' ~dy'' ~dx' ~dy' swap_up_down =
      Printf.printf "===Close region\n%!";
      (* Descend from [f] to [g]. *)
      let interp_dy = dy'' +. (dy' -. dy'') /. (dx' -. dx'') *. (ux' -. dx'') in
      Printf.printf "interp_dy:%f  swap:%b\n%!" interp_dy swap_up_down;
      if swap_up_down then begin
        Printf.printf "continue to ux':%f interp_dy:%f\n%!" ux' interp_dy;
        Path.line_to path ux' interp_dy;
        Printf.printf "descend to ux':%f uy':%f\n%!" ux' uy';
        Path.line_to path ux' uy';
      end else begin
        Printf.printf "continue to ux':%f uy':%f\n%!" ux' uy';
        Path.line_to path ux' uy';
        Printf.printf "descend to ux':%f interp_dy:%f\n%!" ux' interp_dy;
        Path.line_to path ux' interp_dy
      end;
      (* Draw the path of [g] on that region. *)
      List.iter (fun (x, y) -> Path.line_to path x y) acc;
      V.fill ~path vp V.Data;
      Path.clear path;
    and close_region_and_advance ~u_samples ~d_samples ~acc
        ~ux'' ~uy'' ~ux' ~uy' ~ux ~uy ~dx'' ~dy'' ~dx' ~dy' swap_up_down =
      Printf.printf "===Found end of region in samples.\n%!";
      close_region ~acc ~ux' ~uy' ~dx'' ~dy'' ~dx' ~dy' swap_up_down;
      (* If we've called [close_region_and_advance], it's because we've
         reached and end of region. Therefore, we have to
         [advance_OUT_of_region]. *)
      advance_out_of_region ~u_samples ~d_samples ~acc:[]
        ~ux'' ~uy'' ~ux' ~uy' ~ux ~uy ~dx'' ~dy'' ~dx' ~dy' swap_up_down
    in
    V.save vp;
    V.set_color vp fillcolor;
    advance_out_of_region ~u_samples:f_samples ~d_samples:g_samples ~acc:[]
      ~ux'':neg_infinity ~uy'':nan ~ux':neg_infinity ~uy':nan
      ~ux:neg_infinity ~uy:nan
      ~dx'':neg_infinity ~dy'':nan ~dx':neg_infinity ~dy':nan false;
    V.restore vp

  let fx ?strategy ?criterion ?min_step ?nsamples ?(fill=false)
      ?(fillcolor=Color.red) ?(pathstyle=Lines) ?(g=fun _ -> 0.) vp f a b =
    let sampler =
      Iterator.of_function ~tlog:(V.xlog vp) ?min_step ?nsamples
        ?strategy ?criterion
    in
    let iter_f = sampler (fun x -> (x, f x)) a b in
    let miny = ref infinity and maxy = ref neg_infinity in
    let path = Path.make () in
    let rev_f_samples = Iterator.iter_cache
      (fun (fx, fy) ->
          miny := min fy !miny; maxy := max fy !maxy;
          draw_data pathstyle path ~base:(g fx) (fx, fy)) iter_f
    in
    V.auto_fit vp a !miny b !maxy;
    if fill then begin
      let iter_g = sampler (fun x -> (x, g x)) a b in
      let f_samples = List.rev rev_f_samples
      and g_samples = List.rev (Iterator.iter_cache (fun _ -> ()) iter_g) in
      List.iter (fun (x, y) -> Printf.printf "f_sample %f, %f\n%!" x y) f_samples;
      List.iter (fun (x, y) -> Printf.printf "g_sample %f, %f\n%!" x y) g_samples;
      fill_samplings vp fillcolor f_samples g_samples
    end;
    V.stroke ~path vp V.Data;
    List.iter (draw_point pathstyle vp) rev_f_samples

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
      V.set_color vp fillcolor;
      V.fill ~path vp V.Data;
      V.set_color vp Color.black
    end;
    V.stroke ~path:pathcopy vp V.Data;
    List.iter (draw_point pathstyle vp) data

end

(************************************************************************)

module Array =
struct
  include Common

  let x ?(fill=false) ?(fillcolor=Color.red) ?(pathstyle=Boxes 0.5) vp data =
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
     | Lines | Impulses | Boxes _ | Interval _ -> ())

  let xy ?(fill=false) ?(fillcolor=Color.red) ?(pathstyle=Boxes 0.5)
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
     | Lines | Impulses | Boxes _ | Interval _ -> ())

  let stack ?(color=[|Color.rgb 0.8 0.8 0.8|])
      ?(fillcolor=[|Color.rgb 0.95 0.95 0.95|])
      ?(pathstyle=Boxes 0.5) vp data =
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
        V.set_color vp fillcolor.(i mod (Array.length fillcolor));
        V.fill ~path:pathcopy vp V.Data;
      end;
      V.set_color vp color.(i mod (Array.length color));
      V.stroke ~path vp V.Data;
    done;
    V.restore vp

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
