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

open Utils
module V = Viewport

type style =
[ `Lines
| `Points of string
| `Linespoints of string
| `Impulses
| `Boxes of float
| `Interval of float ]

let default_fillcolor = Color.rgb 0.95 0.95 0.95

let rec draw_data ?(base=0.) style path (x, y) =
  if is_finite y then match style with
  | `Lines | `Linespoints _ -> Path.line_to path ~x ~y
  | `Impulses -> draw_data ~base (`Boxes 0.) path (x, y)
  | `Points _ -> ()
  | `Boxes w ->
      Path.rectangle path ~x:(x -. w *. 0.5) ~y:base ~w ~h:(y -. base)
  | `Interval size ->
      Path.move_to path ~x ~y:base;
      Arrows.path_line_to ~size ~head:Arrows.Stop ~tail:Arrows.Stop path x y
  else Path.close path

let close_data ?(base=0.) style path (x, y) =
  if is_finite y then match style with
  | `Lines | `Linespoints _ -> Path.line_to path ~x ~y
  | `Impulses | `Points _ | `Boxes _ | `Interval _ -> ()
  else Path.close path

let draw_point style vp (x, y) =
  if is_finite y then
    match style with
    | `Lines | `Impulses | `Boxes _ | `Interval _ -> ()
    | `Points m | `Linespoints m -> V.mark vp ~x ~y m

let fillpath vp path color =
  V.save vp;
  V.set_color vp color;
  V.fill ~path vp V.Data;
  V.restore vp

(** [advance_samplings swap_up_down] advance in samplings, doing what is
    told by [u_do] and [d_do], until a condition is satisfied and then
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
*)
let rec advance_samplings ~u_samples ~d_samples ~acc
    ~u_do ~d_do ~ux'' ~uy'' ~ux' ~uy' ~dx'' ~dy'' ~dx' ~dy'
    ~until ~next ~last swap_up_down =
  let advance_up_sample u_samples d_samples u_do d_do
      ux'' uy'' ux' uy' dx'' dy'' dx' dy' swap_up_down =
    match u_samples with
    | [] -> (* No more samples, thus no more region. *)
      last ~acc ~ux' ~uy' ~dx'' ~dy'' ~dx' ~dy' swap_up_down
    | (ux, uy) :: utl ->
      if until ~ux' ~uy' ~ux ~uy ~dx' ~dy' then begin
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
        not (is_nan uy') && not (is_nan uy) && not(is_nan dy') && ux >= dx')
      ~next:advance_in_region
      ~last:(fun ~acc:_ ~ux':_ ~uy':_ ~dx'':_ ~dy'':_ ~dx':_ ~dy':_ _ -> ())
      false
  and advance_in_region ~u_samples ~d_samples ~acc:_
      ~ux'' ~uy'' ~ux' ~uy' ~ux ~uy ~dx'' ~dy'' ~dx' ~dy' swap_up_down =
    let interp_uy = uy' +. (uy -. uy') /. (ux -. ux') *. (dx' -. ux') in
    let oriented_advance_samplings =
      let u_do acc fx' fy' =
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
      ~until:(fun ~ux':_ ~uy':_ ~ux:_ ~uy ~dx':_ ~dy':_ -> is_nan uy)
      ~next:close_region_and_advance
      ~last:close_region
      swap_up_down
  and close_region ~acc ~ux' ~uy' ~dx'' ~dy'' ~dx' ~dy' swap_up_down =
    (* Descend from [f] to [g]. *)
    let interp_dy = dy'' +. (dy' -. dy'') /. (dx' -. dx'') *. (ux' -. dx'') in
    if swap_up_down then begin
      Path.line_to path ux' interp_dy;
      Path.line_to path ux' uy';
    end else begin
      Path.line_to path ux' uy';
      Path.line_to path ux' interp_dy
    end;
    (* Draw the path of [g] on that region. *)
    List.iter (fun (x, y) -> Path.line_to path x y) acc;
    V.fill ~path vp V.Data;
    Path.clear path;
  and close_region_and_advance ~u_samples ~d_samples ~acc
      ~ux'' ~uy'' ~ux' ~uy' ~ux ~uy ~dx'' ~dy'' ~dx' ~dy' swap_up_down =
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

(* Factorizes the y function in most submodules (except Function) *)
let y ?(fill=false) ?(fillcolor=default_fillcolor) ?(style=`Lines)
    ?(base=Iterator.zero_iterator ()) vp iterator =
  let path = Path.make () in
  let closingpath = ref [] in
  let f p =
    let bx, by = Iterator.next base in
    draw_data style path ~base:by p;
    if fill then closingpath := (bx, by) :: !closingpath
  in
  let data_rev = Iterator.iter_cache f iterator in
  if fill then begin
    let path = Path.copy path in
    List.iter (close_data style path) !closingpath;
    fillpath vp path fillcolor
  end;
  V.stroke ~path vp V.Data;
  List.iter (draw_point style vp) data_rev

(* Factorizes the xy function in most submodules (except Function) *)
let xy ?(fill=false) ?(fillcolor=default_fillcolor) ?(style=`Lines)
    vp iterator =
  let path = Path.make () in
  let data_rev = Iterator.iter_cache (draw_data style path) iterator in
  if fill then fillpath vp path fillcolor;
  V.stroke ~path vp V.Data;
  List.iter (draw_point style vp) data_rev

(* Factorizes the stack function in most submodules (except Function) *)
(* TODO choose a better default list of colors / fillcolors *)
let stack ?(colors=[|Color.black|]) ?(fillcolors=[| |])
    ?(style=`Boxes 0.5) vp iterators =
  let m = Array.length iterators in
  let curx = ref neg_infinity in
  let curbase = ref 0. in
  let paths = Array.init m (fun _ -> Path.make ()) in
  let backpaths = Array.init (succ m) (fun _ -> []) in
  let stacking i iterator =
    let (x, y) = Iterator.next iterator in
    if i = 0 then curx := x else assert (!curx = x);
    draw_data ~base:(!curbase) style paths.(i) (x, y +. !curbase);
    backpaths.(i) <- (x, !curbase) :: backpaths.(i);
    curbase := !curbase +. y;
    if i = pred m then backpaths.(m) <- (x, !curbase) :: backpaths.(m)
  in
  try while true do curbase := 0.; Array.iteri stacking iterators done
  with Iterator.EOI ->
    V.save vp;
    let draw i path =
      if fillcolors <> [| |] then begin
        let path = Path.copy path in
        List.iter (close_data style path) backpaths.(i);
        fillpath vp path fillcolors.(i mod (Array.length fillcolors))
      end;
      V.set_color vp (colors.(i mod (Array.length colors)));
      V.stroke ~path vp V.Data;
      List.iter (draw_point style vp) backpaths.(succ i)
    in
    Array.iteri draw paths;
    V.restore vp

module Function = struct
  let draw_marks vp style x y =
    match style with
    | `Lines -> ()
    | `Points m | `Linespoints m ->
      for i = 0 to Array.length x - 1 do
        V.mark vp x.(i) y.(i) m
      done

  let x ?tlog ?n ?strategy ?cost ?(style=`Lines) ?base
      ?(fill=false) ?(fillcolor=default_fillcolor) vp f a b =
    (* FIXME: Implies we need to redo the sampling of we switch to log
       scales? *)
    let x, y = Sampler.x ?tlog ?n ?strategy ?cost f a b in
    let path = Path.make () in
    Path.unsafe_line_of_array path x y;
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
        Path.unsafe_line_of_array path_fill bx by
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
    draw_marks vp style x y

  (* FIXME: finish implementing *)
  let xy ?tlog ?n ?strategy ?cost ?(style=`Lines) ?(fill=false)
      ?(fillcolor=default_fillcolor) vp f a b =
    let path = Path.make () in
    let x, y = Sampler.xy ?tlog ?n ?strategy ?cost f a b in
    Path.unsafe_line_of_array path x y;
    V.fit vp (Path.extents path);
    if fill then (
      let path_closed = Path.copy path in (* will not copy x, y *)
      Path.close path_closed;
      V.fill ~path:path_closed vp V.Data;
    );
    (match style with
    | `Lines | `Linespoints _ -> V.stroke ~path vp V.Data
    | `Points _ -> ());
    draw_marks vp style x y

end


(* The following functions simplify the implementation of x, xy and stack
   in standard submodules *)
let basey transform ?base ?fill ?fillcolor ?style vp data =
  y ?fill ?fillcolor ?style ?base vp (transform data)
let basexy transform ?fill ?fillcolor ?style vp data =
  xy ?fill ?fillcolor ?style vp (transform data)
let basestack transform ?colors ?fillcolors ?style vp datas =
  stack ?colors ?fillcolors ?style vp (Array.map transform datas)

module type Common = sig
  type data
  type data2

  val y : ?base:data -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:style -> Viewport.t -> data -> unit

  val xy : ?fill:bool -> ?fillcolor:Color.t -> ?style:style ->
    Viewport.t -> data2 -> unit

  val stack : ?colors:(Color.t array) -> ?fillcolors:(Color.t array) ->
    ?style:style -> Viewport.t -> data array -> unit
end

module Array = struct
  type data = float array
  type data2 = (float * float) array

  let y ?base = match base with
    | None -> basey Iterator.of_array ?base:None
    | Some a -> basey Iterator.of_array ~base:(Iterator.of_array a)

  let xy = basexy Iterator.of_array2
  let stack = basestack Iterator.of_array
end

(* Avoid module name clash. *)
module List_ = struct
  type data = float list
  type data2 = (float * float) list

  let y ?base = match base with
    | None -> basey Iterator.of_list ?base:None
    | Some l -> basey Iterator.of_list ~base:(Iterator.of_list l)
  let xy = basexy Iterator.of_list2
  let stack = basestack Iterator.of_list
end

module Fortran = struct
  open Bigarray

  type data = (float, float64_elt, fortran_layout) Array1.t
  type data2 = (float, float64_elt, fortran_layout) Array2.t

  let y ?base = match base with
    | None -> basey Iterator.of_fortran ?base:None
    | Some b -> basey Iterator.of_fortran ~base:(Iterator.of_fortran b)
  let xy = basexy Iterator.of_fortran2
  let stack = basestack Iterator.of_fortran
end

module C = struct
  open Bigarray

  type data = (float, float64_elt, c_layout) Array1.t
  type data2 = (float, float64_elt, c_layout) Array2.t

  let base = Array1.create float64 c_layout 0

  let y ?base = match base with
    | None -> basey Iterator.of_c ?base:None
    | Some b ->  basey Iterator.of_c ~base:(Iterator.of_c b)
  let xy = basexy Iterator.of_c2
  let stack = basestack Iterator.of_c
end

(* Avoid module name clash. *)
module List = List_
