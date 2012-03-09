(* File: plot.ml

   Copyright (C) 2009-2011

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     Pierre Hauweele <pierre@hauweele.net>
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

(* TODO some comments would be useful ... *)
(* TODO implement the Relief style *)
(* TODO implement key *)
(* TODO Check multilevel implementation *)
(* TODO There are warnings in multilevel implementation *)

open Utils

module V = Viewport
module P = Path
module M = Matrix


let twopi = atan 1. *. 8.

type style = Flat | Highlight of string list | Relief

type colorscheme =
  | Default | Monochrome | Black | CustomColors of (string * Color.t) list
  | ValueDependant of (float -> Color.t)
  | LevelValueDependant of (int -> int -> float -> Color.t -> float -> Color.t)

type keyplacement = NoKey | Rectangle | OverPie | Outer

type keylabels =
  | Label | WithValues | WithPercents | OnlyValues | OnlyPercents
  | CustomLabels of (string -> float -> float -> string)

let defaultcolors =
  let open Color in
  [| yellow; red; blue; green; magenta; cyan; black; white |]

let default_color position =
  defaultcolors.(position mod Array.length defaultcolors)

type multidata = {
  name: string;
  value: float;
  children: multidata list
}

let defaultmultilevelcolorscheme = LevelValueDependant (
  fun level position parentvalue parentcolor value ->
    if level = 0 then default_color position
    else if level < 0 then Color.white
    else if parentvalue < 1E-8 then Color.white
    else Color.lighten parentcolor (1. -. value /. parentvalue)
)

let get_color scheme level position parentvalue parentcolor label value =
  match scheme with
  | Default -> default_color position
  | Monochrome -> Color.white
  | Black -> Color.darken Color.white (value /. parentvalue)
  | CustomColors l -> List.assoc label l
  | ValueDependant f -> f value
  | LevelValueDependant f -> f level position parentvalue parentcolor value

let get_label config parentvalue (label, value) = match config with
  | Label -> label
  | WithValues -> Printf.sprintf "%s (%f)" label value
  | WithPercents ->
    Printf.sprintf "%s (%.2g)" label (value /. parentvalue *. 100.)
  | OnlyValues -> string_of_float value
  | OnlyPercents -> string_of_float (value /. parentvalue *. 100.)
  | CustomLabels f -> f label value (value /. parentvalue *. 100.)

let label_width vp label_config total (name, value) =
  (V.text_extents vp (get_label label_config total (name, value))).Matrix.w

let extents_key vp x0 y0 xend yend total data label_config = function
  | NoKey | OverPie ->
    let center = 0.5 *. (x0 +. xend), 0.5 *. (y0 +. yend)
    and radius = 0.5 *. min (xend -. x0) (yend -. y0) in
    center, radius
  | Rectangle ->
    let getwidth = label_width vp label_config total in
    let maxwidth = List.fold_left (fun acc x -> max acc (getwidth x)) 0. data in
    let center = 0.5 *. (x0 +. xend -. maxwidth), 0.5 *. (y0 +. yend)
    and radius = 0.5 *. min (xend -. x0 -. maxwidth) (yend -. y0) in
    center, radius
  | Outer ->
    let getwidth = label_width vp label_config total in
    let maxwidth = List.fold_left (fun acc x -> max acc (getwidth x)) 0. data in
    let maxheight = (V.text_extents vp "gX").Matrix.h in
    let centerx = 0.5 *. (x0 +. xend) -. maxwidth
    and centery = 0.5 *. (y0 +. yend) -. maxheight
    and radius =
      min (0.5 *. (xend -. x0) -. maxwidth) (0.5 *. (yend -. y0) -. maxheight)
    in
    (centerx, centery), radius

let extents_style style ((cx, cy), radius) = match style with
  | Highlight _ -> (cx, cy), 10. *. radius /. 11.
  | Flat -> (cx, cy), radius
  | Relief ->
    warning "relief style extents not yet implemented; badboxes may appear";
    (cx, cy), radius

let rec get_path style cx cy r1 r2 angle_start angle name =
  match style with
  | Highlight l when List.mem name l ->
    let path = get_path Flat cx cy r1 r2 angle_start angle name in
    let tr = (r2 -. r1) /. 10. in
    let angle = angle_start +. angle /. 2. in
    P.transform (M.make_translate (tr *. cos angle) (tr *. sin angle)) path
  | Highlight _ | Flat ->
    let path = P.make () in
    P.move_to path r1 0.;
    P.line_to path r2 0.;
    P.arc path r2 0. angle;
    P.line_to path (cos angle *. r1) (sin angle *. r1);
    P.arc path r1 angle 0.;
    let path = P.transform (M.make_rotate angle_start) path in
    P.transform (M.make_translate cx cy) path
  | Relief ->
    warning "relief style not yet implemented; falling back to Flat style";
    get_path Flat cx cy r1 r2 angle_start angle name

let raw_flat style vp cx cy r1 r2 angle_start angle color name =
  let path = get_path style cx cy r1 r2 angle_start angle name in
  let basecolor = V.get_color vp in
  V.set_color vp color;
  V.fill vp `Graph path;
  V.set_color vp basecolor;
  V.stroke vp `Graph path;
  ()

let simple ?(style=Relief) ?(colorscheme=Default) ?(keyplacement=Rectangle)
    ?(keylabels=WithValues) ?(x0=0.) ?(y0=0.16) ?(xend=1.) ?(yend=1.)
    vp data =
  let sorted = List.sort (fun (_, x) (_, y) -> compare y x) data in
  let total = List.fold_left (fun acc (_, x) -> acc +. x) 0. sorted in
  let (centerx, centery), radius = extents_style style
    (extents_key vp x0 y0 xend yend total data keylabels keyplacement)
  in
  let raw = raw_flat style vp centerx centery 0. radius in
  let _, finalangle = List.fold_left (fun (pos, angle_start) (name, value) ->
    let color = get_color colorscheme 0 pos total Color.white name value in
    let angle = value /. total *. twopi in
    raw angle_start angle color name;
    (pos + 1, angle_start +. angle)
  ) (0, 0.) sorted in
  if abs_float (finalangle -. twopi) > 1E-8
  then warning "large numerical error in pie chart"
(* FIXME: Cannot simply print things to indicate an error. *)

let rec depth cur = function
  | [] -> cur
  | hd :: tl -> max (depth (cur + 1) hd.children) (depth cur tl)

let multilevel ?(style=Flat) ?(colorscheme=defaultmultilevelcolorscheme)
    ?(keyplacement=OverPie) ?(keylabels=Label)
    ?(x0=0.) ?(y0=0.16) ?(xend=1.) ?(yend=1.)
    vp multidata =
  let centerx = 0.5 *. (x0 +. xend)
  and centery = 0.5 *. (y0 +. yend) in
  let radius_total = min (0.5 *. (xend -. x0)) (0.5 *. (yend -. y0)) in
  let radius_slice = radius_total /. float (depth 1 multidata + 1) in
  let total = List.fold_left (fun acc x -> acc +. x.value) 0. multidata in
  let position = ref 0 in
  let rec f level parentvalue parentcolor angle_start angle element =
    (* Level 0 has twice the place allowed for other levels *)
    let radius_in = float level *. radius_slice in
    let radius_in = radius_in +. if level = 0 then 0. else radius_slice in
    let radius_out = radius_in +. radius_slice in
    let radius_out = radius_out +. if level = 0 then radius_slice else 0. in
    let sorted =
      List.sort (fun x y -> compare x.value y.value) element.children
    in
    let color = get_color colorscheme level !position parentvalue parentcolor
      element.name element.value
    in
    position := !position + 1;
    raw_flat style vp centerx centery radius_in radius_out angle_start angle color element.name;
    List.fold_left (fun angle_start x ->
      let angle = x.value /. element.value *. angle in
      f (level + 1) element.value color angle_start angle x) angle_start sorted;
    angle_start +. angle
  in
  List.fold_left (fun angle_start x ->
    let angle = x.value /. total *. twopi in
    f 0 total Color.white angle_start angle x) 0. multidata;
  ()
