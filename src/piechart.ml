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
(* TODO Check multilevel implementation *)
(* TODO implement key for multilevel *)
(* TODO There are warnings in multilevel implementation *)
(* FIXME Several errors are just printed out instead of being leveraged by
   any kind of exception mechanism (the idea is to let the user take another
   action if a problem of this kind occur) *)

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

type keyplacement = NoKey | Rectangle | OverPie | Outer | Selective of float

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

(* The default multi level color scheme. It takes the usual default colors
   at the center (level 0) elements, then the color attenuates depending on
   the subsequent values. This is more an example than a really useful
   setting. Perhaps we could find a better one ? *)
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
  | WithValues -> Printf.sprintf "%s (%g)" label value
  | WithPercents ->
    Printf.sprintf "%s (%.2f%%)" label (value /. parentvalue *. 100.)
  | OnlyValues -> string_of_float value
  | OnlyPercents -> string_of_float (value /. parentvalue *. 100.)
  | CustomLabels f -> f label value (value /. parentvalue *. 100.)

let label_width vp label_config total (name, value) =
  (V.text_extents vp (get_label label_config total (name, value))).Matrix.w

(* Computes the center and radius of the pie, depending on the key position *)
let rec extents_key vp x0 y0 xend yend total data label_config = function
  | NoKey | OverPie ->
    let center = 0.5 *. (x0 +. xend), 0.5 *. (y0 +. yend)
    and radius = 0.5 *. min (xend -. x0) (yend -. y0) in
    center, radius
  | Rectangle ->
    let getwidth = label_width vp label_config total in
    (* max label width and height *)
    let maxwidth = List.fold_left (fun acc x -> max acc (getwidth x)) 0. data in
    let maxheight = (V.text_extents vp "()").M.h in
    let maxwidth = maxwidth +. 2. *. maxheight in (* place for color box *)
    let center = 0.5 *. (x0 +. xend -. maxwidth), 0.5 *. (y0 +. yend)
    and radius = 0.5 *. min (xend -. x0 -. maxwidth) (yend -. y0) in
    center, radius
  | Outer ->
    let getwidth = label_width vp label_config total in
    let maxwidth = List.fold_left (fun acc x -> max acc (getwidth x)) 0. data in
    let maxheight = (V.text_extents vp "()").M.h in
    let center = 0.5 *. (x0 +. xend), 0.5 *. (y0 +. yend)
    and radius =
      min (0.5 *. (xend -. x0) -. maxwidth) (0.5 *. (yend -. y0) -. maxheight)
    in
    if radius < 1E-8 then
      warning "this viewport is too small or the font is too large";
    center, radius
  | Selective limitrad ->
    let limitvalue = limitrad *. total /. twopi in
    let outerdata = List.filter (fun (_, x) -> x < limitvalue) data in
    let outertotal = List.fold_left (fun acc (_, x) -> acc +. x) 0. outerdata in
    if outertotal > total /. 4. then
      extents_key vp x0 y0 xend yend total data label_config Outer
    else
      extents_key vp x0 y0 xend yend total data label_config Rectangle

(* Adjusts the center and radius of the pie, depending on the style *)
let extents_style style ((cx, cy), radius) = match style with
  | Highlight _ -> (cx, cy), 10. *. radius /. 11.
  | Flat -> (cx, cy), radius
  | Relief ->
    warning "relief style extents not yet implemented; badboxes may appear";
    (cx, cy), radius

(* Computes the path of an element of the pie. An element is a "triangle"
   in the case of a simple pie chart, or a truncated "triangle" in the case
   of a multi level pie. r1 is the inner radius, r2 the outer radius, (cx,
   cy) is the center of the pie. The other parameters are
   self-explanatory. *)
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

(* Draws a label on the pie according to the key placement. *)
let rec draw_label placement vp xend y0 cx cy r1 r2 angle_start angle color
    position label =
  match placement with
  | NoKey -> ()
  | Rectangle ->
    let h = (V.text_extents vp "()").M.h in
    let y = y0 +. h *. float position *. 1.2 in
    V.text vp ~coord:`Graph ~pos:Backend.LT (xend -. 2. *. h) y label;
    let path = Path.make () in
    P.rectangle path (xend -. h) y h h;
    let basecolor = V.get_color vp in
    V.set_color vp color;
    V.fill vp `Graph path;
    V.set_color vp basecolor;
    V.stroke vp `Graph path
  | OverPie ->
    let x = cx +. (r2 +. r1) /. 2. *. cos (angle_start +. angle /. 2.) in
    let y = cy +. (r2 +. r1) /. 2. *. sin (angle_start +. angle /. 2.) in
    let labelcolor = Color.highest_contrast_bw color in
    let basecolor = V.get_color vp in
    V.set_color vp labelcolor;
    V.text vp ~coord:`Graph ~pos:Backend.CC x y label;
    V.set_color vp basecolor;
  | Outer ->
    let angle = angle_start +. angle /. 2. in
    let x1 = cx +. (r2 +. r1) *. 2. /. 3. *. cos angle in
    let y1 = cy +. (r2 +. r1) *. 2. /. 3. *. sin angle in
    let x2 = cx +. (r2 *. 1.1) *. cos angle in
    let y2 = cy +. (r2 *. 1.1) *. sin angle in
    let x3, pos = match x2 with
      | x2 when x2 > cx -> x2 +. (r2 -. r1) /. 5., Backend.RC
      | x2 -> x2 -. (r2 -. r1) /. 5., Backend.LC
    in
    let path = P.make () in
    P.move_to path x1 y1;
    P.line_to path x2 y2;
    P.line_to path x3 y2;
    V.stroke vp `Graph path;
    V.text vp ~coord:`Graph ~pos x3 y2 label
  | Selective limit ->
    let p = if angle < limit then Outer else OverPie in
    draw_label p vp xend y0 cx cy r1 r2 angle_start angle color position label

let draw_quarter style vp cx cy r1 r2 angle_start angle color name =
  let path = get_path style cx cy r1 r2 angle_start angle name in
  let basecolor = V.get_color vp in
  V.set_color vp color;
  V.fill vp `Graph path;
  V.set_color vp basecolor;
  V.stroke vp `Graph path

let simple ?(style=Relief) ?(colorscheme=Default) ?(keyplacement=Rectangle)
    ?(keylabels=WithValues) ?(x0=0.) ?(y0=0.16) ?(xend=1.) ?(yend=1.)
    vp data =
  let sorted = List.sort (fun (_, x) (_, y) -> compare y x) data in
  let total = List.fold_left (fun acc (_, x) -> acc +. x) 0. sorted in
  let (centerx, centery), radius = extents_style style
    (extents_key vp x0 y0 xend yend total data keylabels keyplacement)
  in
  let raw = draw_quarter style vp centerx centery 0. radius in
  let _, finalangle = List.fold_left (fun (pos, angle_start) (name, value) ->
    let color = get_color colorscheme 0 pos total Color.white name value in
    let angle = value /. total *. twopi in
    raw angle_start angle color name;
    let label = get_label keylabels total (name, value) in
    draw_label keyplacement vp xend y0 centerx centery 0. radius angle_start
      angle color pos label;
    (pos + 1, angle_start +. angle)
  ) (0, 0.) sorted in
  if abs_float (finalangle -. twopi) > 1E-8
  then warning "large numerical error in pie chart"
(* FIXME: Cannot simply print things to indicate an error. *)

let rec depth cur = function
  | [] -> cur
  | hd :: tl -> max (depth (cur + 1) hd.children) (depth cur tl)

let multilevel ?(style=Flat) ?(colorscheme=defaultmultilevelcolorscheme)
    ?(keylabels=Label) ?(x0=0.) ?(y0=0.16) ?(xend=1.) ?(yend=1.)
    vp multidata =
  let total = List.fold_left (fun acc x -> acc +. x.value) 0. multidata in
  let (cx, cy), radius_total = extents_style style
    (extents_key vp x0 y0 xend yend total [] keylabels OverPie)
  in
  let radius_slice = radius_total /. float (depth 1 multidata + 1) in
  let rec f level parentvalue parentcolor (position, angle_start) angle element =
    (* Level 0 has twice the place allowed for other levels *)
    let r1 = float level *. radius_slice in
    let r1 = r1 +. if level = 0 then 0. else radius_slice in
    let r2 = r1 +. radius_slice in
    let r2 = r2 +. if level = 0 then radius_slice else 0. in
    let sorted =
      List.sort (fun x y -> compare x.value y.value) element.children
    in
    let color = get_color colorscheme level position parentvalue parentcolor
      element.name element.value in
    draw_quarter style vp cx cy r1 r2 angle_start angle color element.name;
    ignore (List.fold_left (fun pos_anglestart x ->
      let angle = x.value /. element.value *. angle in
      f (level + 1) element.value color pos_anglestart angle x)
              (0, angle_start) sorted);
    position + 1, angle_start +. angle
  in
  ignore (List.fold_left (fun pos_anglestart x ->
    let angle = x.value /. total *. twopi in
    f 0 total Color.white pos_anglestart angle x) (0, 0.) multidata)
