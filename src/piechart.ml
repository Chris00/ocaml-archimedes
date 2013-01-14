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

let pi = atan 1. *. 4.

module V = Viewport

type style =
  | Flat
  | Separated
  | HighlightFlat
  | Relief

type colorscheme =
  | Default
  | Monochrome
  | Black
  | CustomColors of (string * Color.t) list
  | ValueDependant of (float -> Color.t)
  | LevelValueDependant of
      (int -> int -> float -> Color.t -> float -> Color.t)

type keyplacement =
  | NoKey
  | Rectangle
  | OverPie
  | Outer

type keylabels =
  | Key
  | WithValues
  | WithPercents
  | OnlyValues
  | OnlyPercents
  | CustomLabels of (string -> float -> float -> string)

let defaultcolors = [|
  Color.yellow;
  Color.red;
  Color.blue;
  Color.green;
  Color.magenta;
  Color.cyan;
  Color.black;
  Color.white
|]

type multidata = {
  name: string;
  value: float;
  children: multidata list
}

let defaultmultilevelcolorscheme = LevelValueDependant (
  fun level position parentvalue parentcolor value ->
    if level = 0 then defaultcolors.(position)
    else if level < 0 then Color.white
    else if parentvalue < 1E-8 then Color.white
    else Color.lighten parentcolor (1. -. value /. parentvalue)
)

let get_color scheme level position parentvalue parentcolor label value =
  match scheme with
  | Default -> defaultcolors.(position mod Array.length defaultcolors)
  | Monochrome -> Color.white
  | Black -> Color.darken Color.white (value /. parentvalue)
  | CustomColors l -> List.assoc label l
  | ValueDependant f -> f value
  | LevelValueDependant f -> f level position parentvalue parentcolor value

let get_label config parentvalue label value = match config with
  | Key -> label
  | WithValues -> Printf.sprintf "%s (%f)" label value
  | WithPercents ->
    Printf.sprintf "%s (%.2g)" label (value /. parentvalue *. 100.)
  | OnlyValues -> string_of_float value
  | OnlyPercents -> string_of_float (value /. parentvalue *. 100.)
  | CustomLabels f -> f label value (value /. parentvalue *. 100.)

let raw_flat vp centerx centery radius_in radius_out angle_start angle color =
  let transformation = Matrix.make_translate centerx centery in
  Matrix.rotate transformation angle_start;
  let path = Path.make () in
  Path.move_to path radius_in 0.;
  Path.line_to path radius_out 0.;
  Path.arc path radius_out 0. angle;
  Path.line_to path (cos angle *. radius_in) (sin angle *. radius_in);
  Path.arc path radius_in angle 0.;
  let finalpath = Path.transform transformation path in
  V.set_color vp color;
    V.fill vp `Graph finalpath;
  V.set_color vp Color.black;
  V.stroke vp `Graph finalpath;
  angle_start +. angle

let simple ?(style=Relief) ?(colorscheme=Default) ?(keyplacement=Rectangle)
    ?(keylabels=WithValues) ?(x0=0.) ?(y0=0.16) ?(xend=1.) ?(yend=1.)
    vp data =
  let centerx = 0.5 *. (x0 +. xend)
  and centery = 0.5 *. (y0 +. yend) in
  (* TODO we consider we have all the space to draw pie, we need to keep
     space for the key *)
  let radius = min (0.5 *. (xend -. x0)) (0.5 *. (yend -. y0)) in
  let sorted = List.sort (fun (_, x) (_, y) -> compare y x) data in
  let total = List.fold_left (fun acc (_, x) -> acc +. x) 0. sorted in
  let raw = raw_flat vp centerx centery 0. radius in
  let get_color = get_color colorscheme in
  let _, finalangle = List.fold_left (fun (pos, angle_start) (label, value) ->
    let color = get_color 0 pos total Color.white label value in
    let angle = value /. total *. 2. *. pi in
    (pos + 1, raw angle_start angle color)
  ) (0, 0.) sorted in
  if abs_float (finalangle -. 2. *. pi) > 1E-8
  then Printf.printf "warning: large numerical error in pie chart"
(* FIXME: Cannot simply print things to indicate an error. *)

let multilevel ?(style=Flat) ?(colorscheme=defaultmultilevelcolorscheme)
    ?(keyplacement=OverPie) ?(keylabels=Key)
    ?(x0=0.) ?(y0=0.16) ?(xend=1.) ?(yend=1.)
    vp multidata =
  let centerx = 0.5 *. (x0 +. xend)
  and centery = 0.5 *. (y0 +. yend) in
  let radius_out = min (0.5 *. (xend -. x0)) (0.5 *. (yend -. y0)) in
  let total = List.fold_left (fun acc x -> acc +. x.value) 0. multidata in
  let position = ref 0 in
  let rec f level parentvalue parentcolor angle_start angle radius_in element =
    let sorted =
      List.sort (fun x y -> compare x.value y.value) element.children
    in
    let radius =
      if sorted = [] then radius_out else (radius_in +. radius_out) /. 2.
    in
    let color = get_color colorscheme level !position parentvalue parentcolor
      element.name element.value
    in
    position := !position + 1;
    raw_flat vp centerx centery radius_in radius angle_start angle color;
    List.fold_left (fun angle_start x ->
      let angle = x.value /. element.value *. angle in
      f (level + 1) element.value color angle_start angle radius x) angle_start element.children;
    angle_start +. angle
  in
  List.fold_left (fun angle_start x ->
    let angle = x.value /. total *. 2. *. pi in
    f 0 total Color.white angle_start angle 0. x) 0. multidata;
  ()
