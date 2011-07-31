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
  | None
  | CustomColors of (string * Color.t) list
  | ValueDependant of (float -> Color.t)
  | LevelValueDependant of
      (int -> int -> float -> Color.t -> float -> Color.t)

type keyplacement =
  | Rectangle
  | OverPie
  | Outer

type keylabels =
  | Key
  | WithValues
  | WithProcents
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

let simple ?(style=Relief) ?(colorscheme=Default) ?(keyplacement=Rectangle)
    ?(keylabels=WithValues) ?(x0=0.) ?(y0=0.16) ?(xend=1.) ?(yend=1.)
    vp data =
  let center = (x0 +. xend) /. 2., (y0 +. yend) /. 2. in
  let centerx, centery = center in
  (* TODO we consider we have all the space to draw pie, we need to keep
     space for the key *)
  let radius = (min ((xend -. x0) /. 2.) ((yend -. y0) /. 2.)) in
  let sorted = List.sort (fun (_, x) (_, y) -> compare y x) data in
  let total = List.fold_left (fun acc (_, x) -> acc +. x) 0. sorted in
  let strokepath = Path.make () in
  let draw_simple_data (position, angle) (label, value) =
    let path = Path.make() in
    Path.move_to path centerx centery;
    let endangle = angle +. value /. total *. 2. *. pi in
    Path.line_to path
      (centerx +. radius *. cos angle) (centery +. radius *. sin angle);
    Path.arc path radius angle endangle;
    Path.append strokepath path;
    Viewport.set_color vp defaultcolors.(position);
    Viewport.fill ~path vp V.Graph;
    (position + 1, endangle)
  in
  let _, finalangle =
    List.fold_left draw_simple_data (0, 0.) sorted
  in
  Viewport.set_color vp Color.black;
  Viewport.stroke ~path:strokepath vp V.Graph;
  if abs_float (finalangle -. 2. *. pi) > 1E-8
  then Printf.printf "warning: large numerical error in pie chart"

  type multidata = {
    name: string;
    value: float;
    children: multidata list
  }

  let defaultmultilevelcolorscheme = LevelValueDependant (
    fun level position parentvalue parentcolor value ->
      if level = 0 then defaultcolors.(position)
      else if level < 0 then Color.white
      else parentcolor (* TODO lighten:
                          value = parentvalue => parentcolor,
                          (                gradient        ),
                          value = 0           => white     *)
  )

  let multilevel ?(style=Flat) ?(colorscheme=defaultmultilevelcolorscheme)
      ?(keyplacement=OverPie) ?(keylabels=Key)
      ?(x0=0.) ?(y0=0.16) ?(xend=1.) ?(yend=1.)
      vp multidata = ()
