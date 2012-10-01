(* File: archimedes.ml

   Copyright (C) 2011

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

open Printf
include Archimedes_internals

let init = Viewport.init

let close = Viewport.close

let check_suffixes fname s1 s2 =
  Filename.check_suffix fname s1 || Filename.check_suffix fname s2

let backend_of_filename fname =
  if check_suffixes fname ".bmp" ".BMP" then
    ["graphics"; "BMP"; fname]
  else if check_suffixes fname ".png" ".PNG" then
    ["cairo"; "PNG"; fname]
  else if check_suffixes fname ".pdf" ".PDF" then
    ["cairo"; "PDF"; fname]
  else if check_suffixes fname ".ps" ".PS" then
    ["cairo"; "PS"; fname]
  else if check_suffixes fname ".tex" ".TEX" then
    ["tikz"; fname]
  else ["graphics"; "hold"]

let set_color = Viewport.set_color
let set_line_width = Viewport.set_line_width
let xrange = Viewport.xrange
let yrange = Viewport.yrange

let show = Viewport.show

(* Do it last because that will override the Array and List modules. *)
include Plot
