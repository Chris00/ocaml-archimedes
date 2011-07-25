(* File: functions.mli

   Copyright (C) 2009

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

val is_nan : float -> bool
val is_inf : float -> bool
val is_finite : float -> bool

val update_extents : Matrix.rectangle -> float -> float -> Matrix.rectangle

val samplefxy :
  (float -> float * float) ->
  ?min_step:float ->
  ?nsamples:int -> float -> float ->
  int * Matrix.rectangle * (float * float) list

val samplefx :
  ?xlog:bool -> ?ylog:bool ->
  ?min_step:float -> ?max_yrange:float ->
  ?nsamples:int -> (float -> float) -> float -> float ->
  int * (float * float) * (float * float) list

val fxy_list :
  (float -> float * float) ->
  ?min_step:float -> ?nsamples:int -> float -> float -> (float * float) list
(*
val fx_list :
  (float -> float) ->
  ?min_step:float -> ?nsamples:int -> float -> float -> float list
*)
val plotfxy :
  Backend.t ->
  (float -> float * float) -> ?nsamples:int -> float -> float -> unit

val plotfx :
  Backend.t -> (float -> float) -> ?nsamples:int -> float -> float -> unit

val stroke_plot :
  ?init:bool ->
  Backend.t -> (float -> float) -> ?nsamples:int -> float -> float -> unit

val stroke_plot_param :
  ?init:bool ->
  Backend.t ->
  (float -> float * float) -> ?nsamples:int -> float -> float -> unit

type extend =
  | NONE (**No extends, transparent color*)
  | PAD (**Takes the nearest color available*)
  | REPEAT (**Repeats the colors*)
  | REFLECT (**Reflects the colors*)

val color_level :
  (float -> float -> float) ->
  ?extend:extend ->
  xmin:float ->
  xmax:float ->
  ymin:float ->
  ymax:float ->
  float -> Color.t -> float -> Color.t -> float -> float -> Color.t
    (**[color_level f ~xmin ~xmax ~ymin ~ymax fmin cmin fmax cmax] makes a
       function whose domain is [0,1]^2 and image is colors, constructed
       upon f as follows:

       -Let [(t,u)] a point of [0,1]^2. We find [x], resp.  [y] as a
       convex combination of [xmin] and [xmax], resp. [ymin] and [ymax].

       -We then compute [f x y] and set [v] the number for which [f x y =
       v *. fmax +. (1.-.v) *. fmin].

       -If [v] is in the interval [0,1], then the function gives the
       convex combination of the colors [cmin] and [cmax] (formally, each
       component of the new color is computed as [v*.a +. (1.-.v)*.b],
       where [a] and [b] are respectively the components of [cmin] and
       [cmax]).

       -If not, the optional extent gives which color must be returned:
       *PAD (default) makes the function return [cmin] if [v < 0.] and
       [cmax] otherwise.
       *NONE makes the function return a transparent color.
       *REPEAT takes [v] "modulo 1" and finds the corresponding
       color as if [v] was actually in [0,1].
       *REFLECT  takes [v] "modulo 2" and finds the corresponding
       color, taking [v] or [2.-.v]; this corresponds to "reflecting the color"
       beyond the bounds [fmin] and [fmax].
    *)
