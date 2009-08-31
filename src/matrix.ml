(* File: matrix.ml

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

type t = matrix

exception Not_invertible

let make_identity () = { xx=1.; yx=0.; xy=0.; yy=1.; x0=0.; y0=0. }

let make_translate ~x ~y =
  { xx=1.; yx=0.; xy=0.; yy=1.;  x0=x;  y0=y }

let make_scale ~x ~y =
  { xx=x; yx=0.; xy=0.; yy=y;  x0=0.;  y0=0. }

let make_rotate ~angle =
  { xx=cos(angle);    yx=sin(angle);
    xy= -. sin(angle); yy=cos(angle);  x0=0.;  y0=0. }

let set_to_identity m =
  m.xx <- 1.; m.xy <- 0.; m.x0 <- 0.;
  m.yx <- 0.; m.yy <- 1.; m.y0 <- 0.

let translate m ~x ~y =
  m.x0 <- m.x0 +. m.xx *. x +. m.xy *. y;
  m.y0 <- m.y0 +. m.yx *. x +. m.yy *. y

let scale m ~x ~y =
  m.xx <- m.xx *. x;
  m.yx <- m.yx *. x;
  m.xy <- m.xy *. y;
  m.yy <- m.yy *. y

let rotate m ~angle =
  let cosa = cos angle and sina = sin angle in
  let xx = m.xx in
  m.xx <- xx *. cosa +. m.xy *. sina;
  m.xy <- m.xy *. cosa -. xx *. sina;
  let yx = m.yx in
  m.yx <- yx *. cosa +. m.yy *. sina;
  m.yy <- m.yy *. cosa -. yx *. sina

let invert m =
  (* Optimize for scaling|translation matrices which are the ones
     most used. *)
  if m.xy = 0. && m.yx = 0. then (
    m.x0 <- -. m.x0;
    m.y0 <- -. m.y0;
    if m.xx <> 1. then (
      if m.xx = 0. then raise Not_invertible;
      m.xx <- 1. /. m.xx;
      m.x0 <- m.x0 *. m.xx;
    );
    if m.yy <> 1. then (
      if m.yy = 0. then raise Not_invertible;
      m.yy <- 1. /. m.yy;
      m.y0 <- m.y0 *. m.yy;
    );
  )
  else
    let det = m.xx *. m.yy -. m.yx *. m.xy in
    if det = 0. || 1. /. det = 0. (* infinite det *) then
      raise Not_invertible;
    let yy = m.xx /. det in
    m.xx <- m.yy /. det;
    m.xy <- -. m.xy /. det;
    m.yx <- -. m.yx /. det;
    m.yy <- yy;
    let y0 = -. m.yx *. m.x0 -. yy *. m.y0 in
    m.x0 <- -. m.xx *. m.x0 -. m.xy *. m.y0;
    m.y0 <- y0


let det m = m.xx *. m.yy -. m.xy *. m.yx

let transform_distance m ~dx ~dy =
  (m.xx *. dx +. m.xy *. dy,  m.yx *. dx +. m.yy *. dy)

let transform_point m ~x ~y =
  (m.xx *. x +. m.xy *. y +. m.x0,  m.yx *. x +. m.yy *. y +. m.y0)

let blit m1 m2 =
  m2.xx <- m1.xx;
  m2.yx <- m1.yx;
  m2.xy <- m1.xy;
  m2.yy <- m1.yy;
  m2.x0 <- m1.x0;
  m2.y0 <- m1.y0

let copy t = { t with xx = t.xx }

let inv_transform_point m =
  let m_inv = copy m in
  invert m_inv;
  transform_point m_inv

let inv_transform_distance m =
  let m_inv = copy m in
  invert m_inv;
  transform_distance m_inv

let mul b a =
  { xx = b.xx *. a.xx +. b.xy *. a.yx;
    xy = b.xx *. a.xy +. b.xy *. a.yy;
    yx = b.yx *. a.xx +. b.yy *. a.yx;
    yy = b.yx *. a.xy +. b.yy *. a.yy;
    x0 = b.xx *. a.x0 +. b.xy *. a.y0 +. b.x0;
    y0 = b.yx *. a.x0 +. b.yy *. a.y0 +. b.y0; }

(* allow [c] to be [a] or [b]. *)
let mul_in c b a =
  let c_xx = b.xx *. a.xx +. b.xy *. a.yx in
  let c_xy = b.xx *. a.xy +. b.xy *. a.yy in
  let c_yx = b.yx *. a.xx +. b.yy *. a.yx in
  let c_yy = b.yx *. a.xy +. b.yy *. a.yy in
  let c_x0 = b.xx *. a.x0 +. b.xy *. a.y0 +. b.x0 in
  c.y0 <- b.yx *. a.x0 +. b.yy *. a.y0 +. b.y0;
  c.xx <- c_xx;
  c.xy <- c_xy;
  c.yx <- c_yx;
  c.yy <- c_yy;
  c.x0 <- c_x0

let has_shear t =
  t.yx <> 0. || t.xy <> 0.
