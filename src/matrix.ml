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

exception Not_invertible

module Affine =
struct
  type affine = { mutable xx: float; mutable yx: float;
                  mutable xy: float; mutable yy: float;
                  mutable x0: float; mutable y0: float; }

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

  type rectangle = {
    x:float;
    y:float;
    w:float;
    h:float;
  }

  let transform_rectangle ?dist_basepoint t rect =
    let x',y' =
      match dist_basepoint with
        None -> transform_point t rect.x rect.y
      | Some true -> transform_distance t rect.x rect.y
      | Some false -> rect.x, rect.y
    in
    let wx, wy = transform_distance t rect.w 0.
    and hx, hy = transform_distance t 0. rect.h in
    let x = x' +. (min 0. wx) +. (min 0. hx) in
    let y = y' +. (min 0. wy) +. (min 0. hy) in
    let w = (abs_float wx) +. (abs_float hx) in
    let h = (abs_float wy) +. (abs_float hy) in
    {x = x; y = y; w = w; h = h}

  let inv_transform_rectangle ?dist_basepoint t =
    let t' = copy t in
    invert t';
    transform_rectangle ?dist_basepoint t'
end


module Homothety =
struct
  (* Unfortunately, we cannot use "private affine" because the values
     can then be coerced to "affine" and then mutated.  Moreover an
     affine transformation needs to be copied to an homothetic one
     since they are mutable.  Thus it is better to define ones own
     structure here. *)

  include Affine
  type t = affine

  let of_matrix m =
    if m.Affine.yx <> 0. then invalid_arg "Matrix.Homothety.of_matrix: yx <> 0.";
    if m.Affine.xy <> 0. then invalid_arg "Matrix.Homothety.of_matrix: xy <> 0.";
    (* Since [m] is mutable, one must copy it to ensure the invariant
       is preserved in the future. *)
    copy m

  let to_matrix = copy (* need to copy because of mutability *)

  (* Redefine [invert] -- and the functions that use it -- to be more
     efficient *)
  let invert m =
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
    )

  let inv_transform_point m =
    let m_inv = copy m in
    invert m_inv;
    transform_point m_inv

  let inv_transform_distance m =
    let m_inv = copy m in
    invert m_inv;
    transform_distance m_inv

  (* This is easier than for the affine case since rectangles remain
     rectangles after transformation. *)
  let transform_rectangle ?dist_basepoint t rect =
    let x',y' =
      match dist_basepoint with
        None -> transform_point t rect.Affine.x rect.Affine.y
      | Some true -> transform_distance t rect.Affine.x rect.Affine.y
      | Some false -> rect.Affine.x, rect.Affine.y
    in
    let w', h' = transform_distance t rect.Affine.w rect.Affine.h in
    let x, w = if w' >= 0. then x', w' else x' +. w', -. w'
    and y, h = if h' >= 0. then y', h' else y' +. h', -. h' in
    {Affine.x = x; y = y; w = w; h = h}

  let inv_transform_rectangle ?dist_basepoint t =
    let t' = copy t in
    invert t';
    transform_rectangle ?dist_basepoint t'

end

include Affine

type t = Affine.affine

external unsafe_to_homothety : t -> Homothety.t = "%identity"
external unsafe_of_homothety : Homothety.t -> t = "%identity"
