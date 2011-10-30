open Tests_common
open Bigarray
module A = Archimedes
module V = Archimedes.Viewport

let description = "Array and bigarray data"

let draw backend =
  let vp = A.init ~w ~h ~dirs backend in
  let vps = V.columns vp 2 in
  let vp1 = vps.(0) and vp2 = vps.(1) in

  let x = [| 1.; 2.; 3.; -1.; 0. |] in
  A.Axes.box vp1;
  V.set_color vp1 A.Color.green_yellow;
  A.Array.y vp1 x ~style:`Lines;
  V.set_color vp1 A.Color.firebrick;
  V.set_mark_size vp1 20.;
  A.Array.y vp1 x ~style:(`Markers "*");

  let n = 10 in
  let x = Array1.create float64 fortran_layout (2 * n + 1) in
  for i = 1 to Array1.dim x do
    x.{i} <- 1. /. sin(float(i - 1 - n) *. 0.1);
  done;
  A.Axes.box vp2;
  A.Vec.y vp2 x ~fill:true;

  A.close vp
