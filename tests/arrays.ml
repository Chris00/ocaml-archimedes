include Testing
module V = Archimedes.Viewport
module P = Archimedes.Plot

let draw backend =
  let vp = Archimedes.init ~w ~h ~dirs backend in
  let vps = V.layout_columns vp 2 in
  let vp1 = vps.(0) and vp2 = vps.(1) in

  V.set_color vp1 Archimedes.Color.red;
  let x = [| 1.; 2.; 3.; -1.; 0. |] in
  P.Array.x ~pathstyle:(P.Linespoints "+") vp1 x;

  let x = Bigarray.Array1.create Bigarray.float64 Bigarray.fortran_layout 20 in
  for i = 1 to Bigarray.Array1.dim x do
    x.{i} <- 3. *. sin (float i *. 0.1);
  done;
  P.Fortran.x vp2 x;

  Archimedes.Axes.box vp1;
  Archimedes.Axes.box vp2;

  Archimedes.close vp
