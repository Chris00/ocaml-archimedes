open Testing
module V = Viewport

let draw backend =
  let vp = V.init ~w ~h ~dirs backend in
  let vps = V.layout_columns vp 2 in
  let vp1 = vps.(0) and vp2 = vps.(1) in

  V.set_color vp1 Color.red;
  let x = [| 1.; 2.; 3.; -1.; 0. |] in
  Plot.Array.x ~pathstyle:(Plot.Linespoints "+") vp1 x;

  let x = Bigarray.Array1.create Bigarray.float64 Bigarray.fortran_layout 20 in
  for i = 1 to Bigarray.Array1.dim x do
    x.{i} <- 3. *. sin (float i *. 0.1);
  done;
  Plot.Fortran.x vp2 x;

  Axes.box vp1;
  Axes.box vp2;

  V.close vp
