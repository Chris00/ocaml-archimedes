include Testing

module V = Archimedes.Viewport
module P = Archimedes.Plot.Function

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let vps = V.layout_grid vp 2 2 in
  let vp1 = vps.(0) and vp2 = vps.(3) in

  V.set_color vp1 Archimedes.Color.yellow;
  let sampling = P.sampling ~nsamples:50 sin (-5.) 5. in
  P.fill vp1 sampling;
  P.x vp1 sampling ~pathstyle:(Archimedes.Plot.Boxes 0.08);
  V.set_color vp1 Archimedes.Color.black;
  Archimedes.Axes.box vp1;

  let sampling = P.sampling ~nsamples:30 sin (-5.) 5. in
  P.x vp2 sampling ~pathstyle:(Archimedes.Plot.Interval 0.1);
  Archimedes.Axes.box vp2;

  Archimedes.Axes.box vp;

  let vp0 = vps.(1) in
  V.yrange vp0 (-2.) 2.;
  let sampling = P.sampling (fun x -> 1. /. x) (-1.) 1. in
  P.x vp0 sampling;
  Archimedes.Axes.box vp0;

  Archimedes.close vp
