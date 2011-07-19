include Testing

module V = Viewport
module P = Plot.Function

let draw bk =
  let vp = V.init ~w ~h ~dirs bk in
  let vps = V.layout_grid vp 2 2 in
  let vp1 = vps.(0) and vp2 = vps.(3) in

  V.set_color vp1 Color.yellow;
  let sampling = P.sampling ~nsamples:50 sin (-5.) 5. in
  P.fill vp1 sampling;
  P.x ~pathstyle:(Plot.Boxes 0.08) vp1 sampling;
  V.set_color vp1 Color.black;
  Axes.box vp1;

  let sampling = P.sampling ~nsamples:30 sin (-5.) 5. in
  P.x ~pathstyle:(Plot.Interval 0.1) vp2 sampling;
  Axes.box vp2;

  Axes.box vp;

  let vp0 = vps.(1) in
  V.yrange vp0 (-2.) 2.;
  let sampling = P.sampling (fun x -> 1. /. x) (-1.) 1. in
  P.x vp0 sampling;
  Axes.box vp0;

  V.close vp
