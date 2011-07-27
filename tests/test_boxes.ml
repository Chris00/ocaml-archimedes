include Testing

module V = Archimedes.Viewport
module P = Archimedes.Plot.Function

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let vps = V.grid vp 2 2 in

  V.set_color vps.(0).(0) Archimedes.Color.yellow;
  let sampling = P.sampling ~nsamples:50 sin (-5.) 5. in
  P.fill vps.(0).(0) sampling;
  P.x vps.(0).(0) sampling ~pathstyle:(Archimedes.Plot.Boxes 0.08);
  V.set_color vps.(0).(0) Archimedes.Color.black;
  Archimedes.Axes.box vps.(0).(0);

  let sampling = P.sampling ~nsamples:30 sin (-5.) 5. in
  P.x vps.(1).(1) sampling ~pathstyle:(Archimedes.Plot.Interval 0.1);
  Archimedes.Axes.box vps.(1).(1);

  Archimedes.Axes.box vp;

  V.yrange vps.(0).(1) (-2.) 2.;
  let sampling = P.sampling (fun x -> 1. /. x) (-1.) 1. in
  P.x vps.(0).(1) sampling;
  Archimedes.Axes.box vps.(0).(1);

  Archimedes.close vp
