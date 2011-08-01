include Testing

module A = Archimedes
module V = A.Viewport

let draw bk =
  let vp = A.init ~w ~h ~dirs bk in
  let vps = V.grid vp 2 2 in

  A.Axes.box vps.(0).(0);
  A.fx vps.(0).(0) sin (-5.) 5. ~fill:true ~fillcolor:A.Color.yellow;

  A.Axes.box vps.(1).(1);
  V.set_color vps.(1).(1) A.Color.aquamarine;
  A.fx vps.(1).(1) sin (-5.) 5. ~pathstyle:(A.Plot.Interval 0.2);

  A.Axes.box vp;

  V.yrange vps.(0).(1) (-2.) 2.;
  A.Axes.box vps.(0).(1);
  V.set_color vps.(0).(1) A.Color.red;
  A.fx vps.(0).(1) (fun x -> 0.5 /. x) (-1.) 1.;

  A.close vp
