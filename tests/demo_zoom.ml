include Tests_common

module A = Archimedes
module V = A.Viewport

let draw bk =
  let vp = A.init ~w:1024. ~h:600. ~dirs bk in
  let vp_zoom = V.make vp V.Graph 0.5 1. 0.6 1. (fun _ _ _ -> () ) in
  A.Axes.x vp ~offset:(A.Axes.Relative 0.)
    ~tics:(A.Tics.Equidistants (A.Tics.Number 3, 0., pi, 1));
  A.Axes.y vp ~offset:(A.Axes.Relative 0.)
    ~tics:(A.Tics.Equidistants (A.Tics.Number 3, 0., 1., 1));
  A.Axes.x vp_zoom
    ~tics:(A.Tics.Equidistants (A.Tics.Number 3, 0., pi, 1));
  A.Axes.y vp_zoom;
  V.axes_ratio vp pi;
  V.sync_ratio vp_zoom vp;
  A.fx vp sin 0. 10.;
  V.yrange vp (-1.5) 2.;
  A.fx vp_zoom sin (9. *. pi /. 4.) (11. *. pi /. 4.);

  A.close vp
