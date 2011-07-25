include Testing

module V = Archimedes.Viewport
module P = Archimedes.Plot.Function

let draw bk =
  let vp = Archimedes.init ~w:1024. ~h:600. ~dirs bk in
  let vp_zoom = V.make vp V.Graph 0.5 1. 0.6 1. (fun _ _ _ -> () ) in
  Archimedes.Axes.add_x_axis
    ~tics:(Archimedes.Tics.Equidistants (Archimedes.Tics.Number 3, 0., pi, 1))
    ~offset:(Archimedes.Axes.Relative 0.) vp;
  Archimedes.Axes.add_y_axis vp ~offset:(Archimedes.Axes.Relative 0.)
    ~tics:(Archimedes.Tics.Equidistants (Archimedes.Tics.Number 3, 0., 1., 1));
  Archimedes.Axes.add_x_axis vp_zoom
    ~tics:(Archimedes.Tics.Equidistants (Archimedes.Tics.Number 3, 0., pi, 1));
  Archimedes.Axes.add_y_axis vp_zoom;
  V.axes_ratio vp pi;
  V.sync_ratio vp_zoom vp;
  let sampling = P.sampling sin 0. 10. in
  P.x vp sampling;
  V.yrange vp (- 1.5) 2.;
  let sampling_zoom = P.sampling sin (9. *. pi /. 4.) (11. *. pi /. 4.) in
  P.x vp_zoom sampling_zoom;

  Archimedes.close vp
