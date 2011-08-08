include Tests_common
module A = Archimedes
module V = A.Viewport

let draw bk =
  let vp = A.init ~w ~h ~dirs bk in
  V.rectangle vp 0. 0. 1. 1.;
  V.stroke vp `Data;
  V.xrange vp 0. 10.;
  V.yrange vp 0. 5.;
  V.set_line_width vp 1.;
  A.Axes.x vp ~major:("|", 4.2)
    ~tics:(A.Tics.Equidistants (A.Tics.Number 5, 0., 3., 2))
    ~offset:(A.Axes.Relative 2.);
  A.Axes.y vp ~tics:(A.Tics.Equidistants (A.Tics.Number 5, 0., 3., 1));
  V.set_rel_font_size vp 16.;

  A.close vp
