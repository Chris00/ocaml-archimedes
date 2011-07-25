include Testing
module V = Archimedes.Viewport

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  V.rectangle vp 0. 0. 1. 1.;
  V.stroke vp V.Data;
  V.xrange vp 0. 10.;
  V.yrange vp 0. 5.;
  V.set_line_width vp 1.;
  Archimedes.Axes.add_x_axis vp ~major:("|", 4.2)
    ~tics:(Archimedes.Tics.Equidistants (Archimedes.Tics.Number 5, 0., 3., 2))
    ~offset:(Archimedes.Axes.Relative 2.);
  Archimedes.Axes.add_y_axis vp
    ~tics:(Archimedes.Tics.Equidistants (Archimedes.Tics.Number 5, 0., 3., 1));
  V.set_rel_font_size vp 16.;

  Archimedes.close vp
