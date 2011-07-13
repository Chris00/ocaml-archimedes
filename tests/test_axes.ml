open Testing
module V = Viewport

let draw bk =
  let vp = V.init ~w ~h ~dirs bk in
  V.rectangle vp 0. 0. 1. 1.;
  V.stroke vp V.Data;
  V.xrange vp 0. 10.;
  V.yrange vp 0. 5.;
  V.set_line_width vp 1.;
  Axes.add_x_axis ~major:("|", 4.2)
    ~tics:(Tics.Equidistants (Tics.Number 5, 0., 3., 2))
    ~offset:(Axes.Relative 2.) vp;
  Axes.add_y_axis ~tics:(Tics.Equidistants (Tics.Number 5, 0., 3., 1)) vp;
  V.set_rel_font_size vp 16.;

  V.close vp
