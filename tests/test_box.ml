open Testing
module V = Viewport

let draw bk =
  let vp = V.init ~w ~h ~dirs bk in
  V.rectangle vp 0. 0. 1. 1.;
  V.stroke vp V.Data;
  V.xrange vp 0. 10.;
  V.yrange vp 0. 5.;
  V.set_line_width vp 1.;
  Axes.box ~tics:(Tics.Equidistants (Tics.Number 5, 4.2, 1.5, 3)) vp;
  V.close vp
