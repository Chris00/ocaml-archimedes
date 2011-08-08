include Tests_common
module V = Archimedes.Viewport

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  V.rectangle vp 0. 0. 1. 1.;
  V.stroke vp `Data;
  V.xrange vp 0. 10.;
  V.yrange vp 0. 5.;
  V.set_line_width vp 1.;
  Archimedes.Axes.box vp
    ~tics:(Archimedes.Tics.Equidistants
             (Archimedes.Tics.Number 5, 4.2, 1.5, 3));
  Archimedes.close vp
