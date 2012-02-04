include Tests_common

module V = Archimedes.Viewport

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let p = Archimedes.Path.make() in
  Archimedes.Path.rectangle p 0. 0. 1. 1.;
  V.stroke vp `Data p;
  V.xrange vp (-10.) 10.;
  V.yrange vp (-3.) 5.;
  V.set_line_width vp 1.;
  Archimedes.Axes.cross vp;
  Archimedes.close vp
