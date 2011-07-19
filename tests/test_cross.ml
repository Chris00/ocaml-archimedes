include Testing

module V = Viewport

let draw bk =
  let vp = V.init ~w ~h ~dirs bk in
  V.rectangle vp 0. 0. 1. 1.;
  V.stroke vp V.Data;
  V.xrange vp (-10.) 10.;
  V.yrange vp (-3.) 5.;
  V.set_line_width vp 1.;
  Axes.cross vp;
  V.close vp
