open Testing

module V = Viewport
module P = Plot.Array

let draw bk =
  let vp = V.init ~w ~h ~dirs bk in
  V.set_mark_size vp 5.;
  V.set_line_width vp 1.;
  let f t = (cos t, sin t) in
  P.xy_param ~fill:true ~pathstyle:(P.Linespoints "o") vp f 0. (2. *. pi);
  Axes.box ~tics:(Tics.Equidistants (Tics.Number 5, 0., 1., 2)) vp;
  V.close vp
