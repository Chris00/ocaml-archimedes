open Testing

module V = Viewport
module P = Plot.Function

let draw bk =
  let vp = V.init ~w ~h ~dirs bk in
  V.set_mark_size vp 5.;
  V.set_line_width vp 1.;
  let f t = (cos t, sin t) in
  let sampling = P.sampling f 0. (2. *. pi) in
  P.xy ~fill:true ~pathstyle:(Plot.Linespoints "o") vp sampling;
  Axes.box ~tics:(Tics.Equidistants (Tics.Number 5, 0., 1., 2)) vp;
  V.close vp
