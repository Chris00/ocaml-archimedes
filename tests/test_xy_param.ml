include Testing

module V = Archimedes.Viewport
module P = Archimedes.Plot.Function

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  V.set_mark_size vp 5.;
  V.set_line_width vp 1.;
  let f t = (cos t, sin t) in
  let sampling = P.sampling f 0. (2. *. pi) in
  P.xy vp sampling ~fill:true ~pathstyle:(Archimedes.Plot.Linespoints "o");
  Archimedes.Axes.box vp
    ~tics:(Archimedes.Tics.Equidistants (Archimedes.Tics.Number 5, 0., 1., 2));
  Archimedes.close vp
