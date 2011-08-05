include Tests_common

module A = Archimedes
module V = A.Viewport

let draw bk =
  let vp = A.init ~w ~h ~dirs bk in
  V.set_mark_size vp 5.;
  V.set_line_width vp 1.;
  A.xyf vp (fun t -> (cos t, sin t)) 0. (2. *. pi)
    ~fill:true ~style:(`Linespoints "o");
  A.Axes.box vp ~tics:(A.Tics.Equidistants (A.Tics.Number 5, 0., 1., 2));
  A.close vp
