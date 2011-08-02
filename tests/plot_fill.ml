include Tests_common

module A = Archimedes
module V = A.Viewport

let draw bk =
  let vp = A.init ~w ~h ~dirs bk in
  V.set_mark_size vp 5.;
  V.set_line_width vp 1.;
  let g x = sin x in
  let f x = x in
  A.fx vp f (-10.) 10. ~style:(`Linespoints "o") ~fill:true ~base:g;
  A.Axes.box vp (*~tics:(Tics.Equidistants (Tics.Number 5, 0., 1., 2)) *);
  (*   P.fx (* ~g ~fill:true *) ~pathstyle:(P.Linespoints "o") vp (fun x -> x *. x) (-10.) 10.;
       Axes.cross ~tics:(Tics.Equidistants (Tics.Number 5, 0., 3., 1)) vp; *)
  A.close vp
