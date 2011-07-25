include Testing

module V = Archimedes.Viewport
module P = Archimedes.Plot.Function

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  V.set_mark_size vp 5.;
  V.set_line_width vp 1.;
  let g x = sin x in
  let f x = x in
  let sampling = P.sampling f (-10.) 10. in
  let sampling_base = P.sampling g (-10.) 10. in
  P.fill vp ~base:sampling_base sampling;
  P.x ~pathstyle:(Archimedes.Plot.Linespoints "o") vp sampling;
  Archimedes.Axes.box vp (*~tics:(Tics.Equidistants (Tics.Number 5, 0., 1., 2)) *);
  (*   P.fx (* ~g ~fill:true *) ~pathstyle:(P.Linespoints "o") vp (fun x -> x *. x) (-10.) 10.;
       Axes.cross ~tics:(Tics.Equidistants (Tics.Number 5, 0., 3., 1)) vp; *)
  Archimedes.close vp
