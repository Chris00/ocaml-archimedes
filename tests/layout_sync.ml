include Testing

module V = Archimedes.Viewport
module P = Archimedes.Path
module Pl = Archimedes.Plot.Function
module Axes = Archimedes.Axes
module Tics = Archimedes.Tics

let draw bk =
  let vp = Archimedes.init ~w:1024. ~h:768. ~dirs bk in
  let subvps = V.layout_grid ~syncs:(true, true, false, true) vp 2 2 in
  V.rectangle subvps.(0) ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke subvps.(0) V.Data;
(*  V.xrange subvps.(0) (-10.) 10.;
  V.yrange subvps.(0) (-3.) 5.;
  V.xrange subvps.(1) (-5.) 5.;
  V.yrange subvps.(1) (-1.) 5.;*)
  let sampling = Pl.sampling (fun x -> x *. x) 0. 5. in
  Pl.x subvps.(0) sampling;
  Axes.cross ~tics:(Tics.Equidistants (Tics.Number 5, 0., 3., 1)) subvps.(0);
  V.set_color subvps.(1) Archimedes.Color.red;
  Axes.cross ~tics:(Tics.Equidistants (Tics.Number 5, 0., 1., 1)) subvps.(1);
  V.set_color subvps.(2) Archimedes.Color.blue;
  Axes.cross ~tics:(Tics.Equidistants (Tics.Number 5, 0., 1.5, 1)) subvps.(2);
  V.set_color subvps.(3) Archimedes.Color.green;
  Axes.cross ~tics:(Tics.Equidistants (Tics.Number 5, 0., 2.5, 1)) subvps.(3);
  Archimedes.close vp
