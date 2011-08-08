include Tests_common

module A = Archimedes
module P = Archimedes.Path
module Axes = Archimedes.Axes
module Tics = Archimedes.Tics

let draw bk =
  let vp = A.init ~w:1024. ~h:768. ~dirs bk in
  let subvp = A.Viewport.grid ~syncs:(true, true, false, true) vp 2 2 in
  A.Viewport.rectangle subvp.(0).(0) ~x:0. ~y:0. ~w:1. ~h:1.;
  A.Viewport.stroke subvp.(0).(0) `Data;
(*  A.Viewport.xrange subvp.(0).(0) (-10.) 10.;
  A.Viewport.yrange subvp.(0).(0) (-3.) 5.;
  A.Viewport.xrange subvp.(0).(1) (-5.) 5.;
  A.Viewport.yrange subvp.(0).(1) (-1.) 5.;*)
  A.fx subvp.(0).(0) (fun x -> x *. x) 0. 5.;
  Axes.cross ~tics:(Tics.Equidistants(Tics.Number 5, 0., 3., 1)) subvp.(0).(0);
  A.Viewport.set_color subvp.(0).(1) A.Color.red;
  Axes.cross ~tics:(Tics.Equidistants(Tics.Number 5, 0., 1., 1)) subvp.(0).(1);
  A.Viewport.set_color subvp.(1).(0) A.Color.blue;
  Axes.cross ~tics:(Tics.Equidistants(Tics.Number 5, 0., 1.5, 1)) subvp.(1).(0);
  A.Viewport.set_color subvp.(1).(1) A.Color.green;
  Axes.cross ~tics:(Tics.Equidistants(Tics.Number 5, 0., 2.5, 1)) subvp.(1).(1);
  A.close vp
