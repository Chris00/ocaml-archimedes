include Tests_common

module V = Archimedes.Viewport
module Arrows = Archimedes.Arrows

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  V.set_line_width vp 1.;
  Arrows.line ~head:(Arrows.Simple) vp 1. 1. 3. 3.;
  Arrows.arc vp ~tail:Arrows.Simple 2. 2. 0.5 pi (pi /. 4.);
  Arrows.arc vp ~tail:Arrows.Simple 1. 1. 0.5 (pi /. 4.) pi;
  V.xrange vp 0. 4.;
  V.yrange vp 0. 4.;
  Archimedes.Axes.box vp;
  Archimedes.close vp
