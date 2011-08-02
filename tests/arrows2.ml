include Tests_common

module Arrows = Archimedes.Arrows

let draw backend =
  let vp = Archimedes.init ~w ~h ~dirs backend in

  Archimedes.Viewport.set_line_width vp 1.;
  Arrows.line ~head:(Arrows.Unstyled) vp 0. 0.6 0.2 0.6;
  Arrows.line ~head:(Arrows.Simple) vp 0.3 0.6 0.5 0.6;
  Arrows.line ~head:(Arrows.Double) vp 0.6 0.6 0.8 0.6;
  Arrows.line ~head:(Arrows.Triple) vp 0. 0.3 0.2 0.3;
  Arrows.line ~head:(Arrows.Diamond) vp 0.3 0.3 0.5 0.3;
  Arrows.line ~head:(Arrows.Circle) vp 0.6 0.3 0.8 0.3;
  Arrows.line ~head:(Arrows.Stop) vp 0. 0.1 0.2 0.1;

  Archimedes.close vp
