include Tests_common
module A = Archimedes

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  A.Axes.box vp;
  A.Viewport.xlabel vp "axe des X";
  A.Viewport.ylabel vp "axe des Y";
  A.Viewport.title vp "Title of the plot";
  A.Viewport.set_color vp A.Color.burlywood;
  A.Viewport.set_line_width vp 3.;
  A.fx vp atan (-5.) 5.;
  Archimedes.close vp

