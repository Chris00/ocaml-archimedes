include Testing

module A = Archimedes
module V = A.Viewport

let draw bk =
  let vp = A.init ~w ~h ~dirs bk in
  A.Axes.box vp;
  V.set_mark_size vp 5.;
  V.set_line_width vp 1.;
  let f x = sqrt x in
  (* NOTE: when fill:true, with some value of nsamples, the fill has a bad
     point. This will be fixed with the new functions fill implementation
     (WIP). *)
  A.fx vp f (-10.) 10.;
  A.close vp
