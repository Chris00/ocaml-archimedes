open Testing

module V = Viewport
module P = Plot.Function

let draw bk =
  let vp = V.init ~w ~h ~dirs bk in
  V.set_mark_size vp 5.;
  V.set_line_width vp 1.;
  let f x = sqrt x in
  (* NOTE: when fill:true, with some value of nsamples, the fill has a bad
     point. This will be fixed with the new functions fill implementation
     (WIP). *)
  let sampling = P.sampling f (-10.) 10. in
  P.x vp sampling;
  Axes.box vp;
  V.close vp
