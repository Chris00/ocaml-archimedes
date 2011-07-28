include Testing

module A = Archimedes

let f x = x *. sin (1. /. x)

let draw bk =
  let vp = A.init ~w ~h ~dirs bk in
  A.Viewport.set_color vp A.Color.red;
  A.Viewport.set_mark_size vp 1.;
  A.fx ~nsamples:2 ~pathstyle:(A.Plot.Points "o") vp ~fill:true f (-0.4) 0.4;
  A.Viewport.set_color vp A.Color.black;
  A.Axes.box vp;
  A.close vp
