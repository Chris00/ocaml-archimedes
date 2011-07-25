include Testing

module A = Archimedes

let f x = x *. sin (1. /. x)

let draw bk =
  let vp = A.init ~w ~h ~dirs bk in
  A.fx vp ~fill:true f (-0.4) 0.4;
  A.Axes.box vp;
  A.close vp
