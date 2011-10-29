include Tests_common
open Printf

module A = Archimedes

let draw bk =
  let neval = ref 0 in
  let f x = incr neval; x *. sin (1. /. x) in
  let neval_max = 400 in

  let vp = A.init ~w ~h ~dirs bk in
  A.Axes.box vp;
  A.Viewport.set_mark_size vp 1.;
  A.Viewport.set_color vp A.Color.dark_olive_green;
  A.fx vp f (-0.4) 0.4 ~n:neval_max ~style:(`Markers "o")
    ~fill:true ~fillcolor:A.Color.light_goldenrod;
  A.close vp;
  printf "  Number of function evaluations: %i (max: %i)\n" !neval neval_max
