include Tests_common
module A = Archimedes

let draw bk =
  let vp = A.init ~w ~h ~dirs bk in
  A.Viewport.set_ylog vp true;

  A.Axes.box ~tics:(A.Tics.Equidistants (A.Tics.Number 5, 0.1, 10., 5)) vp;
  A.fx vp (fun x -> x *. x) 0.1 10. ~style:(`Linesmarkers "o") ;

  A.close vp
