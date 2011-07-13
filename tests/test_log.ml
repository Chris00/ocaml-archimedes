open Testing

module V = Viewport
module P = Path
module Pl = Plot.Array

let draw bk =
  let vp = V.init ~w ~h ~dirs bk in
  V.set_ylog vp true;

  (*let p = P.make () in
  P.rectangle p 1. 1. 100. 100.;
  V.stroke ~path:p vp V.Data;*)

(*  V.set_mark_size vp 3.;*)
  Axes.box ~tics:(Tics.Equidistants (Tics.Number 5, 0.1, 10., 5)) vp;
  Pl.fx ~pathstyle:(Pl.Linespoints "o") vp (fun x -> x *. x) 0.1 10.;


  V.close vp
