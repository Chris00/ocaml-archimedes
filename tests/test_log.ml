include Testing

module V = Viewport
module P = Path
module Pl = Plot.Function

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  V.set_ylog vp true;

  (*let p = P.make () in
  P.rectangle p 1. 1. 100. 100.;
  V.stroke ~path:p vp V.Data;*)

(*  V.set_mark_size vp 3.;*)
  print_string "test\n";
  Axes.box ~tics:(Tics.Equidistants (Tics.Number 5, 0.1, 10., 5)) vp;
  print_string "test\n";
  let sampling = Pl.sampling (fun x -> x *. x) 0.1 10. in
  print_string "test\n";
  Pl.x ~pathstyle:(Plot.Linespoints "o") vp sampling;
  print_string "test\n";


  Archimedes.close vp
