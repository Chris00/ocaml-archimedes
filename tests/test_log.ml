open Archimedes
module V = Viewport
module P = Path
module Pl = Plot.Array

let draw bk =
  let vp = V.init ~w:1024. ~h:600. ~dirs:["../src"; "./src"] bk in
  V.set_ylog vp true;

  (*let p = P.make () in
  P.rectangle p 1. 1. 100. 100.;
  V.stroke ~path:p vp V.Data;*)

(*  V.set_mark_size vp 3.;*)
  Axes.box ~tics:(Tics.Equidistants (Tics.Number 5, 0.1, 10., 5)) vp;
  Pl.fx ~pathstyle:(Pl.Linespoints "o") vp (fun x -> x *. x) 0.1 10.;


  V.close vp

let () =
  List.iter draw [ "cairo PNG test_log.png";
                   (*"graphics hold";
                     "tikz backend_path.tex"*)
                 ]


(*Local Variables:*)
(*compile-command: "ocamlopt -o test_axes.com -I ../src unix.cmxa dynlink.cmxa bigarray.cmxa archimedes.cmxa test_axes.ml && ocamlc -o test_axes.exe -I ../src unix.cma dynlink.cma bigarray.cma archimedes.cma test_axes.ml"*)
(*End:*)
