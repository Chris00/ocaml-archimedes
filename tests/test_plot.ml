open Archimedes
module V = Viewport.Viewport
module P = Plot.Common

let draw bk =
  let vp = V.init ~w:1024. ~h:600. ~dirs:["../src"; "./src"] bk in
  V.xrange vp (-10.) 10.;
  V.yrange vp 0. 100.;
  P.fx ~pathstyle:(P.Linespoints "o") vp (fun x -> x ** 2.) (-10.) 10.;
  Axes.box vp;
  V.close vp

let () =
  List.iter draw [ "cairo PNG test_plot.png";
                   (*"graphics hold";
                     "tikz backend_path.tex"*)
                 ]


(*Local Variables:*)
(*compile-command: "ocamlopt -o test_axes.com -I ../src unix.cmxa dynlink.cmxa bigarray.cmxa archimedes.cmxa test_axes.ml && ocamlc -o test_axes.exe -I ../src unix.cma dynlink.cma bigarray.cma archimedes.cma test_axes.ml"*)
(*End:*)
