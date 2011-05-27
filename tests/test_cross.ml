open Archimedes
module V = Viewport

let draw bk =
  let vp = V.init ~w:1024. ~h:600. ~dirs:["../src"; "./src"] bk in
  V.rectangle vp 0. 0. 1. 1.;
  V.stroke vp V.Data;
  V.xrange vp (-10.) 10.;
  V.yrange vp (-3.) 5.;
  V.set_line_width vp 1.;
  Axes.cross vp;
  V.close vp

let () =
  List.iter draw [ "cairo PNG test_cross.png";
                   (*"graphics hold";
                     "tikz backend_path.tex"*)
                 ]


(*Local Variables:*)
(*compile-command: "ocamlopt -o test_axes.com -I ../src unix.cmxa dynlink.cmxa bigarray.cmxa archimedes.cmxa test_axes.ml && ocamlc -o test_axes.exe -I ../src unix.cma dynlink.cma bigarray.cma archimedes.cma test_axes.ml"*)
(*End:*)
