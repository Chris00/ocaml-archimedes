open Archimedes
module V = Viewport

let draw bk =
  let pi = atan 1. *. 4. in
  let vp = V.init ~w:1024. ~h:600. ~dirs:["../src"; "./src"] bk in
  V.set_line_width vp 1.;
  Arrows.line ~head:(Arrows.Simple) vp 1. 1. 3. 3.;
  Arrows.arc vp ~tail:Arrows.Simple 2. 2. 0.5 pi (pi /. 4.);
  V.xrange vp 0. 4.;
  V.yrange vp 0. 4.;
  Axes.box vp;
  V.close vp

let () =
  List.iter draw [ "cairo PNG test_arrows.png";
                   "graphics hold";
                   (*"tikz backend_path.tex"*)
                 ]


(*Local Variables:*)
(*compile-command: "ocamlopt -o test_axes.com -I ../src unix.cmxa dynlink.cmxa bigarray.cmxa archimedes.cmxa test_axes.ml && ocamlc -o test_axes.exe -I ../src unix.cma dynlink.cma bigarray.cma archimedes.cma test_axes.ml"*)
(*End:*)
