open Archimedes
module V = Viewport.Viewport
module P = Plot.Array

let pi = acos (-1.)

let draw bk =
  let vp = V.init ~w:1024. ~h:600. ~dirs:["../src"; "./src"] bk in
  V.set_mark_size vp 5.;
  V.set_line_width vp 1.;
  let f t = (cos t, sin t) in
  P.xy_param ~fill:true ~pathstyle:(P.Linespoints "o") vp f 0. (2. *. pi);
  Axes.box ~tics:(Tics.Equidistants (Tics.Number 5, 0., 1., 2)) vp;
  V.close vp

let () =
  List.iter draw [ "cairo PNG test_xy_param.png";
                   (*"graphics hold";
                     "tikz backend_path.tex"*)
                 ]


(*Local Variables:*)
(*compile-command: "ocamlopt -o test_axes.com -I ../src unix.cmxa dynlink.cmxa bigarray.cmxa archimedes.cmxa test_axes.ml && ocamlc -o test_axes.exe -I ../src unix.cma dynlink.cma bigarray.cma archimedes.cma test_axes.ml"*)
(*End:*)
