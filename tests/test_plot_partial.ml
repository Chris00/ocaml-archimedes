open Archimedes
module V = Viewport
module P = Plot.Array

let draw bk =
  let vp = V.init ~w:1024. ~h:600. ~dirs:["../src"; "./src"] bk in
  V.set_mark_size vp 5.;
  V.set_line_width vp 1.;
  let f x = sqrt x in
  (* NOTE: when fill:true, with some value of nsamples, the fill has a bad
     point. This will be fixed with the new functions fill implementation
     (WIP). *)
  P.fx ~fill:false vp f (-10.) 10.;
  Axes.box vp;
  V.close vp

let () =
  List.iter draw [ "cairo PNG test_plot_partial.png";
                   (*"graphics hold";
                     "tikz backend_path.tex"*)
                 ]


(*Local Variables:*)
(*compile-command: "ocamlopt -o test_axes.com -I ../src unix.cmxa dynlink.cmxa bigarray.cmxa archimedes.cmxa test_axes.ml && ocamlc -o test_axes.exe -I ../src unix.cma dynlink.cma bigarray.cma archimedes.cma test_axes.ml"*)
(*End:*)
