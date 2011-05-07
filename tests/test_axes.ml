open Archimedes
module V = Viewport.Viewport

let draw bk =
  let vp = V.init ~w:1024. ~h:600. ~dirs:["../src"; "./src"] bk in
  V.rectangle vp 0. 0. 1. 1.;
  V.stroke vp V.Data;
  V.xrange vp 0. 10.;
  V.yrange vp 0. 5.;
  V.set_line_width vp 1.;
  V.add_x_axis ~major:("o", 4.2) vp;
  V.add_y_axis vp;
  V.set_rel_font_size vp 16.;
  V.draw_axes vp;

  V.close vp

let () =
  List.iter draw [ "cairo PNG test_axes.png";
                   (*"graphics hold";
                     "tikz backend_path.tex"*)
                 ]


(*Local Variables:*)
(*compile-command: "ocamlopt -o test_axes.com -I ../src unix.cmxa dynlink.cmxa bigarray.cmxa archimedes.cmxa test_axes.ml && ocamlc -o test_axes.exe -I ../src unix.cma dynlink.cma bigarray.cma archimedes.cma test_axes.ml"*)
(*End:*)
