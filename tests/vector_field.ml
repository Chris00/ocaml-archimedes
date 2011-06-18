open Archimedes
module V = Viewport

let draw bk =
  let f vp x y =
    Arrows.line ~head:Arrows.Simple vp x y (x -. y) (x +. y)
  in
  let vp = V.init ~w:1024. ~h:600. ~dirs:["../src"; "./src"] bk in
  V.set_line_width vp 1.;
  for x = -5 to 5 do
    for y = -5 to 5 do
      f vp (float x +. 0.5) (float y +. 0.5)
    done
  done;
  Axes.box vp;
  V.close vp

let () =
  List.iter draw [ "cairo PNG vector_field.png";
                   (*"graphics hold";
                     "tikz backend_path.tex"*)
                 ]


(*Local Variables:*)
(*compile-command: "ocamlopt -o test_axes.com -I ../src unix.cmxa dynlink.cmxa bigarray.cmxa archimedes.cmxa test_axes.ml && ocamlc -o test_axes.exe -I ../src unix.cma dynlink.cma bigarray.cma archimedes.cma test_axes.ml"*)
(*End:*)
