open Archimedes
module V = Viewport.Viewport

let markers = Array.of_list (Pointstyle.names ())

let draw backend =
  let vp = V.init ~w:800. ~h:600. ~dirs:["../src"; "./src"] backend in

  V.set_line_width vp 1.;
  Arrows.line ~head:(Arrows.Unstyled) vp 0. 0.6 0.2 0.6;
  Arrows.line ~head:(Arrows.Simple) vp 0.3 0.6 0.5 0.6;
  Arrows.line ~head:(Arrows.Double) vp 0.6 0.6 0.8 0.6;
  Arrows.line ~head:(Arrows.Triple) vp 0. 0.3 0.2 0.3;
  Arrows.line ~head:(Arrows.Diamond) vp 0.3 0.3 0.5 0.3;
  Arrows.line ~head:(Arrows.Circle) vp 0.6 0.3 0.8 0.3;
  Arrows.line ~head:(Arrows.Stop) vp 0. 0.1 0.2 0.1;

  V.close vp


let () =
  let bk =
    if Array.length Sys.argv > 1 then [Sys.argv.(1)]
    else [ "cairo PNG test_arrows2.png";(*;
           "tikz marks.tex";
           "graphics hold" *)]
  in
  List.iter draw bk
(*  try List.iter draw bk
  with Backend.Error e ->
    print_endline (Backend.string_of_error e);
    exit 1*)
