open Archimedes
module V = Viewport.Viewport
module P = Plot.Common

let draw bk =
  let vp = V.init ~w:1024. ~h:600. ~dirs:["../src"; "./src"] bk in
  V.set_global_color vp Color.yellow;
  P.fx ~fill:true ~pathstyle:(P.Boxes 0.08) vp sin (-10.) 10.;
  V.set_global_color vp Color.black;
  Axes.box vp;
  V.close vp

let () =
  List.iter draw [ "cairo PNG test_boxes.png";
                   (*"graphics hold";
                     "tikz backend_path.tex"*)
                 ]
