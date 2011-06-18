open Archimedes
module V = Viewport
module P = Plot.Array

let draw bk =
  let vp = V.init ~w:1024. ~h:600. ~dirs:["../src"; "./src"] bk in
  let vps = V.layout_grid vp 2 2 in
  let vp1 = vps.(0) and vp2 = vps.(3) in

  V.set_color vp1 Color.yellow;
  P.fx ~nsamples:50 ~fill:true ~pathstyle:(P.Boxes 0.08) vp1 sin (-5.) 5.;
  V.set_color vp1 Color.black;
  Axes.box vp1;

  P.fx ~nsamples:30 ~pathstyle:(P.Interval 0.1) vp2 sin (-5.) 5.;
  Axes.box vp2;

  Axes.box vp;

  let vp0 = vps.(1) in
  V.yrange vp0 (-2.) 2.;
  P.fx vp0 (fun x -> 1. /. x) (-1.) 1.;
  Axes.box vp0;

  V.close vp

let () =
  List.iter draw [ "cairo PNG test_boxes.png";
                   (*"graphics hold";
                     "tikz backend_path.tex"*)
                 ]
