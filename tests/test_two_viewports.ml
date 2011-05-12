open Archimedes
module V = Viewport.Viewport
module P = Plot.Array

let draw bk =
  let vp = V.init ~w:1024. ~h:600. ~dirs:["../src"; "./src"] bk in
  let vps = V.layout_rows vp 2 in
  print_int (Array.length vps);
  let vp1 = vps.(0) and vp2 = vps.(1) in

  let path = Path.make () in
  Path.rectangle path 0. 0. 1. 1.;

  V.stroke ~path vp V.Graph;
  V.stroke ~path vp1 V.Graph;
  V.stroke ~path vp2 V.Graph;

  P.fx vp1 (fun x -> sin x *. (cos x +. 1.)) 0. 10.;
  P.fx vp2 (fun x -> x ** 2.) 0. 10.;

  V.close vp

let () =
  List.iter draw [ "cairo PNG test_boxes.png";
                   (*"graphics hold";
                     "tikz backend_path.tex"*)
                 ]
