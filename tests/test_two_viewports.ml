open Archimedes
module V = Viewport
module P = Plot.Array

let draw bk =
  let vp = V.init ~w:1024. ~h:600. ~dirs:["../src"; "./src"] bk in
  let vps = V.layout_rows vp 2 in
  let vp1 = vps.(0) and vp2 = vps.(1) in

  let path = Path.make () in
  Path.rectangle path 0. 0. 1. 1.;


  V.stroke ~path vp1 V.Graph;
  V.stroke ~path vp2 V.Graph;

  P.fx vp1 (fun x -> sin x *. (cos x +. 1.)) 0. 10.;
  P.fx vp2 (fun x -> x ** 2.) 0. 10.;

(*  V.rectangle vp2 ~x:0. ~y:0. ~w:1. ~h:1.;*)
  V.stroke vp2 V.Device;

  V.close vp

let () =
  List.iter draw [ "cairo PNG test_two_viewports.png";
                   (*"graphics hold";
                     "tikz backend_path.tex"*)
                 ]
