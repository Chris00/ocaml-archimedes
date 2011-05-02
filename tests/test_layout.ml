open Printf
open Archimedes
module V = Viewport.Viewport
module P = Path

let pi = 4. *. atan 1.

let draw bk =
  let vp = V.init ~w:800. ~h:700. ~dirs:["../src"; "./src"] bk in
  let subvps = V.layout_grid vp 2 2 in
  let colors = [| Color.red; Color.blue; Color.green; Color.black |] in
  let trace i vp =
    V.set_global_color vp colors.(i);
    V.move_to vp ~x:0. ~y:0.;
    V.line_to vp ~x:1. ~y:0.;
    V.line_to vp ~x:1. ~y:1.;
    V.line_to vp ~x:0. ~y:1.;
    V.line_to vp ~x:0. ~y:0.;
    V.stroke vp V.Device
  in
  Array.iteri trace subvps;
(*
  let vp1 = subvps.(0)
  and vp2 = subvps.(1)
  and vp3 = subvps.(2)
  and vp4 = subvps.(3) in
  V.set_global_color vp Color.red;
  V.rectangle vp1 ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke vp1 V.Device;
  V.set_global_color vp Color.green;
  V.rectangle vp2 ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke vp2 V.Device;
  V.set_global_color vp Color.blue;
  V.rectangle vp3 ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke vp3 V.Device;
  V.set_global_color vp Color.black;
  V.rectangle vp4 ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke vp4 V.Device;*)
  V.close vp

let () =
  List.iter draw [ "cairo PNG test_layout.png"(*;
                   "graphics hold";
                   "tikz backend_path.tex"*)
                 ]


(* Local Variables: *)
(* compile-command: "make -kB backend_path.exe" *)
(* End: *)
