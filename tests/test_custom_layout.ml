open Printf
open Archimedes
module V = Viewport.Viewport
module P = Path

let pi = 4. *. atan 1.

let draw bk =
  let vp = V.init ~w:1024. ~h:768. ~dirs:["../src"; "./src"] bk in
  let trace i vp =
    V.set_rel_line_width vp 1.;

    V.set_global_color vp Color.red;
    V.rectangle vp ~x:0. ~y:0. ~w:1. ~h:1.;
    V.stroke vp V.Graph;

    V.set_line_width vp 1.;

    V.set_global_color vp Color.blue;
    V.rectangle vp ~x:0. ~y:0. ~w:1. ~h:1.;
    V.stroke vp V.Orthonormal;

    V.set_global_color vp Color.green;
    V.move_to vp ~x:0. ~y:0.;
    V.line_to vp ~x:1. ~y:0.;
    V.line_to vp ~x:1. ~y:1.;
    V.line_to vp ~x:0. ~y:1.;
    V.line_to vp ~x:0. ~y:0.;
    V.stroke vp V.Device
  in
  let redim _ _ _ = () in
  let custom_vp = V.make vp V.Device 0.3 0.4 0.3 0.8 redim in
  trace 1 custom_vp;

  V.move_to vp 0.3 0.;
  V.line_to vp 0.3 1.;
  V.set_global_color vp Color.yellow;
  V.stroke vp V.Device;

  V.set_global_color vp Color.red;
  V.set_rel_line_width vp 1.;
  V.rectangle vp ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke vp V.Graph;

  V.close vp

let () =
  List.iter draw [ "cairo PNG test_custom_layout.png";
                   (*"graphics hold";
                   "tikz backend_path.tex"*)
                 ]


(* Local Variables: *)
(* compile-command: "make -kB backend_path.exe" *)
(* End: *)
