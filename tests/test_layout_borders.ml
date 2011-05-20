open Printf
open Archimedes
module V = Viewport.Viewport
module P = Path

let pi = 4. *. atan 1.

let draw bk =
  let vp = V.init ~w:1024. ~h:768. ~dirs:["../src"; "./src"] bk in
  let v1, v2, v3, v4, v5 = V.layout_borders ~north:0.3 ~south:0.1 ~west:0.1 ~east:0.5 vp in
  V.rectangle v1 ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke v1 V.Device;
  V.set_color v2 Color.red;
  V.rectangle v2 ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke v2 V.Device;
  V.set_color v3 Color.blue;
  V.rectangle v3 ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke v3 V.Device;
  V.set_color v4 Color.green;
  V.rectangle v4 ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke v4 V.Device;
  V.set_color v5 Color.yellow;
  V.rectangle v5 ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke v5 V.Device;

  V.close vp

let () =
  try
    List.iter draw [ "cairo PNG test_layout_borders.png"(*;
                     "graphics hold";
                     "tikz backend_path.tex"*)
                   ]
  with Backend.Error e as exn ->
    Printf.printf "Backend.Error %s\n%!" (Backend.string_of_error e);
    raise exn


(* Local Variables: *)
(* compile-command: "make -kB backend_path.exe" *)
(* End: *)
