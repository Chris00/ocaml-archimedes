open Printf
open Archimedes
module V = Viewport.Viewport
module P = Path

let pi = 4. *. atan 1.

let draw bk =
  let vp = V.init ~w:800. ~h:700. ~dirs:["../src"; "./src"] bk in
  let bk = V.get_backend vp in
  let p = P.make () in
  P.move_to p ~x:0.2 ~y:0.2;
  P.line_to p ~x:0.8 ~y:0.8;
  V.set_line_width vp 42.;
  V.set_global_color vp Color.red;
  V.stroke ~path:p vp V.Device;
  V.close vp

let () =
  List.iter draw [ "cairo PNG viewport_path.png"(*;
                   "graphics hold";
                   "tikz backend_path.tex"*)
                 ]


(* Local Variables: *)
(* compile-command: "make -kB backend_path.exe" *)
(* End: *)
