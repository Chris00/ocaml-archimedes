open Printf
open Archimedes
module V = Viewport.Viewport
module P = Path

let pi = 4. *. atan 1.

let draw bk =
  let vp = V.init ~w:1024. ~h:768. ~dirs:["../src"; "./src"] bk in

  (*V.xrange vp (-5.) 5.;
  V.yrange vp 0. 10.;*)

  (* Create a path *)
  let p = P.make () in
  P.rectangle p ~x:(-.3.) ~y:2. ~w:6. ~h:6.;

  V.set_line_width vp 21.;
  V.set_global_color vp Color.red;
  V.stroke ~path:p vp V.Data;

  V.set_global_color vp Color.black;
  V.move_to vp ~x:(-3.) ~y:2.;
  V.line_to vp ~x:(-2.) ~y:3.;
  V.stroke vp V.Data;

  V.close vp

let () =
  List.iter draw [ "cairo PNG viewport_autofit.png"(*;
                   "graphics hold";
                   "tikz backend_path.tex"*)
                 ]


(* Local Variables: *)
(* compile-command: "make -kB backend_path.exe" *)
(* End: *)
