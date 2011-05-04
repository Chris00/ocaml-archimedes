open Printf
open Archimedes
module V = Viewport.Viewport
module P = Path

let pi = 4. *. atan 1.

let draw bk =
  let vp = V.init ~w:1024. ~h:768. ~dirs:["../src"; "./src"] bk in

  V.xrange vp (-10.) 10.;
  V.yrange vp (-2.) 8.;

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

  V.set_mark_size vp 1.;
  V.set_line_width vp 1.;
  V.render_mark vp 0. 0. "o";

  V.set_mark_size vp 50.;
  V.set_global_color vp Color.red;
  V.render_mark vp (-3.) 2. "X";

  V.move_to vp 1. 1.;
  V.line_to vp 2. 2.;
  V.set_global_color vp Color.green;
  V.stroke vp V.Data;

  V.rectangle vp 0. 0. 1. 1.;
  V.set_global_color vp Color.yellow;
  V.stroke vp V.Graph;

  V.set_global_color vp Color.black;
  V.add_x_axis vp;
  V.draw_axes vp;

  V.close vp

let () =
  List.iter draw [ "cairo PNG viewport_autofit.png"(*;
                   "graphics hold";
                   "tikz backend_path.tex"*)
                 ]


(* Local Variables: *)
(* compile-command: "make -kB backend_path.exe" *)
(* End: *)
