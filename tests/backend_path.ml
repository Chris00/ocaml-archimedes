open Printf
open Archimedes

let pi = 4. *. atan 1.

let draw bk =
  let bk = Backend.make bk 600. 600. ~dirs:[ "./src"; "../src" ] in
  Backend.set_matrix bk (Backend.backend_to_device bk);

  Backend.move_to bk 500. 300.;
  Backend.arc bk 150. 0. (2. *. pi);
  Backend.stroke bk;

  Backend.move_to bk 400. 300.;
  Backend.arc bk 100. 0. (2. *. pi);
  Backend.fill bk;

  Backend.set_color bk (Color.rgb 0.9 0. 0.2);
  Backend.move_to bk 450. 400.;
  Backend.scale bk 3. 1.;
  Backend.arc bk 50. 0. (2. *. pi);
  Backend.stroke bk;

  Backend.set_color bk (Color.rgba 0. 0.7 0.2 0.5);
  Backend.move_to bk 150. 400.;
  (* Backend.arc bk 20. 0. (2. *. pi); *)
  Backend.curve_to bk 150. 480.  80. 420.  50. 400.;
  Backend.fill bk;

  Backend.close bk

let () =
  List.iter draw [ "cairo PDF backend_path.pdf";
                   "tikz backend_path.tex";
                   "graphics hold" ]


(* Local Variables: *)
(* compile-command: "make -kB backend_path.exe" *)
(* End: *)
