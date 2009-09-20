open Printf
open Archimedes

let pi = 4. *. atan 1.

let draw bk =
  let bk = Backend.make bk 800. 700. ~dirs:[ "./src"; "../src" ] in
  Backend.set_matrix bk (Backend.backend_to_device bk);

  (* Only the size at the time of stroking is important. *)
  Backend.set_line_width bk 1.;
  Backend.move_to bk 100. 150.;
  Backend.line_to bk 300. 150.;
  Backend.set_line_width bk 100.;
  Backend.stroke bk;

  Backend.set_line_width bk 70.;
  Backend.move_to bk 100. 450.;
  Backend.arc bk 200. pi (2.5 *. pi);
  Backend.stroke bk;
  Backend.set_color bk (Color.rgb 1. 0. 0.);
  Backend.move_to bk 100. 450.;
  Backend.arc bk 200. pi (2.5 *. pi);
  Backend.set_line_width bk 10.;
  Backend.stroke bk;

  Backend.close bk

let () =
  List.iter draw [ "cairo PDF backend_line.pdf";
                   "tikz backend_line.tex";
                   "graphics hold" ]


(* Local Variables: *)
(* compile-command: "make -kB backend_line.exe" *)
(* End: *)
