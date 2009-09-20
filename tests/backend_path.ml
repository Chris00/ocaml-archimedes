open Printf
open Archimedes

let pi = 4. *. atan 1.

let draw bk =
  let bk = Backend.make bk 800. 700. ~dirs:[ "./src"; "../src" ] in
  Backend.set_matrix bk (Backend.backend_to_device bk);
  Backend.scale bk 1.5 1.5;

  Backend.move_to bk 500. 300.;
  Backend.arc bk 150. 0. (2. *. pi);
  Backend.stroke bk;

  (* Black disk *)
  Backend.move_to bk 400. 300.;
  Backend.arc bk 100. 0. (2. *. pi);
  Backend.fill bk;

  Backend.set_color bk (Color.rgb 0.5 0. 0.5);
  Backend.move_to bk 20. 430.;
  Backend.curve_to bk 30. 480. 100. 450. 130. 430.;
  Backend.line_to bk 50. 440.;
  Backend.stroke bk;
  Backend.move_to bk 20. 400.;
  Backend.curve_to bk 30. 450. 100. 420. 130. 400.;
  Backend.line_to bk 50. 380.;
  Backend.fill bk;
  Backend.move_to bk 20. 370.;
  Backend.line_to bk 100. 370.;
  Backend.line_to bk 50. 350.;
  Backend.fill bk;

  Backend.save bk;
  begin
    Backend.set_color bk (Color.rgb 0.9 0. 0.2);
    Backend.move_to bk 450. 400.;
    Backend.scale bk 3. 1.;
    Backend.arc bk 50. 0. (2. *. pi);
    Backend.stroke bk;

    Backend.save bk;
    Backend.move_to bk 100. 300.;
    Backend.rotate bk (pi /. 4.);
    Backend.arc bk 10. 0. (2. *. pi);
    Backend.stroke bk;
    Backend.restore bk;

    Backend.set_color bk (Color.rgba 0. 0.7 0.2 0.5);
    Backend.move_to bk 150. 400.;
    (* Backend.arc bk 20. 0. (2. *. pi); *)
    Backend.curve_to bk 150. 480.  80. 420.  50. 400.;
    Backend.fill bk;
  end;
  Backend.restore bk;

  (* Filling a self crossing path. *)
  Backend.move_to bk 50. 100.;
  Backend.curve_to bk 80. 180.  180. 180.  200. 100.;
  Backend.curve_to bk 220. 30.  350. 20.  400. 100.;
  Backend.fill bk;

  (* Path made of several subpaths *)
  Backend.set_color bk (Color.rgb 0. 0. 0.7);
  Backend.move_to bk 30. 300.;
  Backend.line_to bk 100. 280.;
  Backend.line_to bk 100. 320.;
  Backend.rectangle bk 30. 220. 100. 10.;
  Backend.line_to bk 120. 270.;
  Backend.line_to bk 140. 250.;
  Backend.rectangle bk 30. 200. 100. 10.; (* subpath in itself *)
  Backend.fill bk;

  Backend.close bk

let () =
  List.iter draw [ "cairo PDF backend_path.pdf";
                   "tikz backend_path.tex";
                   "graphics hold" ]


(* Local Variables: *)
(* compile-command: "make -kB backend_path.exe" *)
(* End: *)
