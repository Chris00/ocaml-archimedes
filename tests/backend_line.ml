include Tests_common
module Backend = Archimedes.Backend

let description = "Some drawings on backend (arcs, equal sign, huge line)"

let draw bk =
  let bk = Backend.make bk 700. 700. ~dirs in

  (* Only the size at the time of stroking is important. *)
  Backend.set_line_width bk 1.;
  Backend.move_to bk 100. 150.;
  Backend.line_to bk 300. 150.;
  Backend.set_line_width bk 100.;
  Backend.stroke bk;

  (* Arc *)
  Backend.set_line_width bk 70.;
  Backend.move_to bk 100. 450.;
  Backend.arc bk 200. pi (2.5 *. pi);
  Backend.stroke bk;
  Backend.set_color bk (Archimedes.Color.rgb 1. 0. 0.);
  Backend.move_to bk 100. 450.;
  Backend.arc bk 200. pi (2.5 *. pi);
  Backend.set_line_width bk 10.;
  Backend.stroke bk;

  (* Curve to *)
  Backend.set_color bk (Archimedes.Color.rgb 1. 0.7 0.);
  Backend.move_to bk 150. 200.;
  Backend.curve_to bk ~x1:250. ~y1:250. ~x2:100. ~y2:100. ~x3:0. ~y3:0.;
  Backend.set_line_width bk 15.;
  Backend.stroke bk;

  (* The line width and coordinates are relative to the current CTM *)
  Backend.set_line_width bk 10.;
  Backend.move_to bk 200. 500.;
  Backend.line_to bk 350. 500.;
  Backend.stroke bk;
  Backend.scale bk 1. 4.;
  Backend.move_to bk 200. 110.;
  Backend.line_to bk 350. 110.;
  Backend.stroke bk;

  Backend.close bk
