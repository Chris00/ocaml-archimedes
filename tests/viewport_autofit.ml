open Tests_common

module V = Archimedes.Viewport
module P = Archimedes.Path

let description = "Check the automatic determination of ranges"

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in

  V.xrange vp (-10.) 10.;
  V.yrange vp (-2.) 8.;

  (* Create a path *)
  let p = P.make () in
  P.rectangle p ~x:(-.3.) ~y:2. ~w:6. ~h:6.;

  V.set_line_width vp 21.;
  V.set_color vp Archimedes.Color.orange;
  V.stroke ~path:p vp `Data;

  V.set_color vp Archimedes.Color.forest_green;
  V.move_to vp ~x:(-3.) ~y:2.;
  V.line_to vp ~x:(-2.) ~y:3.;
  V.stroke vp `Data;

  V.set_mark_size vp 30.;
  V.set_line_width vp 1.;
  V.mark vp 0. 0. "o";

  V.set_mark_size vp 50.;
  V.set_color vp Archimedes.Color.silver;
  V.mark vp (-3.) 2. "x";

  V.move_to vp 1. 1.;
  V.line_to vp 2. 2.;
  V.set_color vp Archimedes.Color.green;
  V.stroke vp `Data;

  V.rectangle vp 0. 0. 1. 1.;
  V.set_color vp Archimedes.Color.red;
  V.stroke vp `Graph;

  V.set_color vp Archimedes.Color.black;
  Archimedes.Axes.x vp;

  Archimedes.close vp
