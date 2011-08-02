include Tests_common

module V = Archimedes.Viewport
module P = Archimedes.Path

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in

  (* Create a path *)
  let p = P.make () in
  P.move_to p ~x:0.2 ~y:0.2;
  P.line_to p ~x:0.8 ~y:0.8;

  (* Draw it (absolute) *)
  V.set_line_width vp 42.;
  V.set_color vp Archimedes.Color.red;
  V.stroke ~path:p vp V.Device;

  (* Draw it (relative) *)
  V.set_rel_line_width vp 5.;
  V.set_color vp Archimedes.Color.black;
  V.stroke ~path:p vp V.Device;

  (* Draw it (relative) *)
  V.set_rel_line_width vp 1.;
  V.set_color vp Archimedes.Color.white;
  V.stroke ~path:p vp V.Device;

  Archimedes.close vp
