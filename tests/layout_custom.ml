include Tests_common

module V = Archimedes.Viewport
module P = Archimedes.Path

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let p_rect = Archimedes.Path.make () in
  Archimedes.Path.rectangle p_rect ~x:0. ~y:0. ~w:1. ~h:1.;
  let trace _ vp =
    V.set_rel_line_width vp 1.;

    V.set_color vp Archimedes.Color.red;
    V.stroke vp `Graph p_rect;

    V.set_line_width vp 1.;

    V.set_color vp Archimedes.Color.blue;
    V.stroke vp `Orthonormal p_rect;

    V.set_color vp Archimedes.Color.green;
    let p = Archimedes.Path.make () in
    Archimedes.Path.move_to p ~x:0. ~y:0.;
    Archimedes.Path.line_to p ~x:1. ~y:0.;
    Archimedes.Path.line_to p ~x:1. ~y:1.;
    Archimedes.Path.line_to p ~x:0. ~y:1.;
    Archimedes.Path.line_to p ~x:0. ~y:0.;
    V.stroke vp `Device p
  in
  let custom_vp = V.make vp 0.3 0.4 0.3 0.8 in
  trace 1 custom_vp;

  let p = Archimedes.Path.make () in
  Archimedes.Path.move_to p 0.3 0.;
  Archimedes.Path.line_to p 0.3 1.;
  V.set_color vp Archimedes.Color.yellow;
  V.stroke vp `Device p;

  V.set_color vp Archimedes.Color.red;
  V.set_rel_line_width vp 1.;
  V.stroke vp `Graph p_rect;

  Archimedes.close vp
