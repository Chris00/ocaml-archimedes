include Tests_common

module V = Archimedes.Viewport
module P = Archimedes.Path

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let trace _ vp =
    V.set_rel_line_width vp 1.;

    V.set_color vp Archimedes.Color.red;
    V.rectangle vp ~x:0. ~y:0. ~w:1. ~h:1.;
    V.stroke vp `Graph;

    V.set_line_width vp 1.;

    V.set_color vp Archimedes.Color.blue;
    V.rectangle vp ~x:0. ~y:0. ~w:1. ~h:1.;
    V.stroke vp `Orthonormal;

    V.set_color vp Archimedes.Color.green;
    V.move_to vp ~x:0. ~y:0.;
    V.line_to vp ~x:1. ~y:0.;
    V.line_to vp ~x:1. ~y:1.;
    V.line_to vp ~x:0. ~y:1.;
    V.line_to vp ~x:0. ~y:0.;
    V.stroke vp `Device
  in
  let custom_vp = V.make vp 0.3 0.4 0.3 0.8 in
  trace 1 custom_vp;

  V.move_to vp 0.3 0.;
  V.line_to vp 0.3 1.;
  V.set_color vp Archimedes.Color.yellow;
  V.stroke vp `Device;

  V.set_color vp Archimedes.Color.red;
  V.set_rel_line_width vp 1.;
  V.rectangle vp ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke vp `Graph;

  Archimedes.close vp
