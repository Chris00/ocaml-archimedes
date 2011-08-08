include Tests_common

module V = Archimedes.Viewport
module P = Archimedes.Path

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let v1, v2, v3, v4, v5 =
    V.layout_borders ~north:0.3 ~south:0.1 ~west:0.1 ~east:0.5 vp in
  V.rectangle v1 ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke v1 `Device;
  V.set_color v2 Archimedes.Color.red;
  V.rectangle v2 ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke v2 `Device;
  V.set_color v3 Archimedes.Color.blue;
  V.rectangle v3 ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke v3 `Device;
  V.set_color v4 Archimedes.Color.green;
  V.rectangle v4 ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke v4 `Device;
  V.set_color v5 Archimedes.Color.yellow;
  V.rectangle v5 ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke v5 `Device;

  Archimedes.close vp
