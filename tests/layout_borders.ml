include Tests_common

module V = Archimedes.Viewport
module P = Archimedes.Path

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let p = Archimedes.Path.make () in
  Archimedes.Path.rectangle p ~x:0. ~y:0. ~w:1. ~h:1.;
  let v1, v2, v3, v4, v5 =
    V.layout_borders ~north:0.3 ~south:0.1 ~west:0.1 ~east:0.5 vp in
  V.stroke v1 `Device p;
  V.set_color v2 Archimedes.Color.red;
  V.stroke v2 `Device p;
  V.set_color v3 Archimedes.Color.blue;
  V.stroke v3 `Device p;
  V.set_color v4 Archimedes.Color.green;
  V.stroke v4 `Device p;
  V.set_color v5 Archimedes.Color.yellow;
  V.stroke v5 `Device p;

  Archimedes.close vp
