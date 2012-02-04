include Tests_common

module A = Archimedes
module V = A.Viewport

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let vps = V.rows vp 2 in

  let path = Archimedes.Path.make () in
  Archimedes.Path.rectangle path 0. 0. 1. 1.;

  V.stroke vps.(0) `Graph path;
  V.stroke vps.(1) `Graph path;

  A.fx vps.(0) (fun x -> sin x *. (cos x +. 1.)) 0. 10.;
  A.fx vps.(1) (fun x -> x ** 2.) 0. 10.;

  Archimedes.close vp
