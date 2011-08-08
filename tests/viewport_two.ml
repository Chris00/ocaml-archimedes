include Tests_common

module A = Archimedes
module V = A.Viewport

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let vps = V.rows vp 2 in

  let path = Archimedes.Path.make () in
  Archimedes.Path.rectangle path 0. 0. 1. 1.;

  V.stroke ~path vps.(0) `Graph;
  V.stroke ~path vps.(1) `Graph;

  A.fx vps.(0) (fun x -> sin x *. (cos x +. 1.)) 0. 10.;
  A.fx vps.(1) (fun x -> x ** 2.) 0. 10.;

(*  V.rectangle vps.(1) ~x:0. ~y:0. ~w:1. ~h:1.;*)
  V.stroke vps.(1) `Device;

  Archimedes.close vp
