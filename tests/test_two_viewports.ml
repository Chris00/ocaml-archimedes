include Testing

module V = Archimedes.Viewport
module P = Archimedes.Plot.Function

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let vps = V.rows vp 2 in

  let path = Archimedes.Path.make () in
  Archimedes.Path.rectangle path 0. 0. 1. 1.;

  V.stroke ~path vps.(0) V.Graph;
  V.stroke ~path vps.(1) V.Graph;

  let sampling1 = P.sampling (fun x -> sin x *. (cos x +. 1.)) 0. 10. in
  let sampling2 = P.sampling (fun x -> x ** 2.) 0. 10. in
  P.x vps.(0) sampling1;
  P.x vps.(1) sampling2;

(*  V.rectangle vps.(1) ~x:0. ~y:0. ~w:1. ~h:1.;*)
  V.stroke vps.(1) V.Device;

  Archimedes.close vp
