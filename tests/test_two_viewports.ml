include Testing

module V = Archimedes.Viewport
module P = Archimedes.Plot.Function

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let vps = V.layout_rows vp 2 in
  let vp1 = vps.(0) and vp2 = vps.(1) in

  let path = Archimedes.Path.make () in
  Archimedes.Path.rectangle path 0. 0. 1. 1.;

  V.stroke ~path vp1 V.Graph;
  V.stroke ~path vp2 V.Graph;

  let sampling1 = P.sampling (fun x -> sin x *. (cos x +. 1.)) 0. 10. in
  let sampling2 = P.sampling (fun x -> x ** 2.) 0. 10. in
  P.x vp1 sampling1;
  P.x vp2 sampling2;

(*  V.rectangle vp2 ~x:0. ~y:0. ~w:1. ~h:1.;*)
  V.stroke vp2 V.Device;

  Archimedes.close vp
