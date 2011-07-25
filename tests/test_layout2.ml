include Testing

module V = Archimedes.Viewport
module P = Archimedes.Path

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let subvps = V.layout_grid ~syncs:(true, true, false, true) vp 3 4 in
  V.xrange subvps.(4) (-.2.) 7.;
  V.yrange subvps.(4) (-.5.) 5.;
  for i = 0 to 11 do
    V.rectangle subvps.(i) ~x:0. ~y:0. ~w:1. ~h:1.;
    V.stroke subvps.(i) V.Device;
    Archimedes.Axes.cross subvps.(i)
      ~tics:(Archimedes.Tics.Equidistants(Archimedes.Tics.Number 3, 0., 2., 1))
  done;

  Archimedes.close vp
