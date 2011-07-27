include Testing

module V = Archimedes.Viewport
module P = Archimedes.Path

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let subvp = V.grid ~syncs:(true, true, false, true) vp 4 3 in
  V.xrange subvp.(1).(1) (-.2.) 7.;
  V.yrange subvp.(1).(1) (-.5.) 5.;
  for x = 0 to 3 do
    for y = 0 to 2 do
      let vp = subvp.(x).(y) in
      V.rectangle vp ~x:0. ~y:0. ~w:1. ~h:1.;
      V.stroke vp V.Device;
      Archimedes.Axes.cross vp ~tics:(Archimedes.Tics.Equidistants
                                        (Archimedes.Tics.Number 3, 0., 2., 1))
    done;
  done;
  Archimedes.close vp
