include Tests_common
module V = Archimedes.Viewport

let draw bk =
  let f vp x y = Archimedes.Arrows.line vp x y (x -. y) (x +. y) in
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let vp2 = V.make vp V.Device 0.3 0.7 0.2 0.8 (fun _ _ _ -> ()) in
  V.xrange vp2 (-3.) 3.;
  V.yrange vp2 (-2.) 2.;
  for x = -5 to 5 do
    for y = -5 to 5 do
      f vp2 (float x +. 0.5) (float y +. 0.5)
    done
  done;
  Archimedes.Axes.box vp2;
  Archimedes.close vp
