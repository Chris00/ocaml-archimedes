include Testing

module V = Archimedes.Viewport

let draw bk =
  let f vp x y = Archimedes.Arrows.line vp x y (x -. y) (x +. y)
    ~head:Archimedes.Arrows.Simple in
  let vp = Archimedes.init ~w ~h ~dirs bk in
  V.set_line_width vp 1.;
  for x = -5 to 5 do
    for y = -5 to 5 do
      f vp (float x +. 0.5) (float y +. 0.5)
    done
  done;
  Archimedes.Axes.box vp;
  Archimedes.close vp
