open Testing

module V = Viewport

let draw bk =
  let f vp x y =
    Arrows.line ~head:Arrows.Simple vp x y (x -. y) (x +. y)
  in
  let vp = V.init ~w ~h ~dirs bk in
  V.set_line_width vp 1.;
  for x = -5 to 5 do
    for y = -5 to 5 do
      f vp (float x +. 0.5) (float y +. 0.5)
    done
  done;
  Axes.box vp;
  V.close vp
