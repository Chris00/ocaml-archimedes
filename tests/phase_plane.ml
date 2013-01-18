include Tests_common

let description = "Phase plane (test nan)"

let rec range_hue a b n =
  let h = (b -. a) /. float(n - 1) in
  let hue = 240. /. float n in
  Array.init n (fun i -> (a +. float i *. h, Archimedes.Color.hue(float i *. hue)))

let f u =
  let u2 = u *. u in
  u2 *. (0.25 *. u2 -. 0.5)

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  Archimedes.Axes.box vp;
  Archimedes.Viewport.xlabel vp "u";
  Archimedes.Viewport.ylabel vp "u'";
  let level_set (u0, c) =
    Archimedes.set_color vp c;
    Archimedes.Array.xy vp [| -. u0 |] [| 0. |];
    let g u = sqrt(1. -. 1. /. ((1. +. f u0 -. f u)**2.)) in
    Archimedes.fx vp g (-. u0) u0;
    Archimedes.fx vp (fun u -> -. g u) (-. u0) u0
  in
  Array.iter level_set (range_hue 1. 3. 20);
  Archimedes.close vp
