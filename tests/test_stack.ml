include Testing

module V = Archimedes.Viewport
module P = Archimedes.Plot.Array

let data = [|
  [| 42.; 21.; 14.; 7.; 6.; 3.; 2.; 1.; -2. |];
  [| 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 3. |];
  [| 41.; 61.; 67.; 73.; 73.; 75.; 75.; 75.; 83. |];
  [| 5.; 5.; 5.; 5.; 5.; 5.; 5.; 5.; 5. |];
|]

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  V.set_line_width vp 1.;
  P.stack vp data;
  Archimedes.Axes.cross vp;
  Archimedes.close vp
