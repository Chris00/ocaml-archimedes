open Testing

module V = Viewport
module P = Plot.Array

let data = [|
  [| 42.; 21.; 14.; 7.; 6.; 3.; 2.; 1.; -2. |];
  [| 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 3. |];
  [| 41.; 61.; 67.; 73.; 73.; 75.; 75.; 75.; 83. |]
|]

let draw bk =
  let vp = V.init ~w ~h ~dirs bk in
  V.set_line_width vp 1.;
  P.stack vp data;
  Axes.cross vp;
  V.close vp
