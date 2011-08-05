include Tests_common

module A = Archimedes

let data = [|
  [| 42.; 21.; 14.; 7.; 6.; 3.; 2.; 1.; 2. |];
  [| 18.;  3.; 3.; 4.; 5.; 6.; 7.; 8.; 3. |];
  [| 15.; 61.; 67.; 73.; 73.; 75.; 75.; 75.; 83. |];
  [| 15.;  5.; 5.; 5.; 5.; 5.; 5.; 5.; 5. |];
|]

let draw bk =
  let vp = A.init ~w ~h ~dirs bk in
  A.Viewport.set_line_width vp 1.;
  A.Array.stack vp data;
  A.Axes.cross vp;
  A.close vp
