include Tests_common

module A = Archimedes

let data =
  [| 42.; 21.; -14.; 7.; 6.; -13.; -7.; 28.; 3.;
     2.; 1.; -2.; 42.; -3.; 7.; -21.; 84.; 5. |]
let months =
  [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
     "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

let draw bk =
  let vp = A.init ~w ~h ~dirs bk in
  let tic_month x =
    let x' = truncate x in
    let y = 2010 + x' / 12
    and m = x' mod 12 in
    let month = months.(m) in
    let label = if m = 0 then month ^ " " ^ string_of_int y else month in
    label
  in
  A.Axes.add_x_axis vp
    ~tics:(A.Tics.Equidistants (A.Tics.Custom tic_month, 0., 1., 0));
  A.Axes.add_y_axis vp;
  A.Plot.Array.x vp data ~pathstyle:(A.Plot.Boxes 1.)
    ~fill:true ~fillcolor:A.Color.blue;
  A.close vp
