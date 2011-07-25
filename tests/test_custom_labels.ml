include Testing

module V = Viewport
module P = Plot.Array

let data =
  [| 42.; 21.; -14.; 7.; 6.; -13.; -7.; 28.; 3.;
     2.; 1.; -2.; 42.; -3.; 7.; -21.; 84.; 5. |]
let months =
  [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
     "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let custom_month x =
    let x' = truncate x in
    let y = 2010 + x' / 12
    and m = x' mod 12 in
    let month = months.(m) in
    let label = if m = 0 then month ^ " " ^ string_of_int y else month in
    Some (label)
  in
  Axes.add_x_axis vp
    ~tics:(Tics.Equidistants ((Tics.Custom custom_month), 0., 1., 0));
  Axes.add_y_axis vp;
  P.x vp data;
  Archimedes.close vp
