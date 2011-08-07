include Tests_common

module A = Archimedes

let data =
  [| 42.; 21.; -14.; 7.; 6.; -13.; -7.; 28.; -40.;
     2.; 1.; -30.; 42.; -20.; 7.; -41.; 20.; 5. |]
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
  A.Axes.x vp ~tics:(A.Tics.Equidistants (A.Tics.Custom tic_month, 0., 1., 0));
  A.Axes.y vp;
  A.Array.y vp data ~style:(`Bars 1.)
    ~fill:true ~fillcolor:A.Color.light_blue;

  let cumul_data = Array.copy data in
  for i = 1 to Array.length data - 1 do
    cumul_data.(i) <- cumul_data.(i) +. cumul_data.(i - 1)
  done;
  A.Viewport.set_line_width vp 2.;
  A.Viewport.set_color vp A.Color.red;
  A.Array.y vp cumul_data ~style:`Lines;

  A.close vp
