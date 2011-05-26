open Archimedes
module V = Viewport.Viewport
module P = Plot.Array

let data = [| 42.; 21.; 14.; 7.; 6.; 13.; 7.; 28.; 3.; 2.; 1.; -2.; 42.; 3.; 7.; 21.; 84.; 5. |]
let months =
  [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
     "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

let draw bk =
  let vp = V.init ~w:1024. ~h:600. ~dirs:["../src"; "./src"] bk in
  let custom_month x = Some (months.(truncate x mod 12)) in
  Axes.add_x_axis ~tics:(Tics.Equidistants ((Tics.Custom custom_month), 0., 1., 0)) vp;
  Axes.add_y_axis vp;
  P.x vp data;
  V.close vp

let () =
  List.iter draw [ "cairo PNG test_custom_labels.png";
                   (*"graphics hold";
                     "tikz backend_path.tex"*)
                 ]
