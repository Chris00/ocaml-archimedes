(* Graph of packages and uploads *)

module A = Archimedes

let string_of_month m =
  if 0 <= m && m <= 11 then
    [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug";
       "Sep"; "Oct"; "Nov"; "Dec" |].(m)
  else ""

let upload_chart stats =
  let vp = A.init ["Cairo"; "PNG"; "pkg_chart.png"] ~w:410. ~h:187.
    ~bg:(A.Color.rgba 1. 1. 1. 0.)in
  let months = Array.of_list(List.map (fun (m,_,_) -> string_of_month m) stats) in
  let month x = try months.(truncate x) with _ -> "" in
  A.Axes.x vp ~tics:(A.Tics.Equidistants(A.Tics.Custom month, 0., 1., 0))
    ~major:("tic_down", 2.);
  A.Axes.y vp;

  A.set_color vp A.Color.silver;
  A.List.y vp (List.map (fun (_, _, upl) -> float upl) stats)
    ~style:(`Bars 0.8) ~fill:true ~fillcolor:A.Color.light_gray;

  A.set_color vp A.Color.black;
  A.set_line_width vp 3.;
  A.List.y vp (List.map (fun (_, pkg, _) -> float pkg) stats)
    ~style:`Lines;

  A.close vp


let () =
  let stats = [(6, 3, 2); (7, 4, 4); (8, 6, 4); (9, 15, 2);
               (10, 20, 0); (11, 22, 6); (0, 28, 3); (1, 30, 5);
               (2, 30, 2); (3, 35, 4); (4, 37, 20); (5, 37, 10) ] in
  upload_chart stats
