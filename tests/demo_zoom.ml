open Archimedes
module V = Viewport.Viewport
module P = Plot.Array

let pi = acos (-1.)

let draw bk =
  let vp = V.init ~w:1024. ~h:600. ~dirs:["../src"; "./src"] bk in
  let vp_zoom = V.make vp V.Graph 0.5 1. 0.6 1. (fun _ _ _ -> () ) in
  Axes.add_x_axis
    ~tics:(Tics.Equidistants (Tics.Number 3, 0., pi, 1))
    ~offset:(Axes.Relative 0.) vp;
  Axes.add_y_axis ~tics:(Tics.Equidistants (Tics.Number 3, 0., 1., 1)) ~offset:(Axes.Relative 0.) vp;
  Axes.add_x_axis
    ~tics:(Tics.Equidistants (Tics.Number 3, 0., pi, 1)) vp_zoom;
  Axes.add_y_axis vp_zoom;
  V.axes_ratio vp pi;
  V.sync_ratio vp_zoom vp;
  P.fx vp sin 0. 10.;
  V.yrange vp (- 1.5) 2.;
  P.fx vp_zoom sin (9. *. pi /. 4.) (11. *. pi /. 4.);

  V.close vp

let () =
  List.iter draw [ "cairo PNG demo_zoom.png";
                   (*"graphics hold";
                     "tikz backend_path.tex"*)
                 ]


(*Local Variables:*)
(*compile-command: "ocamlopt -o test_axes.com -I ../src unix.cmxa dynlink.cmxa bigarray.cmxa archimedes.cmxa test_axes.ml && ocamlc -o test_axes.exe -I ../src unix.cma dynlink.cma bigarray.cma archimedes.cma test_axes.ml"*)
(*End:*)
