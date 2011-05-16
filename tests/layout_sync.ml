open Printf
open Archimedes
module V = Viewport.Viewport
module P = Path
module Pl = Plot.Array

let draw bk =
  let vp = V.init ~w:1024. ~h:768. ~dirs:["../src"; "./src"] bk in
  let subvps = V.layout_grid ~syncs:(true, true, false, true) vp 2 2 in
  V.rectangle subvps.(0) ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke subvps.(0) V.Data;
  V.xrange subvps.(0) (-10.) 10.;
  V.yrange subvps.(0) (-3.) 5.;
  V.xrange subvps.(1) (-5.) 5.;
  V.yrange subvps.(1) (-1.) 5.;
 (* Pl.fx subvps.(0) (fun x -> x *. x) 0. 30.;*)
  V.set_line_width subvps.(0) 1.;
  Axes.cross ~tics:(Tics.Equidistants (Tics.Number 5, 0., 3., 1)) subvps.(0);
  V.set_global_color subvps.(1) Color.red;
  Axes.cross ~tics:(Tics.Equidistants (Tics.Number 5, 0., 1., 1)) subvps.(1);
  V.set_global_color subvps.(2) Color.blue;
  Axes.cross ~tics:(Tics.Equidistants (Tics.Number 5, 0., 1.5, 1)) subvps.(2);
  V.set_global_color subvps.(3) Color.green;
  Axes.cross ~tics:(Tics.Equidistants (Tics.Number 5, 0., 2.5, 1)) subvps.(3);
  V.close vp

let () =
  try
    List.iter draw [ "cairo PNG layout_sync.png";
                    (* "graphics hold";
                     "tikz backend_path.tex"*)
                   ]
  with Backend.Error e as exn ->
    Printf.printf "Backend.Error %s\n%!" (Backend.string_of_error e);
    raise exn


(* Local Variables: *)
(* compile-command: "make -kB backend_path.exe" *)
(* End: *)
