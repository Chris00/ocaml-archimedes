open Printf
open Archimedes
module V = Viewport
module P = Path

let pi = 4. *. atan 1.

let draw bk =
  let vp = V.init ~w:1024. ~h:768. ~dirs:["../src"; "./src"] bk in
  let subvps = V.layout_grid ~syncs:(true, true, false, true) vp 3 4 in
  V.xrange subvps.(4) (-.2.) 7.;
  V.yrange subvps.(4) (-.5.) 5.;
  for i = 0 to 11 do
    V.rectangle subvps.(i) ~x:0. ~y:0. ~w:1. ~h:1.;
    V.stroke subvps.(i) V.Device;
    Axes.cross ~tics:(Tics.Equidistants (Tics.Number 3, 0., 2., 1)) subvps.(i)
  done;

  V.close vp

let () =
  try
    List.iter draw [ "cairo PNG test_layout2.png"(*;
                     "graphics hold";
                     "tikz backend_path.tex"*)
                   ]
  with Backend.Error e as exn ->
    Printf.printf "Backend.Error %s\n%!" (Backend.string_of_error e);
    raise exn


(* Local Variables: *)
(* compile-command: "make -kB backend_path.exe" *)
(* End: *)
