open Printf
open Archimedes
module V = Viewport.Viewport
module P = Path

let pi = 4. *. atan 1.

let draw bk =
  let vp = V.init ~w:1024. ~h:768. ~dirs:["../src"; "./src"] bk in
  let subvps = V.layout_grid ~syncs:(true, false, true, true) vp 2 2 in
  V.rectangle subvps.(0) 0. 0. 1. 1.;
  V.stroke subvps.(0) V.Data;
  V.xrange subvps.(0) (-10.) 10.;
  V.yrange subvps.(0) (-3.) 5.;
  V.set_line_width subvps.(0) 1.;
  Axes.cross subvps.(0);
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
