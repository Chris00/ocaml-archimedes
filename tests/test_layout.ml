open Printf
open Archimedes
module V = Viewport.Viewport
module P = Path

let pi = 4. *. atan 1.

let draw bk =
  let vp = V.init ~w:1024. ~h:768. ~dirs:["../src"; "./src"] bk in
  let subvps = V.layout_grid vp 2 2 in
  let subsubvps = V.layout_grid subvps.(3) 2 2 in
  let subsubsubvps = V.layout_grid subsubvps.(3) 2 2 in
  let colors = [| Color.red; Color.blue; Color.green; Color.black |] in
  let trace i vp =
    V.set_rel_line_width vp 1.;

    V.set_global_color vp colors.(i mod 4);
    V.rectangle vp ~x:0. ~y:0. ~w:1. ~h:1.;
    V.stroke vp V.Graph;

    V.set_line_width vp 1.;

    V.set_global_color vp colors.((i + 2) mod 4);
    V.rectangle vp ~x:0. ~y:0. ~w:1. ~h:1.;
    V.stroke vp V.Orthonormal;

    V.set_global_color vp colors.((i + 1) mod 4);
    V.move_to vp ~x:0. ~y:0.;
    V.line_to vp ~x:1. ~y:0.;
    V.line_to vp ~x:1. ~y:1.;
    V.line_to vp ~x:0. ~y:1.;
    V.line_to vp ~x:0. ~y:0.;
    V.stroke vp V.Device
  in
  Array.iteri trace (Array.concat [subvps; subsubvps; subsubsubvps]);

  V.close subvps.(0);

  V.move_to vp 0.3 0.;
  V.line_to vp 0.3 1.;
  V.set_global_color vp Color.yellow;
  V.stroke vp V.Device;

  V.set_global_color vp Color.red;
  V.set_rel_line_width vp 1.;
  V.rectangle vp ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke vp V.Graph;

  V.close vp

let () =
  try
    List.iter draw [ "cairo PNG test_layout.png"(*;
                     "graphics hold";
                     "tikz backend_path.tex"*)
                   ]
  with Backend.Error e as exn ->
    Printf.printf "Backend.Error %s\n%!" (Backend.string_of_error e);
    raise exn


(* Local Variables: *)
(* compile-command: "make -kB backend_path.exe" *)
(* End: *)
