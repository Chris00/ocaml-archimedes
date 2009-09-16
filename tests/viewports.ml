open Archimedes
module B = Backend
module A = Axes
module H = Handle

let () =
  let f s =
    try
      let handle = H.make ~dirs:[ "../src"; "./src"] s 600. 600. in
      let vps = H.Viewport.matrix handle 2 2 in
      H.use (vps.(0).(0));
      H.set_color handle Color.blue;
      H.move_to handle 0. 0.;
      H.line_to handle 1. 0.;
      H.line_to handle 1. 1.;
      H.close_path handle;
      H.stroke handle;
      H.use (vps.(1).(0));
      H.set_color handle Color.red;
      H.move_to handle 0. 0.;
      H.line_to handle 1. 0.;
      H.line_to handle 1. 1.;
      H.line_to handle 0. 1.;
      H.close_path handle;
      H.stroke handle;
      H.use (vps.(0).(1));
      H.set_color handle Color.green;
      let two_pi = 8. *. atan 1. in
      H.move_to handle 1. 0.;
      H.arc handle 1. 0. two_pi;
      H.fill_preserve handle;
      H.set_color handle Color.magenta;
      H.stroke handle;
      H.set_rel_line_width handle 10.;
      H.move_to handle (-0.8) 0.;
      H.line_to handle 0.8 0.;
      H.set_color handle Color.white;
      H.stroke handle;
      H.use vps.(1).(1);
      H.set_color handle Color.yellow;
      H.move_to handle 0. 0.;
      H.line_to handle 0. 1.;
      H.line_to handle 1. 0.5;
      H.close_path handle;
      H.set_line_width handle 5.;
      H.fill_preserve handle;
      H.set_color handle Color.magenta;
      H.stroke handle;
      H.close handle
    with
      B.Error e ->
        print_string (B.string_of_error e);
        exit 1
  in
  try f (Sys.argv.(1))
  with _ -> List.iter f
    ["graphics";
     "tikz viewports.tex";
     "cairo PDF viewports.pdf";
     "cairo PNG viewports.png"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o viewports.com -I ../src dynlink.cmxa bigarray.cmxa archimedes.cmxa viewports.ml && ocamlc -o viewports.exe -I ../src dynlink.cma bigarray.cma archimedes.cma viewports.ml"*)
(*End:*)
