open Archimedes
module B = Backend
module A = Axes
module H = Handle

let () =
  let f s =
    Printf.printf "%s\n***************************\n%!" s;
    try
      let handle = H.make ~dirs:[ "../src"; "./src"] s 400. 400. in
      let vps = H.Viewport.matrix handle 2 2 in
      H.use (vps.(0).(0));
      let parabola x = x *. x in
      H.f handle ~color:Color.blue parabola (-3.) 3.;
      H.use (vps.(1).(0));
      H.set_color handle Color.red;
      let finish handle =
        H.line_to handle 3. 10.;
        H.line_to handle (-3.) 10.;
        H.close_path handle;
        H.fill handle
      in
      H.f handle ~finish parabola (-3.) 3.;
      H.use (vps.(0).(1));
      H.set_color handle Color.green;
      let finish handle =
        H.line_to handle 3. (-1.);
        H.line_to handle (-3.) (-1.);
        H.close_path handle;
        H.stroke handle
      in
      H.f handle ~finish parabola (-3.) 3.;
      H.use (vps.(1).(1));
      let xaxis =
        H.make_xaxis (`P "|") `Number CB (`P "tic_up") (`Linear(7,0))
      in
      let yaxis =
        H.make_yaxis (`P "-") `Number LC (`P "tic_left") (`Linear(7,2))
      in
      let axes = H.make_axes (`Rectangle(true,true)) xaxis yaxis in
      ignore
        (H.print_axes handle axes {A.x1 = -3.;x2 = 3.;y1=0.;y2 = 9.});
      H.f handle parabola (-3.) 3.;
      H.close handle
    with
      B.Error e ->
        print_string (B.string_of_error e);
        exit 1
  in
  try f (Sys.argv.(1))
  with _ -> List.iter f
    ["tikz functions.tex";
     "graphics";
     "cairo PNG functions.png";
     "cairo PDF functions.pdf";
    ]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_function.com -I ../src dynlink.cmxa bigarray.cmxa archimedes.cmxa test_function.ml && ocamlc -o test_function.exe -I ../src dynlink.cma bigarray.cma archimedes.cma test_function.ml"*)
(*End:*)
