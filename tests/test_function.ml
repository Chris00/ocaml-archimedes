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
      let parabola x = x *. x in
      H.plotfx handle parabola (-3.) 3.;
      H.stroke handle;
      H.use (vps.(1).(0));
      H.set_color handle Color.red;
      H.plotfx handle parabola (-3.) 3.;
      H.stroke handle;
      (*let xaxis =
        A.make_axis (`P "|") `Abscissa B.CB (`P "tic_up") (`Linear(7,0))
      in
      let yaxis =
        A.make_axis (`P "-") `Ordinate B.LC (`P "tic_left") (`Linear(7,2))
      in
      let axes = A.make (`Rectangle(true,true)) xaxis yaxis in*)
     (* H.use (vps.(1).(0));
      H.set_color handle Color.red;
      H.plotfx handle (*~axes*) parabola (-3.) 3.;
      H.stroke handle;
*)
      H.close handle
    with
      B.Error e ->
        print_string (B.string_of_error e);
        exit 1
  in List.iter f
       ["graphics";
        "tikz functions.tex";
        "cairo PDF functions.pdf";
        "cairo PNG functions.png"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_function.com -I ../src dynlink.cmxa bigarray.cmxa archimedes.cmxa test_function.ml && ocamlc -o test_function.exe -I ../src dynlink.cma bigarray.cma archimedes.cma test_function.ml"*)
(*End:*)
