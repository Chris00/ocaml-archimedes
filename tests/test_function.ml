open Archimedes
module B = Backend
module A = Axes
module H = Handle

let () =
  let f s =
    Printf.printf "%s\n***************************\n%!" s;
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
      H.line_to handle 3. 10.;
      H.line_to handle (-3.) 10.;
      H.close_path handle;
      H.fill handle;
      let xaxis =
        H.make_xaxis (`P "|") `Number CB (`P "tic_up") (`Linear(7,0))
      in
      let yaxis =
        H.make_yaxis (`P "-") `Number LC (`P "tic_left") (`Linear(7,2))
      in
      let axes = H.make_axes (`Rectangle(true,true)) xaxis yaxis in
      H.use (vps.(0).(1));
      H.set_color handle Color.green;
      H.plotfx handle parabola (-3.) 3.;
      H.line_to handle 3. (-1.);
      H.line_to handle (-3.) (-1.);
      H.close_path handle;
      H.stroke handle;
      H.use (vps.(1).(1));
      let vp' =
        H.print_axes axes ~ranges:{A.xmin = -3.;xmax = 3.;ymin=0.;ymax = 9.}
        handle
      in
      (match vp' with
         None -> Printf.printf "No axes%!"
       | Some vp' ->
           H.set_color handle Color.black;
           (*H.use vp';*)
           H.plotfx handle parabola (-3.) 3.);
      H.close handle
    with
      B.Error e ->
        print_string (B.string_of_error e);
        exit 1
  in List.iter f
       ["tikz functions.tex";
        "cairo PDF functions.pdf";
        "graphics";
        "cairo PNG functions.png";
       ]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_function.com -I ../src dynlink.cmxa bigarray.cmxa archimedes.cmxa test_function.ml && ocamlc -o test_function.exe -I ../src dynlink.cma bigarray.cma archimedes.cma test_function.ml"*)
(*End:*)
