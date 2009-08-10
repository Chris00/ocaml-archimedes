open Archimedes
module B = Backend
module A = Axes
module F = Functions

let () =
  let f s =
    try
      let cr =
        B.make ~dirs:[ "../src"; "./src"] s 1000. 1000.
      in
      let layer = Layer.make () in
      Layer.set_color layer (Color.make 0. 0. 1.);
      F.stroke_plot layer (fun x -> x *. x) (-3.) 3.;
      Layer.set_line_width layer 0.5;
      let def_axes = A.make_default
        ~mode:(A.Default.Two_lines(-3.,0.))
        (A.Default.Graph(6,1)) (A.Default.Graph(6,3))
      in
      A.print_axes def_axes ~color_labels:(Color.make 0. 0. 0.) layer;
      let width = 500. and height = 500. in
      let ofsx = 0. and ofsy = 0. in
      let x2 = 500. and y2 = 500. in
      Layer.flush_backend
        ~autoscale:(Layer.Not_allowed)
        layer ~ofsx ~ofsy ~width ~height cr;
      Layer.flush_backend
        layer ~ofsx:x2 ~ofsy ~width ~height cr;
      Layer.flush_backend
        ~autoscale:(Layer.Free(Layer.Unlimited, Layer.Unlimited))
        layer ~ofsx ~ofsy:y2 ~width ~height cr;
      Layer.flush_backend
        ~autoscale:(Layer.Free(Layer.Limited_out 50., Layer.Limited_out 20.))
        layer ~ofsx:x2 ~ofsy:y2 ~width ~height cr;
      B.close cr
    with
      B.Error e ->
        print_string (B.string_of_error e);
        exit 1
  in List.iter f ["cairo PDF funct.pdf";"cairo PNG funct.png"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_function.com -I ../src dynlink.cmxa bigarray.cmxa archimedes.cmxa test_function.ml && ocamlc -o test_function.exe -I ../src dynlink.cma bigarray.cma archimedes.cma test_function.ml"*)
(*End:*)
