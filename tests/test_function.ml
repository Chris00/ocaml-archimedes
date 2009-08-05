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
      A.make_axes layer ~color_labels:(Color.make 0. 0. 0.)
        (Axes.Graph(6,1)) (Axes.Graph(6,3))
        (Axes.Two_lines(-3.,0.,Axes.Line 0.2, Axes.Line 0.2));
      Layer.flush_backend
        ~autoscale:(Layer.Not_allowed)
        layer ~ofsx:0. ~ofsy:0. ~width:500. ~height:500. ~pos:B.CC cr;
      Layer.flush_backend
        layer ~ofsx:0. ~ofsy:500. ~width:500. ~height:500. ~pos:B.CC cr;
      Layer.flush_backend
        ~autoscale:(Layer.Free(Layer.Unlimited, Layer.Unlimited))
        layer ~ofsx:500. ~ofsy:0. ~width:500. ~height:500. cr;
      Layer.flush_backend
        ~autoscale:(Layer.Free(Layer.Limited_out 50., Layer.Limited_out 20.))
        layer ~ofsx:500. ~ofsy:500. ~width:500. ~height:500. ~pos:B.CC cr;
      B.close cr
    with
      B.Error e ->
        print_string (B.string_of_error e);
        exit 1
  in List.iter f ["cairo PDF funct.pdf";"cairo PNG funct.png"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_function.com -I ../src dynlink.cmxa bigarray.cmxa archimedes.cmxa test_function.ml && ocamlc -o test_function.exe -I ../src dynlink.cma bigarray.cma archimedes.cma test_function.ml"*)
(*End:*)
