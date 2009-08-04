open Archimedes
module B = Backend
module A = Axes
module F = Functions

let () =
  let f s =
    try
      let cr =
        B.make ~dirs:[ "../src"; "./src"] s 150. 150.
      in
      let layer = Layer.make () in
      F.stroke_plot layer (fun x -> x *. x) (-3.) 3.;
      Layer.set_line_width layer 0.5;
      A.make_axes layer ~color_labels:(Color.make 1. 0. 0.)
        (Axes.Graph(6,1)) (Axes.Graph(6,3))
        (Axes.Two_lines(-3.,0.,Axes.Line 0.2, Axes.Line 0.2));
      Layer.flush_backend layer ~ofsx:25. ~ofsy:0. ~width:120. ~height:120. cr;
      B.close cr
    with
      B.Error e ->
        print_string (B.string_of_error e);
        exit 1
  in List.iter f ["cairo PDF functions.pdf";"cairo PNG functions.png"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_function.com -I ../src dynlink.cmxa bigarray.cmxa archimedes.cmxa test_function.ml && ocamlc -o test_function.exe -I ../src dynlink.cma bigarray.cma archimedes.cma test_function.ml"*)
(*End:*)
