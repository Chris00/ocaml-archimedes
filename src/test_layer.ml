module B = Backend

let () =
  let f s =
    try
      let cr =
        B.make ~dirs:[".";
                      "C:\\Program Files\\Objective Caml\\lib";
                      "C:\\Program Files\\Objective Caml\\lib\\site-lib\\cairo2"]
          s 150. 150.
      in
     (* B.set_color cr (Color.color ~a:0.7 0.9 0. 0.2);*)
      let layer = Layer.make () in
      Layer.line_to layer 0. 0.;
      Layer.line_to layer 1. 0.;
      Layer.line_to layer 0. 1.;
      Layer.line_to layer 1. 1.;
      (*Layer.line_to layer 0. 0.;*)
      Layer.close_path layer;
      (*Layer.set_line_width layer 0.05;*)
      Layer.stroke layer;
      let layer1 = Layer.make () in
      Layer.move_to layer1 1. 1.;
      Layer.line_to layer1 2. 2.;
      Layer.line_to layer1 3. 1.;
      Layer.line_to layer1 2. 0.;
      Layer.close_path layer1;
   (*   Layer.curve_to layer1 1. 0. 0. 1. 0. 0.;
      Layer.close_path layer1;*)
      Layer.fill layer1;
      Layer.set_color layer1 (Color.color 0. 0. 0.);
      Layer.set_line_width layer1 0.5;
      Layer.make_axes layer1 ~color_labels:(Color.color 1. 0. 0.)
        (Axes.Graph(6,1)) (Axes.Graph(4,1))
        (Axes.Two_lines(0.,0.,Axes.Line 0.2, Axes.Line 0.2));

      B.set_color cr (Color.color ~a:0.7 0. 0.2 0.7);
      Layer.flush ~autoscale:(Layer.Uniform (Layer.Limited(1.,100.)))
        layer ~ofsx:25. ~ofsy:0. ~width:100. ~height:100. cr;
      (*B.stroke cr;*)
      B.set_color cr (Color.color ~a:0.7 0.9 0.2 0.);
      Layer.flush ~autoscale:(Layer.Free(Layer.Unlimited, Layer.Limited_out 30.))
        layer1 ~ofsx:50. ~ofsy:120. ~width:100. ~height:(-100.) cr;
     (* B.fill cr;*)
      B.close cr
    with
      B.Error e ->
        Printf.printf "Error of backend: \n%s"
          (match e with
             B.Corrupted_dependency(string) -> "Corrupted dep\n"^string
           | B.Non_loadable_dependency(string) -> "NLD"^string
           | B.Nonexistent(string) -> "NE"^string
           | B.Corrupted(string) -> "CORR"^string
           | B.Not_registering(string) -> "NR"^string);
        exit 1
  in List.iter f ["cairo PDF layer.pdf";"cairo PNG layer.png"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_layer.com dynlink.cmxa color.cmx archimedes.cmxa layer.cmx test_layer.ml && ocamlc -o test_layer.exe dynlink.cma color.cmo archimedes.cma layer.cmo test_layer.ml"*)
(*End:*)
