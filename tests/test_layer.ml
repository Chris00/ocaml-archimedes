open Archimedes
module B = Backend
(*module A = Axes.Print(Layer)*)

let () =
  let f s =
    try
      let cr =
        B.make ~dirs:[ "../src"] s 150. 150.
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
      Layer.set_color layer1 (Color.make 0. 0. 0.);
      Layer.set_line_width layer1 0.5;
 (*     let rect = Layer.layer_extents layer1 in
      let xmin, ymin = rect.B.x, rect.B.y in
      let xmax, ymax = rect.B.w +. xmin, rect.B.h +. ymin in
      A.make_axes layer1 ~color_labels:(Color.make 1. 0. 0.)
        xmin xmax ymin ymax
        (Axes.Graph(6,1)) (Axes.Graph(4,1))
        (Axes.Two_lines(0.,0.,Axes.Line 0.2, Axes.Line 0.2));*)

      B.set_color cr (Color.make ~a:0.7 0. 0.2 0.7);
      Layer.flush ~autoscale:(Layer.Uniform (Layer.Limited(1.,100.)))
        layer ~ofsx:25. ~ofsy:0. ~width:100. ~height:100. cr;
      (*B.stroke cr;*)
      B.set_color cr (Color.make ~a:0.7 0.9 0.2 0.);
      Layer.flush ~autoscale:(Layer.Free(Layer.Unlimited, Layer.Limited_out 30.))
        layer1 ~ofsx:50. ~ofsy:120. ~width:100. ~height:(-100.) cr;
     (* B.fill cr;*)
      B.close cr
    with
      B.Error e ->
        print_string (B.string_of_error e);
        exit 1
  in List.iter f ["cairo PDF layer.pdf";"cairo PNG layer.png"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o layer.com -I ../src dynlink.cmxa archimedes.cmxa axes.cmx layer.cmx test_layer.ml && ocamlc -o layer.exe -I ../src dynlink.cma archimedes.cma axes.cmo layer.cmo test_layer.ml"*)
(*End:*)
