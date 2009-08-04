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
     let print s =
        Printf.printf "%s - Matrix: \n%!" s;
        let matrix = B.get_matrix cr in
        let sf = string_of_float in
          Printf.printf "%s%s%s%s%s%s\n%!" (sf matrix.B.xx) 
            (sf matrix.B.xy) (sf matrix.B.x0) (sf matrix.B.yx) 
            (sf matrix.B.yy) (sf matrix.B.y0) in
     (* print "Init";
      B.scale cr 50. 50.;
      print "Scale 50";
      B.translate cr 25. 30.;
      print "Tr 25, 30";
      B.scale cr 0.2 0.2;
      print "Scale 1/50";
      B.translate cr (-0.5) (-.0.6);
      print "Tr -.5 -.6";*)
      let layer = Layer.make () in
      Layer.line_to layer 0. 0.;
      Layer.line_to layer 1. 0.;
      Layer.line_to layer 0. 1.;
      Layer.line_to layer 1. 1.;
      (*Layer.line_to layer 0. 0.;*)
      Layer.close_path layer;
      (*Layer.set_line_width layer 0.05;*)
      Layer.stroke_layer_preserve layer;
      Layer.set_color layer (Color.make 1. 1. 0.);
      Layer.stroke_preserve layer;
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
      print "before flush";
      Layer.flush_backend ~autoscale:(Layer.Uniform (Layer.Limited(1.,50.)))
        layer ~ofsx:25. ~ofsy:0. ~width:100. ~height:100. cr;
      print "after flush";
      B.set_color cr (Color.make ~a:0.7 0.9 0.2 0.6);
      B.fill cr;
      B.arc cr 50. 50. 50. 0. 4.;
      B.close_path cr;
      B.fill cr;
      (*B.stroke cr;*)
      B.set_color cr (Color.make ~a:0.7 0.9 0.2 0.);
      Layer.flush_backend
        ~autoscale:(Layer.Free(Layer.Unlimited, Layer.Limited_out 30.))
        layer1 ~ofsx:50. ~ofsy:120. ~width:100. ~height:(-100.) cr;
      B.fill cr;
      B.close cr
    with
      B.Error e ->
        print_string (B.string_of_error e);
        exit 1
  in List.iter f ["cairo PDF layer.pdf";"cairo PNG layer.png"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o layer.com -I ../src dynlink.cmxa archimedes.cmxa axes.cmx transform_coord.cmx layer.cmx test_layer.ml && ocamlc -o layer.exe -I ../src dynlink.cma archimedes.cma axes.cmo transform_coord.cmo layer.cmo test_layer.ml"*)
(*End:*)
