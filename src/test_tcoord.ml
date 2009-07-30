module B = Backend
module T = Transform_coord
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
      let coord = Coordinate.create 100. 0. 50. 0. 100. 0. in
      let layer = T.use cr coord in
      T.line_to layer 0. 0.;
      T.line_to layer 1. 0.;
      T.line_to layer 0. 1.;
      T.line_to layer 1. 1.;
      T.close_path layer;
      T.stroke layer;
      (*let layer1 =  in
      Layer.move_to layer1 1. 1.;
      Layer.line_to layer1 2. 2.;
      Layer.line_to layer1 3. 1.;
      Layer.line_to layer1 2. 0.;
      Layer.close_path layer1;
   (*   Layer.curve_to layer1 1. 0. 0. 1. 0. 0.;
      Layer.close_path layer1;*)
      Layer.fill layer1;

      B.set_color cr (Color.color ~a:0.7 0. 0.2 0.7);
      Layer.flush ~autoscale:(Layer.Uniform (Layer.Limited(1.,100.)))
        layer ~ofsx:25. ~ofsy:0. ~width:100. ~height:100. cr;
      (*B.stroke cr;*)
      B.set_color cr (Color.color ~a:0.7 0.9 0.2 0.);
      Layer.flush ~autoscale:(Layer.Free(Layer.Unlimited, Layer.Limited_out 30.))
        layer1 ~ofsx:50. ~ofsy:50. ~width:100. ~height:100. cr;
     (* B.fill cr;*)*)
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
  in List.iter f ["cairo PDF tcoord.pdf";"cairo PNG tcoord.png"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_tcoord.com dynlink.cmxa color.cmx archimedes.cmxa coordinate.cmx transform_coord.cmx test_tcoord.ml && ocamlc -o test_tcoord.exe dynlink.cma color.cmo archimedes.cma coordinate.cmo transform_coord.cmo test_tcoord.ml"*)
(*End:*)
