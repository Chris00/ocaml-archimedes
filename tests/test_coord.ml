open Archimedes
module B = Backend
module T = Transform_coord
(*module A = Axes.Print(T)*)
let () =
  let f s =
    try
      let coord = B.Coordinate.create 100. 0. 50. 0. 100. 0. in
      let t = T.make ~dirs:[ "./src";"../src"]
        s ~coord 150. 150. in
      T.set_color t (Color.make ~a:0.7 0. 0.2 0.7);
      T.line_to t 0. 0.;
      T.line_to t 1. 0.;
      T.line_to t 0. 1.;
      T.line_to t 1. 1.;
      T.close_path t;
      T.stroke t;
      T.set_matrix t (B.Coordinate.create 25. 0. 50. 0. (-25.) 120.);
      T.set_color t (Color.make ~a:0.7 0.9 0. 0.2);
      T.move_to t 1. 1.;
      T.line_to t 2. 2.;
      T.line_to t 3. 1.;
      T.line_to t 2. 0.;
      T.close_path t;
      T.fill t;
      (*A.make_axes t 0. 3. 0. 2. (Axes.Graph(6,1)) (Axes.Graph(4,1))
        (Axes.Rectangle(Axes.Line 0.2, Axes.Line 0.2));*)
      T.close t
    with
      B.Error e ->
        print_string(B.string_of_error e);
        exit 1
  in List.iter f ["cairo PDF tcoord.pdf";"cairo PNG tcoord.png"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_coord.com -I ../src dynlink.cmxa archimedes.cmxa axes.cmx transform_coord.cmx test_coord.ml && ocamlc -o test_coord.exe dynlink.cma -I ../src archimedes.cma axes.cmo transform_coord.cmo test_coord.ml"*)
(*End:*)
