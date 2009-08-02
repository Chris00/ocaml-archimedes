module B = Backend
module T = Transform_coord
module A = Axes.Print(T)
let () =
  let f s =
    try
      let coord = B.Coordinate.create 100. 0. 50. 0. 100. 0. in
      let t = T.make ~dirs:[".";
                            "C:\\Program Files\\Objective Caml\\lib";
                            "C:\\Program Files\\Objective Caml\\lib\\site-lib\\cairo2"]
        s ~coord 150. 150. in
      T.set_color t (Color.color ~a:0.7 0. 0.2 0.7);
      T.line_to t 0. 0.;
      T.line_to t 1. 0.;
      T.line_to t 0. 1.;
      T.line_to t 1. 1.;
      T.close_path t;
      T.stroke t;
      T.set_matrix t (B.Coordinate.create 25. 0. 50. 0. (-25.) 120.);
      T.set_color t (Color.color ~a:0.7 0.9 0. 0.2);
      T.move_to t 1. 1.;
      T.line_to t 2. 2.;
      T.line_to t 3. 1.;
      T.line_to t 2. 0.;
      T.close_path t;
      T.fill t;
      A.make_axes t 0. 3. 0. 2. (Axes.Graph(6,1)) (Axes.Graph(4,1))
        (Axes.Rectangle(Axes.Line 0.2, Axes.Line 0.2));
      T.close t
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
(*compile-command: "ocamlopt -o test_tcoord.com dynlink.cmxa color.cmx archimedes.cmxa axes.cmx transform_coord.cmx test_tcoord.ml && ocamlc -o test_tcoord.exe dynlink.cma color.cmo archimedes.cma axes.cmo transform_coord.cmo test_tcoord.ml"*)
(*End:*)
