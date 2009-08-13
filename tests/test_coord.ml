open Archimedes
module B = Backend
module T = Coord_handler
let () =
  let f s =
    try
      let h = B.make ~dirs:[ "./src";"../src"] s 150. 150. in
      Printf.printf "handle init%!";
      let t = T.use h in
      let h' = T.get_handle t in
      (*It seems that a reference to the backend is necessary --
        otherwise, crash on Win$...*)
      Printf.printf "CH init%!";
      T.add_scale t "unit" 1. 1.;
      Printf.printf "_%!";
      T.set_coordinate t "unit";
      Printf.printf "Coord init%!";
      (* T.add_transform t "tr1"
         {B.xx= 100.; xy=0.; x0=50.; yx= 0.; yy= 100.; y0= 0.};*)
      (*I have to see why that previous command does not work...*)
      T.add_scale t "tr1" 100. 100.;
      T.translate t ~name:"tr1" ~x:0.5 ~y:0.;
      Printf.printf "_%!";
      T.set_coordinate t "tr1";
      Printf.printf "Coord init%!";
      T.set_color t (Color.make ~a:0.7 0. 0.2 0.7);
      Printf.printf "%s%!" (T.print_coordinate t);
      T.print_matrix t;
      T.move_to t 0. 0.;
      T.line_to t 1. 0.;
      T.line_to t 0. 1.;
      T.line_to t 1. 1.;
      T.close_path t;
      T.stroke_init t;
      T.set_coordinate t "unit";
  (*    T.add_transform t "tr2"
        {B.xx= 25.; xy=0.; x0=50.; yx= 0.; yy= (-25.); y0= 120.};*)
      T.add_translate t "tr2" 50. 120.;
      T.scale t ~name:"tr2" ~x:25. ~y:(-25.);
      T.set_coordinate t "tr2";
      Printf.printf "%s%!" (T.print_coordinate t);
      T.print_matrix t;
      T.set_color t (Color.make ~a:0.7 0.9 0. 0.2);
      T.move_to t 1. 1.;
      T.line_to t 2. 2.;
      T.line_to t 3. 1.;
      T.line_to t 2. 0.;
      T.close_path t;
      T.fill t;
      B.close h'
    with
      T.Error e ->
        Printf.eprintf "%s" (T.string_of_error e);
        exit 1
    | B.Error e ->
        Printf.eprintf "%s" (B.string_of_error e);
        exit 1
  in List.iter f ["graphics";"cairo PDF coord.pdf";(*"cairo PNG coord.png"*)]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_coord.com -I ../src unix.cmxa dynlink.cmxa bigarray.cmxa archimedes.cmxa test_coord.ml && ocamlc -g -o test_coord.exe unix.cma dynlink.cma bigarray.cma -I ../src archimedes.cma test_coord.ml"*)
(*End:*)
