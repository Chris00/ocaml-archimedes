open Archimedes
module B = Backend
module A = Axes
module F = Functions
module C = Coord_handler

let () =
  let f s =
    try
      let cr = B.make ~dirs:[ "../src"; "./src"] s 600. 600. in
      let cr = C.use cr in
      let h = C.get_handle cr in
      C.add_scale cr "unit" 1. 1.;
      Printf.printf "*%!";
      C.set_coordinate cr "unit";
      Printf.printf "ù%!";
      let make_fun x y scx scy name color=
        Printf.printf "*%!";
        C.add_translate cr name x y;
        Printf.printf "-%!";
        C.scale cr ~name ~x:scx ~y:scy;
        C.set_coordinate cr name;
        Printf.printf "-%!";
        C.set_color cr color;
        F.plot cr (fun x -> x *. x) (-3.) 3.;
        C.stroke_init cr;
      in
      let c color = Color.add (Color.make ~a:0. 0. 0. 0.) color in
      let xaxis = A.make_axis (`P "|") (`P "tic_up") `Linear
        (A.Automatic) `Numbers
      and yaxis = A.make_axis (`P "-") (`P "tic_left") `Linear
        (A.Semi_automatic 2.2) `Numbers in
      let axes = A.make (`Two_lines(0.,0.)) xaxis yaxis in
      make_fun 200. 200. 20. 20. "t0" (c Color.blue);
      let h' = C.get_handle cr in
      Printf.printf "-%!";
      C.set_line_width cr 0.5;
      C.set_color cr Color.black;
      Printf.printf "-%!";
      A.print axes (-3.) 3. 0. 9. cr;
      Printf.printf "-%!";
      C.set_coordinate cr "unit";
      Unix.sleep 5;
      Printf.printf "$%!";
      (*C.close cr*)
      C.show_text cr 0. 500. 500. B.LB "Du texte.";
      B.show_text h 0. 100. 100. B.RT "Du texte.";
      B.close h'
    with
      C.Error e ->
        print_string (C.string_of_error e);
        exit 1
    | B.Error e ->
        print_string (B.string_of_error e);
        exit 1
  in List.iter f
       ["graphics";"tikz axes.tex";"cairo PDF axes.pdf";"cairo PNG axes.png"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_axes.com -I ../src unix.cmxa dynlink.cmxa bigarray.cmxa archimedes.cmxa test_axes.ml && ocamlc -o test_axes.exe -I ../src unix.cma dynlink.cma bigarray.cma archimedes.cma test_axes.ml"*)
(*End:*)
