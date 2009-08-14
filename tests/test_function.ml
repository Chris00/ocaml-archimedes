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
        C.set_line_width cr 0.5;
        C.set_color cr Color.black;
        Printf.printf "-%!";
(*
        let xaxis = A.make_axis (`P "|") (`P "tic_up") `Linear
          (A.Fixed(6,1)) `Numbers
        and yaxis = A.make_axis (`P "-") (`P "tic_left") `Linear
          (A.Fixed(6,3)) `Numbers in
        let axes = A.make (`Two_lines(0.,0.)) xaxis yaxis in
        Printf.printf "-%!";
        A.print axes (-3.) 3. 0. 9. cr;
        Printf.printf "-%!";
        C.set_line_width cr 1.;*)
        C.set_coordinate cr "unit";
      in
      let c color = Color.add (Color.make ~a:0. 0. 0. 0.) color in
      make_fun 150. 0. 1. 1. "t0" (c Color.blue);
      make_fun 450. 0. 50. 30. "t1" (c Color.green);
      make_fun 150. 300. 20. 20. "t2" (c Color.yellow);
      make_fun 450. 300. 30. 30. "t3" (c Color.red);
      (* B.move_to h 0. 500.;
         B.line_to h 1000. 500.;
         B.stroke h;*)
      Unix.sleep 10;
      Printf.printf "$%!";
      (*C.close cr*)
      B.close h
    with
      C.Error e ->
        print_string (C.string_of_error e);
        exit 1
  in List.iter f ["graphics";"cairo PDF functions.pdf";"cairo PNG functions.png"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_function.com -I ../src dynlink.cmxa bigarray.cmxa archimedes.cmxa test_function.ml && ocamlc -o test_function.exe -I ../src dynlink.cma bigarray.cma archimedes.cma test_function.ml"*)
(*End:*)
