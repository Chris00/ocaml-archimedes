open Archimedes
module B = Backend
module A = Axes
module F = Functions
module C = Coord_handler

let () =
  let f s =
    try
      let cr = C.make ~dirs:[ "../src"; "./src"] s 1000. 1000. in
      let make_fun x y scx scy=
        C.save cr;
        C.add_translate cr "current" x y;
        C.scale cr scx scy;
        C.set_color cr Color.blue;
        F.stroke_plot cr (fun x -> x *. x) (-3.) 3.;
        C.set_line_width cr 0.5;
        let xaxis = A.make_axis (`P "|") (`P "tic_up") `Linear
          (A.Fixed(6,1)) `Numbers
        and yaxis = A.make_axis (`P "-") (`P "tic_left") `Linear
          (A.Fixed(6,3)) `Numbers in
        let axes = A.make (`Two_lines(0.,0.)) xaxis yaxis in
        A.print axes (-3.) 3. 0. 9. cr;
        C.restore cr;
      in
      make_fun 0. 0. 1. 1.;
      make_fun 500. 0. 100. 50.;
      make_fun 0. 500. 50. 50.;
      make_fun 500. 0. 20. 40.;


     (* let width = 500. and height = 500. in
      let ofsx = 0. and ofsy = 0. in
      let x2 = 500. and y2 = 500. in
      Layer.flush_backend
        ~autoscale:(Layer.Not_allowed)
        layer ~ofsx ~ofsy ~width ~height cr;
      Layer.flush_backend
        layer ~ofsx:x2 ~ofsy ~width ~height cr;
      Layer.flush_backend
        ~autoscale:(Layer.Free(Layer.Unlimited, Layer.Unlimited))
        layer ~ofsx ~ofsy:y2 ~width ~height cr;
      Layer.flush_backend
        ~autoscale:(Layer.Free(Layer.Limited_out 50., Layer.Limited_out 20.))
        layer ~ofsx:x2 ~ofsy:y2 ~width ~height cr;*)
      C.close cr
    with
      C.Error e ->
        print_string (C.string_of_error e);
        exit 1
  in List.iter f ["cairo PDF functions.pdf";"cairo PNG functions.png"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_function.com -I ../src dynlink.cmxa bigarray.cmxa archimedes.cmxa test_function.ml && ocamlc -o test_function.exe -I ../src dynlink.cma bigarray.cma archimedes.cma test_function.ml"*)
(*End:*)
