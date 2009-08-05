open Archimedes
module B = Backend
module A = Axes
module F = Functions

let () =
  let f s =
    try
      let cr =
        B.make ~dirs:[ "../src"; "./src"] s 250. 150.
      in
      let layer = Layer.make () in
      Layer.set_color layer (Color.make 0. 0. 1.);
      F.stroke_plot layer (fun x -> x *. x) (-3.) 3.;
      Layer.set_line_width layer 0.5;
      A.make_axes layer ~color_labels:(Color.make 0. 0. 0.)
        (Axes.Graph(6,1)) (Axes.Graph(6,3))
        (Axes.Two_lines(-3.,0.,Axes.Line 0.2, Axes.Line 0.2));
      Layer.flush_backend layer ~ofsx:25. ~ofsy:15. ~width:120. ~height:120. cr;
      (* B.rectangle cr 0. 0. 20. 20.;*)
      B.fill cr;
      let text = "Text"  in
      let rect = B.text_extents cr text in
      let plot_text (x,y,pos,r,g,b) =
        B.set_color cr (Color.make r g b);
        let dx =  match pos with
          | B.CC | B.CT | B.CB ->  rect.B.x +. x -. rect.B.w *. 0.5
          | B.RC | B.RT | B.RB -> rect.B.x +. x
          | B.LC | B.LT | B.LB -> rect.B.x +. x -. rect.B.w
        and dy = match pos with
          | B.CC | B.RC | B.LC -> rect.B.y +. y -. rect.B.h *. 0.5
          | B.CT | B.RT | B.LT -> rect.B.y +. y
          | B.CB | B.RB | B.LB -> rect.B.y +. y -. rect.B.h
        in
        B.rectangle cr dx dy rect.B.w rect.B.h;
        B.stroke cr;
        B.show_text cr 0. x y pos text
      in
      List.iter plot_text 
        [
        120.,20.,B.RT,1.,0.,0.;120.,40.,B.RC,0.7,0.5,0.;120.,60.,B.RT,0.7,0.,0.5;
        160.,20.,B.CT,0.5,0.7,0.;160.,40.,B.CC,0.,1.,0.;160.,60.,B.CT,0.,0.7,0.5;
        200.,20.,B.LT,0.5,0.,0.7;200.,40.,B.LC,0.,0.5,0.7;200.,60.,B.LT,0.,0.,1.
        ];
        B.close cr
    with
      B.Error e ->
        print_string (B.string_of_error e);
        exit 1
  in List.iter f ["cairo PDF funct.pdf";"cairo PNG funct.png"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_function.com -I ../src dynlink.cmxa bigarray.cmxa archimedes.cmxa test_function.ml && ocamlc -o test_function.exe -I ../src dynlink.cma bigarray.cma archimedes.cma test_function.ml"*)
(*End:*)
