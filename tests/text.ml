open Archimedes
module B = Backend

let () =
  let f s =
    try
      let cr =
        B.make ~dirs:[ "../src"; "./src"] s 250. 150.
      in
      if String.sub s 0 5 = "cairo" then (
        B.scale cr 1. (-1.);
        B.translate cr 0. (-150.));
      let text = "Test with a y."  in
     (* B.scale cr 1. 2.;*)
      let rect = B.text_extents cr text in
      let w = rect.B.w and h = rect.B.h in
      let w = 2. *. w and h = 2.*.h in
      let w' = 2.*.w and h' = 2.*.h in
      let plot_text (x,y,pos,r,g,b) =
        let left_x = x and top_y = y in
        let dx =  match pos with
          | B.CC | B.CT | B.CB -> left_x -. rect.B.w *. 0.5
          | B.RC | B.RT | B.RB -> left_x
          | B.LC | B.LT | B.LB -> left_x -. rect.B.w
        and dy = match pos with
          | B.CC | B.RC | B.LC -> top_y -. rect.B.h *. 0.5
          | B.CT | B.RT | B.LT -> top_y
          | B.CB | B.RB | B.LB -> top_y -. rect.B.h
        in
        B.set_color cr (Color.make ~a:0.6 r g b);
        B.rectangle cr dx dy rect.B.w rect.B.h;
        B.stroke cr;
        B.set_color cr (Color.make r g b);
        B.show_text cr 0. x y pos text;
        B.arc cr x y 2. 0. 7.;
        B.set_color cr (Color.make ~a:0.5 r g b);
        B.stroke cr;
        B.arc cr dx dy 1. 0. 7.;
        B.set_color cr (Color.make ~a:0.3 r g b);
        B.stroke cr
      in
      let x,y = 10.,20. in
      List.iter plot_text
        [
          x,     y,     B.RT, 1.,  0.,  0.;
          x,     y+.h,  B.RC, 0.8, 0.5, 0.;
          x,     y+.h', B.RB, 0.8, 0.,  0.5;

          x+.w,  y,     B.CT, 0.5, 0.8, 0.;
          x+.w,  y+.h,  B.CC, 0.,  1.,  0.;
          x+.w,  y+.h', B.CB, 0.,  0.8, 0.5;

          x+.w', y,     B.LT, 0.5, 0.,  0.8;
          x+.w', y+.h,  B.LC, 0.,  0.5, 0.8;
          x+.w', y+.h', B.LB, 0.,  0.,  1.
        ];
      B.close cr
    with
      B.Error e ->
        print_string (B.string_of_error e);
        exit 1
  in List.iter f
       ["cairo PDF text.pdf";
        "cairo PNG text.png";
        "tikz text.tex";
        "graphics"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o text.com -I ../src dynlink.cmxa bigarray.cmxa archimedes.cmxa text.ml && ocamlc -o text.exe -I ../src dynlink.cma bigarray.cma archimedes.cma text.ml"*)
(*End:*)
