open Archimedes
module B = Backend

let () =
  let f s =
    try
      let cr =
        B.make ~dirs:[ "../src"; "./src"] s 300. 300.
      in
      B.scale cr 1. 2.;
      B.set_font_size cr 40.;
      let text = "Test with a y."  in
      let rect = B.text_extents cr text in
      let w = 140. and h = 40. in
      let w' = 2.*.w and h' = 2.*.h in
      let plot_text (x,y,pos,r,g,b) =
        let dx =  match pos with
          | CC | CT | CB -> x -. rect.B.w *. 0.5
          | RC | RT | RB -> x
          | LC | LT | LB -> x -. rect.B.w
        and dy = match pos with
          | CC | RC | LC -> y -. rect.B.h *. 0.5
          | CT | RT | LT -> y
          | CB | RB | LB -> y -. rect.B.h
        in
        (*let wx,wy = Matrix.inv_transform_distance matrix rect.B.w 0.
        and hx,hy = Matrix.inv_transform_distance matrix 0. rect.B.h in*)
        let wx = rect.B.w
        and hy = rect.B.h in
        B.arc cr dx dy 1. 0. 7.;
        B.set_color cr (Color.make r g b);
        B.stroke cr;
        B.rectangle cr dx dy wx hy;
        (*B.line_to cr x y;*)
        B.stroke cr;
        B.arc cr x y 2. 0. 7.;
        B.stroke cr;
        B.show_text cr 0. x y pos text
      in
      let x,y = 10.,20. in
      List.iter plot_text
        [
          x,     y,     RT, 1.,  0.,  0.;
          x,     y+.h,  RC, 0.8, 0.5, 0.;
          x,     y+.h', RB, 0.8, 0.,  0.5;

          x+.w,  y,     CT, 0.5, 0.8, 0.;
          x+.w,  y+.h,  CC, 0.,  1.,  0.;
          x+.w,  y+.h', CB, 0.,  0.8, 0.5;

          x+.w', y,     LT, 0.5, 0.,  0.8;
          x+.w', y+.h,  LC, 0.,  0.5, 0.8;
          x+.w', y+.h', LB, 0.,  0.,  1.
        ];
      B.close cr
    with
      B.Error e ->
        print_string (B.string_of_error e);
        exit 1
  in List.iter f
       ["cairo PDF text.pdf";
        "cairo PNG text.png";
        "tikz text.tex draw";
        "graphics"]

(*Local Variables:*)
(*compile-command: "ocamlopt -o text.com -I ../src dynlink.cmxa bigarray.cmxa archimedes.cmxa text.ml && ocamlc -o text.exe -I ../src dynlink.cma bigarray.cma archimedes.cma text.ml"*)
(*End:*)
