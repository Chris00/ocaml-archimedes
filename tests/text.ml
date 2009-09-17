open Archimedes
module B = Backend

let () =
  let f s =
    try
      let backend =
        B.make ~dirs:[ "../src"; "./src"] s 300. 300.
      in
      B.set_matrix backend (B.backend_to_device backend);
      B.scale backend 1. 2.;
      let matrix = B.get_matrix backend in
      B.set_font_size backend 14.;
      let text = "Test with a y."  in
      let extents = B.text_extents backend text in
     (* let wx,wy = Matrix.inv_transform_distance matrix rect.w 0.
      and hx,hy = Matrix.inv_transform_distance matrix 0. rect.h in
      let x',y' = Matrix.inv_transform_distance matrix rect.x rect.y in*)
      let rect =
        Matrix.inv_transform_rectangle ~dist_basepoint:true matrix extents
      in
      let w = 140. and h = 40. in
      let w' = 2.*.w and h' = 2.*.h in
      let plot_text (x,y,pos,r,g,b) =
        let dx =  match pos with
          | CC | CT | CB -> x -. rect.w *. 0.5
          | RC | RT | RB -> x
          | LC | LT | LB -> x -. rect.w
        and dy = match pos with
          | CC | RC | LC -> y -. rect.h *. 0.5
          | CT | RT | LT -> y
          | CB | RB | LB -> y -. rect.h
        in
        B.move_to backend dx dy;
        B.arc backend 1. 0. 7.;
        B.set_color backend (Color.make r g b);
        B.stroke backend;
        B.rectangle backend dx dy rect.w rect.h;
        (*B.line_to backend x y;*)
        B.stroke backend;
        B.move_to backend x y;
        B.arc backend 2. 0. 7.;
        B.stroke backend;
        B.set_color backend (Color.make ~a:0.4 r g b);
        B.show_text backend 0. x y pos text;
        B.move_to backend (dx -. rect.x) (dy -. rect.y);
        B.rel_line_to backend rect.w 0.;
        B.stroke backend
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
      B.close backend
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
