include Testing
module B = Archimedes.Backend
module Matrix = Archimedes.Matrix

let draw bk =
  let backend = B.make ~dirs bk 300. 300. in
  B.scale backend 1. 2.;
  let matrix = B.get_matrix backend in
  let inv_matrix = Matrix.copy matrix in
  Matrix.invert inv_matrix;
  B.set_font_size backend 14.;
  let text = "Test with a y."  in
  let extents = B.text_extents backend text in
  (* let wx,wy = Matrix.inv_transform_distance matrix rect.w 0.
     and hx,hy = Matrix.inv_transform_distance matrix 0. rect.h in
     let x',y' = Matrix.inv_transform_distance matrix rect.x rect.y in*)
  let rect =
    Matrix.transform_rectangle ~dist_basepoint:true inv_matrix extents
  in
  let w = 140. and h = 40. in
  let w' = 2.*.w and h' = 2.*.h in
  let plot_text (x,y,pos,r,g,b) =
    let dx =  match pos with
      | B.CC | B.CT | B.CB -> x -. rect.Matrix.w *. 0.5
      | B.RC | B.RT | B.RB -> x
      | B.LC | B.LT | B.LB -> x -. rect.Matrix.w
    and dy = match pos with
      | B.CC | B.RC | B.LC -> y -. rect.Matrix.h *. 0.5
      | B.CT | B.RT | B.LT -> y
      | B.CB | B.RB | B.LB -> y -. rect.Matrix.h
    in
    B.move_to backend dx dy;
    B.arc backend 1. 0. 7.;
    B.set_color backend (Archimedes.Color.rgb r g b);
    B.stroke backend;
    B.rectangle backend dx dy rect.Matrix.w rect.Matrix.h;
    (*B.line_to backend x y;*)
    B.stroke backend;
    B.move_to backend x y;
    B.arc backend 2. 0. 7.;
    B.stroke backend;
    B.set_color backend (Archimedes.Color.rgba r g b 0.7);
    B.show_text backend 0. x y pos text;
    B.set_color backend (Archimedes.Color.rgba r g b 0.4);
    B.move_to backend (dx -. rect.Matrix.x) (dy -. rect.Matrix.y);
    B.rel_line_to backend rect.Matrix.w 0.;
    B.stroke backend
  in
  let x,y = 10.,20. in
  List.iter plot_text
    [ x,     y,     B.RT, 1.,  0.,  0.;
      x,     y+.h,  B.RC, 0.8, 0.5, 0.;
      x,     y+.h', B.RB, 0.8, 0.,  0.5;

      x+.w,  y,     B.CT, 0.5, 0.8, 0.;
      x+.w,  y+.h,  B.CC, 0.,  1.,  0.;
      x+.w,  y+.h', B.CB, 0.,  0.8, 0.5;

      x+.w', y,     B.LT, 0.5, 0.,  0.8;
      x+.w', y+.h,  B.LC, 0.,  0.5, 0.8;
      x+.w', y+.h', B.LB, 0.,  0.,  1.
    ];
  B.close backend

