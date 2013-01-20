include Tests_common

module A = Archimedes

let description =
  "Test highest contrast color selection."^
  " Text reading should be comfortable for every background color."

let draw bk =
  let vp = A.init ~w ~h ~dirs bk
  and text = "Quick brown fox"
  (*and text = "Joy"*)
  and n = List.length A.Color.colors in
  let a = 1.5
  and g = 0.25 in
  let rec dim () =
    let t_e = A.Viewport.text_extents vp ~coord:`Device text in
    let w = t_e.A.Matrix.w *. a and h = t_e.A.Matrix.h *. a in
    let cols_max = truncate ((1. -. w *. g) /. (w *. (1. +. g)))
    and rows_max = truncate ((1. -. h *. g) /. (h *. (1. +. g))) in
    let pivot = truncate (sqrt (float n)) in
    let div_up a b = if a mod b = 0 then a / b else a / b + 1 in
    let pivot_up = div_up n pivot in
    if cols_max * rows_max < n then (* Text is too big, shrink it *)
      (A.Viewport.set_font_size vp (A.Viewport.get_font_size vp -. 1.);
       if A.Viewport.get_font_size vp < 1. then
         failwith "too many colors to test";
       dim ())
    else if pivot <= cols_max && pivot_up <= rows_max then pivot, pivot_up
    else if pivot_up <= cols_max && pivot <= rows_max then pivot_up, pivot
    else if pivot > cols_max then cols_max, min (div_up n cols_max) rows_max
    else min (div_up n rows_max) cols_max, rows_max
  in
  let cols, rows = dim () in
  let w = 1. /. (float cols +. float (cols - 1) *. g)
  and h = 1. /. (float rows +. float (rows - 1) *. g) in
  let p = A.Path.make () in
  ignore (List.fold_left (fun (col, row) color ->
    let x = float col *. (w +. w *. g)
    and y = float row *. (h +. h *. g) in
    A.Path.clear p;
    A.Path.rectangle p x y w h;
    A.Viewport.set_color vp color;
    A.Viewport.fill vp `Device p;
    A.Viewport.set_color vp (A.Color.highest_contrast_bw color);
    A.Viewport.text vp ~coord:`Device (x +. w /. 2.) (y +. h /. 2.) text;
    if col = cols - 1 then (0, row + 1) else (col + 1, row)
  ) (0, 0) A.Color.colors);
  A.close vp;
