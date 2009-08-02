module B = Backend
module Coord = Backend.Coordinate

type t =
    {h:B.t; mutable c:Backend.matrix; s:Backend.matrix Stack.t}

let make ?dirs name ?coord width height =
  {h = B.make ?dirs name width height;
   c = (match coord with None -> Coord.identity () | Some c -> c);
   s = Stack.create ()}

let use handle coord = {h = handle; c = coord; s = Stack.create ()}

let use_unit_square handle x1 y1 x2 y2 =
  use handle (Coord.create (x2 -. x1) 0. x1 0. (y2 -. y1) y1)

let get_handle t = t.h


(*Coordinate transformations are performed by the Backend.matrix part*)
let translate t = Coord.translate t.c
let scale t = Coord.scale t.c
(*let rotate t = Coord.rotate t.c*)

let change_coord t coord = t.c <- coord

(*Backend primitives*)

let width t = B.width t.h
let height t = B.height t.h
let close t = B.close t.h
let set_color t = B.set_color t.h
let set_line_width t = B.set_line_width t.h
let set_line_cap t = B.set_line_cap t.h
let set_dash t = B.set_dash t.h
let set_line_join t = B.set_line_join t.h
let get_line_width t = B.get_line_width t.h
let get_line_cap t = B.get_line_cap t.h
let get_dash t = B.get_dash t.h
let get_line_join t = B.get_line_join t.h

(*The operations of drawing need transforming the point first, then
  doing the order*)

let move_to t x y =
  let x', y' = Coord.transform t.c x y in
  B.move_to t.h x' y'

let line_to t x y =
  let x', y' = Coord.transform t.c x y in
  B.line_to t.h x' y'

let rel_move_to t x y =
  let x', y' = Coord.transform_dist t.c x y in
  B.rel_move_to t.h x' y'

let rel_line_to t x y =
  let x', y' = Coord.transform_dist t.c x y in
  B.rel_line_to t.h x' y'

let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  let x1, y1 = Coord.transform t.c x1 y1
  and x2, y2 = Coord.transform t.c x2 y2
  and x3, y3 = Coord.transform t.c x3 y3 in
  B.curve_to t.h ~x1 ~y1 ~x2 ~y2 ~x3 ~y3

(*FIXME: these two primitives do not translate well in Backend.t
  coordinates -- due to scaling*)
let rectangle t ~x ~y ~w ~h =
  let x, y = Coord.transform t.c x y
  and w, w' = Coord.transform_dist t.c w 0.
  and h', h = Coord.transform_dist t.c 0. h in
  if Coord.has_shear t.c then
    (B.move_to t.h x y;
     B.rel_line_to t.h w w';
     B.rel_line_to t.h h' h;
     B.rel_line_to t.h (-.w) (-.w');
     B.close_path t.h)
  else B.rectangle t.h x y w h
(*
  let arc t = B.arc t*)

let close_path t = B.close_path t.h
let path_extents t = B.path_extents t.h
let stroke t = B.stroke t.h
let stroke_preserve t = B.stroke_preserve t.h
let fill t = B.fill t.h
let fill_preserve t = B.fill_preserve t.h
(*let clip t = B.clip t.h
let clip_preserve t = B.clip_preserve t.h*)
let clip_rectangle t = B.clip_rectangle t.h

let save t =
  B.save t.h;
  Stack.push (Coord.copy t.c) t.s

let restore t =
  t.c <- Stack.pop t.s;
  B.restore t.h
let select_font_face t = B.select_font_face t.h
let set_font_size t = B.set_font_size t.h
let show_text t ~rotate ~x ~y =
  let x,y = Coord.transform t.c x y in
  (*FIXME: need to know how t.c rotates, to get a better rotation of text*)
  B.show_text t.h ~rotate ~x ~y
(*let text_extents t = B.text_extents t.h*)

let make_axes t  ?color_axes ?color_labels xmin xmax ymin ymax
    datax datay mode =
  (match color_axes with
     Some c -> save t; set_color t c
   | None -> ());
  (*Axes*)
  let ofsx, ofsy, ticx, ticy =
    match mode with
      Axes.Rectangle(tx,ty) ->
        rectangle t xmin ymin (xmax -. xmin) (ymax -. ymin);
        xmin, ymin, tx, ty
    | Axes.Two_lines(x,y,tx,ty) ->
        move_to t xmin y;
        line_to t xmax y;
        move_to t x ymin;
        line_to t x ymax;
        x,y, tx, ty
  in
  (*Tics or like*)
  let tic x' y' x_axis ticstyle =
    match ticstyle with
      Axes.Line(r) ->
        move_to t x' y';
        (*FIXME: tic have to be independent of zoom:
          using [Backend] mode for stroking*)
        let x,y =
          if x_axis then
            (rel_line_to t 0. (-.r/.2.);
             rel_line_to t 0. r;
             x',(y'+.r))
          else (*y_axis*)
            (rel_line_to t (-.r/.2.) 0.;
             rel_line_to t r 0.;
             (x'-.r),y')
        in
        (match color_labels with
           Some c ->
             save t;
             set_color t c
         | None -> ());
        show_text t ~rotate:0.
          ~x ~y (if x_axis then B.CB else B.LC)
          (string_of_float (if x_axis then x' else y'));
        (match color_labels with
           Some c -> restore t;
         | None -> ())
  in
  let make_data data ticmode x_axis =
    match data with
      Axes.Graph(major,minor) ->
        let step =
          let diff = if x_axis then xmax -. xmin else ymax -. ymin in
          diff /. (float major) in
        for i = 0 to major do (*major tics in X axis*)
          let ofs = (float i) *. step in
          let x, y =
            if x_axis then xmin +. ofs, ofsy
            else ofsx, ymin +. ofs
          in
          (*Tic to put, centered in (x, y), with label 'x' or 'y' as
            given by x_axis.*)
          tic x y x_axis ticmode
        done
  in
  (*Make data for X axis*)
  make_data datax ticx true;
  (*Make data for Y axis*)
  make_data datay ticy false;
  stroke t;
  (match color_axes with
     Some c -> restore t
   | None -> ())


(*Local Variables:*)
(*compile-command: "ocamlopt -c transform_coord.ml && ocamlc -c transform_coord.ml"*)
(*End:*)
