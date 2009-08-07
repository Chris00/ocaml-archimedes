module B = Backend
module Coord = Backend.Matrix

type styles =
    {
      mutable coord:Backend.matrix; (*Current coordinate transformation to perform*)
      mutable point: Pointstyle.t;(*Current point style*)
    }

type t =
    {handle:B.t; (*Handle on which we operate*)
     mutable styles: styles; (*Current styles to apply*)
     history:styles Stack.t; (*Saved states*)
    }

type error = No_saved_states

exception Error of error

let make ?dirs name ?(coord=(Coord.identity ()))
    ?(point=(Pointstyle.make "default")) width height =
  {handle = B.make ?dirs name width height;
   styles = {coord = coord; point = point};
   history = Stack.create ()}

let use handle ?(point=(Pointstyle.make "default")) coord =
  {handle = handle;
   styles = {coord = coord; point = point};
   history= Stack.create ()}

let use_unit_square handle x1 y1 x2 y2 =
  use handle {B.xx = x2 -. x1; xy=0.; x0=x1; yx=0.; yy = y2 -. y1; y0=y1}

let get_handle t = t.handle


(*Coordinate transformations are performed by the Backend.matrix part*)
let translate t ~x ~y = Coord.translate t.styles.coord x y
let scale t ~x ~y = Coord.scale t.styles.coord x y
let rotate t ~angle = Coord.rotate t.styles.coord angle

let set_matrix t coord = t.styles.coord <- coord
let get_matrix t = Coord.copy t.styles.coord

let set_point_style t s = t.styles.point <- s
let get_point_style t = t.styles.point


(*Backend primitives*)

let width t = B.width t.handle
let height t = B.height t.handle
let close t = B.close t.handle
let set_color t = B.set_color t.handle
let set_line_width t = B.set_line_width t.handle
let set_line_cap t = B.set_line_cap t.handle
let set_dash t = B.set_dash t.handle
let set_line_join t = B.set_line_join t.handle
let get_line_width t = B.get_line_width t.handle
let get_line_cap t = B.get_line_cap t.handle
let get_dash t = B.get_dash t.handle
let get_line_join t = B.get_line_join t.handle

(*The operations of drawing need transforming the point first, then
  doing the order*)

let move_to t ~x ~y =
  let x', y' = Coord.transform t.styles.coord x y in
  B.move_to t.handle x' y'

let line_to t ~x ~y =
  let x', y' = Coord.transform t.styles.coord x y in
  B.line_to t.handle x' y'

let rel_move_to t ~x ~y =
  let x', y' = Coord.transform_dist t.styles.coord x y in
  B.rel_move_to t.handle x' y'

let rel_line_to t ~x ~y =
  let x', y' = Coord.transform_dist t.styles.coord x y in
  B.rel_line_to t.handle x' y'

let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  let x1, y1 = Coord.transform t.styles.coord x1 y1
  and x2, y2 = Coord.transform t.styles.coord x2 y2
  and x3, y3 = Coord.transform t.styles.coord x3 y3 in
  B.curve_to t.handle ~x1 ~y1 ~x2 ~y2 ~x3 ~y3

(*FIXME: these two primitives do not translate well in Backend.t
  coordinates -- due to scaling*)
let rectangle t ~x ~y ~w ~h =
  let x, y = Coord.transform t.styles.coord x y
  and w, w' = Coord.transform_dist t.styles.coord w 0.
  and h', h = Coord.transform_dist t.styles.coord 0. h in
  if Coord.has_shear t.styles.coord then
    (B.move_to t.handle x y;
     B.rel_line_to t.handle w w';
     B.rel_line_to t.handle h' h;
     B.rel_line_to t.handle (-.w) (-.w');
     B.close_path t.handle)
  else B.rectangle t.handle x y w h

(*
let arc t = failwith "NI due to scalings"*)

let close_path t = B.close_path t.handle
let clear_path t = B.clear_path t.handle
let path_extents t = B.path_extents t.handle
let stroke t = B.stroke t.handle
let stroke_preserve t = B.stroke_preserve t.handle
let fill t = B.fill t.handle
let fill_preserve t = B.fill_preserve t.handle
(*let clip t = B.clip t.handle
let clip_preserve t = B.clip_preserve t.handle*)
let clip_rectangle t = B.clip_rectangle t.handle

let save t =
  B.save t.handle;
  Stack.push {t.styles with coord = t.styles.coord} t.history

let restore t =
  try
    t.styles <- Stack.pop t.history;
    B.restore t.handle
  with Stack.Empty ->
    raise (Error No_saved_states)

let select_font_face t = B.select_font_face t.handle
let set_font_size t = B.set_font_size t.handle
let show_text t ~rotate ~x ~y =
  let x,y = Coord.transform t.styles.coord x y in
  (*FIXME: need to know how t.styles.coord rotates, to get a better rotation of text*)
  B.show_text t.handle ~rotate ~x ~y

let text_extents t = B.text_extents t.handle

let point t x y = Pointstyle.point t.styles.point x y t.handle

let points t list = Pointstyle.points t.styles.point list t.handle
(*Local Variables:*)
(*compile-command: "ocamlopt -c -for-pack Archimedes transform_coord.ml && ocamlc -c -for-pack Archimedes transform_coord.ml"*)
(*End:*)
