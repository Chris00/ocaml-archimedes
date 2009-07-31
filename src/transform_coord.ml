module B = Backend
module Coord = Coordinate

type t =
    {h:B.t; mutable c:Coord.t; s:Coord.t Stack.t}

let make ?dirs name ?coord width height =
  {h = B.make ?dirs name width height;
   c = (match coord with None -> Coord.identity () | Some c -> c);
   s = Stack.create ()}

let use handle coord = {h = handle; c = coord; s = Stack.create ()}

let use_unit_square handle x1 y1 x2 y2 =
  use handle (Coord.create (x2 -. x1) 0. x1 0. (y2 -. y1) y1)

let get_handle t = t.h


(*Coordinate transformations are performed by the Coord.t part*)
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
  coordinates -- due to scaling
(*
  let rectangle t = B.rectangle t
  let arc t = B.arc t*)*)

let close_path t = B.close_path t.h
let path_extents t = B.path_extents t.h
let stroke t = B.stroke t.h
let stroke_preserve t = B.stroke_preserve t.h
let fill t = B.fill t.h
let fill_preserve t = B.fill_preserve t.h
let clip t = B.clip t.h
let clip_preserve t = B.clip_preserve t.h

let save t =
  B.save t.h;
  Stack.push (Coord.copy t.c) t.s

let restore t =
  t.c <- Stack.pop t.s;
  B.restore t.h

let text t = B.text t.h
let text_extents t = B.text_extents t.h

(*Local Variables:*)
(*compile-command: "ocamlopt -c transform_coord.ml && ocamlc -c transform_coord.ml"*)
(*End:*)
