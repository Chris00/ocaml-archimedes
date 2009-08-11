module B = Backend
module Coord = Coordinate

(*
type styles =
    {
      mutable ctm: Coordinate.ctm; (*Current coordinate transformation to perform*)
      mutable point: Pointstyle.t;(*Current point style*)
    }
*)

type t =
    {handle:B.t; (*Handle on which we operate*)
     (*mutable styles: styles; (*Current styles to apply*)*)
     mutable coord:Coord.t;(*Holds the current transformation*)
     mutable coords: (string * Coord.t) list
       (*List of transformations available. Will be always non-empty
         (contains initial transformation with name "") except when
         closed. So testing whether the handle is closed will be
         equivalent to testing this list is empty.*)
       (* history:styles Stack.t; (*Saved states*)*)
    }

type error =
    Not_Found of string (*Raised when no coordinate transformation has
                          the supplied name*)
  | Closed

exception Error of error

let check t = if t.coords = [] then raise (Error Closed)

let make ?dirs name ?(coord=(Coord.make_identity ())) width height =
  {handle = B.make ?dirs name width height;
   coord = coord;
   coords = ["", coord]}

let use ?(coord=(Coord.make_identity ())) handle =
  {handle = handle;
   coord = coord;
   coords = ["", coord]},
  Coord.use handle coord

let use_unit_square handle x1 y1 x2 y2 =
  let coord = Coord.make_identity() in
  Coord.translate coord x1 y1;
  Coord.scale coord (x2 -. x1) (y2 -. y1);
  use ~coord handle

let use_normalized handle =
  let coord = Coord.make_identity() in
  Coord.scale coord (B.width handle) (B.height handle);
  use ~coord handle

let get_handle ?initial t =
  check t;
  (match initial with
     None -> ()
   | Some c -> B.set_matrix t.handle c);
  t.handle

let close t =
  B.close t.handle; t.coords <- []

module Map =
struct
  let fetch t name =
    match name with
      None -> t.coord
    | Some name ->
        try List.assoc name t.coords
        with Not_found ->
          raise (Error (Not_Found name))

  let add t name coord =
    t.coords <- (name, coord)::t.coords

  let remove t name =
    (*FIXME: don't use List.remove_assoc because it is not
      tail-recursive and does not flag error if not found.*)
    let rec find_and_kill browsed remains =
      match remains with
        [] -> raise (Error (Not_Found name))
      | (x,c)::list ->
          if x = name then
            (t.coords <- List.rev_append browsed list;
             c)
          else find_and_kill ((x,c)::browsed) list
    in find_and_kill [] t.coords

  let remove_silent t name =
    (*List.remove_assoc name t.coords*)
    try ignore (remove t name)
    with Error (Not_Found _) -> ()
end


let translate t ?name ~x ~y =
  check t;
  let coord = Map.fetch t name in
  Coord.translate coord x y

let scale t ?name ~x ~y =
  check t;
  let coord = Map.fetch t name in
  Coord.scale coord x y

let rotate t ?name ~angle =
  check t;
  let coord = Map.fetch t name in
  Coord.rotate coord angle

let add_translate t name ?from ~x ~y =
  check t;
  let coord = Map.fetch t from in
  let new_coord = Coord.make_translate coord x y in
  Map.add t name new_coord

let add_scale t name ?from ~x ~y =
  check t;
  let coord = Map.fetch t from in
  let new_coord = Coord.make_scale coord x y in
  Map.add t name new_coord

let add_rotate t name ?from ~angle =
  check t;
  let coord = Map.fetch t from in
  let new_coord = Coord.make_rotate coord angle in
  Map.add t name new_coord

let add_transform t name ?from matrix =
  check t;
  let coord = Map.fetch t from in
  let new_coord = Coord.make_from_transform coord matrix in
  Map.add t name new_coord

let set_coordinate t name =
  check t; t.coord <- Map.fetch t (Some name)

let get_coordinate t = check t; Coord.copy t.coord



(*Backend primitives*)

(*(*FIXME: needed? If so, in which coords?*)
let width t = B.width t.handle
let height t = B.height t.handle*)
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
  let x', y' = Coord.to_device t.coord x y in
  B.move_to t.handle x' y'

let line_to t ~x ~y =
  let x', y' = Coord.to_device t.coord x y in
  B.line_to t.handle x' y'

let rel_move_to t ~x ~y =
  let x', y' = Coord.to_device_distance t.coord x y in
  B.rel_move_to t.handle x' y'

let rel_line_to t ~x ~y =
  let x', y' = Coord.to_device_distance t.coord x y in
  B.rel_line_to t.handle x' y'

let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  let x1, y1 = Coord.to_device t.coord x1 y1
  and x2, y2 = Coord.to_device t.coord x2 y2
  and x3, y3 = Coord.to_device t.coord x3 y3 in
  B.curve_to t.handle ~x1 ~y1 ~x2 ~y2 ~x3 ~y3

(*FIXME: these two primitives do not translate well in Backend.t
  coordinates -- due to rotations.*)
let rectangle t ~x ~y ~w ~h =
  let x, y = Coord.to_device t.coord x y
  and w, w' = Coord.to_device_distance t.coord w 0.
  and h', h = Coord.to_device_distance t.coord 0. h in
  (*if Coord.has_shear t.styles.coord then*)
    (B.move_to t.handle x y;
     B.rel_line_to t.handle w w';
     B.rel_line_to t.handle h' h;
     B.rel_line_to t.handle (-.w) (-.w');
     B.close_path t.handle)
  (*else B.rectangle t.handle x y w h*)

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


(*FIXME: what about saving defined coordinates and killing the new
  ones when restoring?*)
let save t =
  B.save t.handle
    (*  Stack.push {t.styles with coord = t.styles.coord} t.history*)

let restore t =
(*  try
    t.styles <- Stack.pop t.history;*)
    B.restore t.handle
  (*with Stack.Empty ->
    raise (Error No_saved_states)*)

let select_font_face t = B.select_font_face t.handle
let set_font_size t = B.set_font_size t.handle
let show_text t ~rotate ~x ~y =
  let x,y = Coord.to_device t.coord x y in
  (*FIXME: need to know how t.coord rotates, to get a better rotation of text*)
  B.show_text t.handle ~rotate ~x ~y

let text_extents t = B.text_extents t.handle

let point t ps x y = Pointstyle.point ps x y t.handle

let points t ps list = Pointstyle.points ps list t.handle
(*Local Variables:*)
(*compile-command: "ocamlopt -c -for-pack Archimedes transform_coord.ml && ocamlc -c -for-pack Archimedes transform_coord.ml"*)
(*End:*)
