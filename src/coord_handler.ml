module B = Backend
module Coord = Coordinate

(*
type styles =
    {
      mutable ctm: Coordinate.ctm; (*Current coordinate transformation to perform*)
      mutable point: Pointstyle.t;(*Current point style*)
    }
*)


type name =
  | Device
  | Normalized
  | Marks
  | N of string


module Keys =
struct
  type t = name

  let compare = Pervasives.compare
end

module M = Map.Make(Keys)

let init_map list =
  let rec create_coords coords = function
      [] -> coords
    | (x,y)::l -> create_coords (M.add x y coords) l
  in
  create_coords M.empty list

type t =
    {handle:B.t; (*Handle on which we operate*)
     (*mutable styles: styles; (*Current styles to apply*)*)
     mutable coordname: name;
     (*Name of the current transformation*)
     mutable coords: Coord.t M.t;
     (*Transformations available. Will be always non-empty (contains
       initial transformations with keys Default, Normalized and
       Marks) except when closed. So testing whether the handle is
       closed will be equivalent to testing this registry is empty.*)
     initial: Coord.ctm;
     (*Used to store the initial transformation matrix of a backend.*)
     history:name Stack.t;
     (*Saved transformations; used to restore the handle to a previous
       transformation if needed.*)
    }

type error =
    Not_Found of string (*Raised when no coordinate transformation has
                          the supplied name*)
  | Closed
  | Trying_modify_built_in
  | No_saved_states

exception Error of error

let string_of_error = function
  | Not_Found s -> Printf.sprintf "Coordinate %s is not found" s
  | Closed -> "Closed"
  | Trying_modify_built_in -> "Try to modified built-in coordinates"
  | No_saved_states -> "No saved states"

let check t = if M.is_empty t.coords then raise (Error Closed)

let make_coords width height =
  let dev = Coord.make_identity () in
  let normalized = Coord.make_scale dev width height in
  let marks = Coord.make_scale dev (width /. 100.) (width /. 100.) in
    (*marks is so defined to not have stretch on pointstyles by default.*)
  dev, normalized, marks

let make ?dirs name width height =
  let dev, normalized, marks = make_coords width height in
  let coords = init_map
    [Device, dev; Normalized, normalized; Marks, marks] in
  let handle = B.make ?dirs name width height in
  let ctm = Coord.use handle normalized in
  {handle = handle;
   coordname = Normalized;
   coords = coords;
   initial = ctm;
   history = Stack.create ();}

let use handle =
  let width, height = B.width handle, B.height handle in
  let dev, normalized, marks = make_coords width height in
  let ctm = Coord.use handle dev in
  let coords = init_map
    [Device, dev; Normalized, normalized; Marks, marks]
  in
  {handle = handle;
   coordname= Device;
   coords = coords;
   initial = ctm;
   history = Stack.create ();}

let use_unit_square handle ~name x1 y1 x2 y2 =
  let width, height = B.width handle, B.height handle in
  let dev, normalized, marks = make_coords width height in
  let tm = Coord.make_from_transform dev (B.get_matrix handle) in
  let unit_square = Coord.make_translate tm x1 y1 in
  Coord.scale unit_square (x2 -. x1) (y2 -. y1);
  let ctm = Coord.use handle unit_square in
  let coords = init_map
    [Device, dev; Normalized, normalized; Marks, marks; (N name), unit_square]
  in
  {handle = handle;
   coordname= N name;
   coords = coords;
   initial = ctm;
   history = Stack.create ();}

let use_normalized handle =
  let width, height = B.width handle, B.height handle in
  let dev, normalized, marks = make_coords width height in
  let ctm = Coord.use handle normalized in
  let coords = init_map
    [Device, dev; Normalized, normalized; Marks, marks]
  in
  {handle = handle;
   coordname= Normalized;
   coords = coords;
   initial = ctm;
   history = Stack.create ();}

let get_handle t =
  check t;
  (*Coordinate.restore t.handle t.initial;
  (*as this breaks the assumption that the handle makes the correct
    transformation*)
  t.coords <- M.empty;
  (*we cannot reuse a Coord_handler which gave his handle.*)*)
  t.handle

let close t =
  B.close t.handle; t.coords <- M.empty






(********************************************************************)


let get_coord t name =
  match name with
    None ->
      (match t.coordname with
        Device | Normalized | Marks -> raise (Error Trying_modify_built_in)
      | N _ -> M.find t.coordname t.coords)
      (*FIXME: cannot be built-in coord.*)
  | Some s ->
      try M.find (N s) t.coords
      with Not_found ->
        raise (Error (Not_Found s))


let get_coord_built_in t name =
  let s =
    match name with
      None -> t.coordname
        (*FIXME: cannot be built-in coord.*)
    | Some s -> s
  in
  try M.find s t.coords
  with Not_found ->
    let error = Not_Found
      (match s with
       | N(s) -> s
       | _ -> failwith "Built-in coordinate not found"
      )
    in
    raise (Error error)

let add_coord t name coord =
  (*FIXME: what to do if name already given?*)
 (* try
    ignore (M.find (N name) t.coords);
    raise (Error (Already_exists name))
  with Not_found ->*)
    t.coords <- M.add (N name) coord t.coords

let translate t ?name ~x ~y =
  check t;
  let coord = get_coord t name in
  Coord.translate coord x y;
  match name with
    None -> B.translate t.handle x y
  | Some s -> if t.coordname = N s then B.translate t.handle x y

let scale t ?name ~x ~y =
  check t;
  let coord = get_coord t name in
  Coord.scale coord x y;
  match name with
    None -> B.scale t.handle x y
  | Some s -> if t.coordname = N s then B.scale t.handle x y

let rotate t ?name ~angle =
  check t;
  let coord = get_coord t name in
  Coord.rotate coord angle;
  match name with
    None -> B.rotate t.handle angle
  | Some s -> if t.coordname = N s then B.rotate t.handle angle

let scale_marks t ~x ~y =
  check t;
  let coord = M.find Marks t.coords in
  Coord.scale coord x y

let rotate_marks t ~angle =
  check t;
  let coord = M.find Marks t.coords in
  Coord.rotate coord angle

let add_translate t name ?from ~x ~y =
  check t;
  let coord = get_coord_built_in t from in
  let new_coord = Coord.make_translate coord x y in
  add_coord t name new_coord

let add_scale t name ?from ~x ~y =
  check t;
  let coord = get_coord_built_in t from in
  let new_coord = Coord.make_scale coord x y in
  add_coord t name new_coord

let add_rotate t name ?from ~angle =
  check t;
  let coord = get_coord_built_in t from in
  let new_coord = Coord.make_rotate coord angle in
  add_coord t name new_coord

let add_transform t name ?from matrix =
  check t;
  let coord = get_coord_built_in t from in
  let new_coord = Coord.make_from_transform coord matrix in
  add_coord t name new_coord

let set_coordinate t name =
  check t;
  let coord = get_coord t (Some name) in
 (* let m = B.get_matrix t.handle in
  Printf.printf "Setting coord: %f %f %f %f %f %f\n"
    m.B.xx m.B.xy m.B.yx m.B.yy m.B.x0 m.B.y0;*)
  let ms = Coord.use t.handle coord in
 (* let mn = B.get_matrix t.handle in
  Printf.printf "into coord %s; new coord: %f %f %f %f %f %f\n"
    name mn.B.xx mn.B.xy mn.B.yx mn.B.yy mn.B.x0 mn.B.y0;
  Coord.restore t.handle ms;
  let ms = B.get_matrix t.handle in
  Printf.printf "Reset; new coord: %f %f %f %f %f %f\n"
    ms.B.xx ms.B.xy ms.B.yx ms.B.yy ms.B.x0 ms.B.y0;
  B.set_matrix t.handle mn;
  let mn = B.get_matrix t.handle in
  Printf.printf "Finally; new coord: %f %f %f %f %f %f\n"
    mn.B.xx mn.B.xy mn.B.yx mn.B.yy mn.B.x0 mn.B.y0;*)
  t.coordname <- N name

let print_coordinate t = check t;
  match t.coordname with
  | Device -> "~DEVICE"
  | Normalized -> "~NORMALIZED"
  | Marks -> "~MARKS"
  | N s -> "_"^s

let print_matrix t =
  let m = B.get_matrix t.handle in
  Printf.printf "%f %f %f %f %f %f" m.B.xx m.B.xy m.B.yx m.B.yy m.B.x0 m.B.y0

(*Backend primitives
*********************************************************************)

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

let move_to t = B.move_to t.handle

let line_to t = B.line_to t.handle

let rel_move_to t = B.rel_move_to t.handle

let rel_line_to t = B.rel_line_to t.handle

let curve_to t = B.curve_to t.handle

let rectangle t ~x ~y ~w ~h = B.rectangle t.handle x y w h

let arc t = B.arc t.handle

let close_path t = B.close_path t.handle
let clear_path t = B.clear_path t.handle
let path_extents t = B.path_extents t.handle
let stroke t = B.stroke t.handle
let stroke_preserve t = B.stroke_preserve t.handle
let stroke_init t =
  let ctm = Coord.use t.handle (M.find Device t.coords) in
  B.stroke t.handle;
  Coord.restore t.handle ctm

let stroke_init_preserve t =
  let ctm = Coord.use t.handle (M.find Device t.coords) in
  B.stroke_preserve t.handle;
  Coord.restore t.handle ctm

let fill t = B.fill t.handle
let fill_preserve t = B.fill_preserve t.handle
(*let clip t = B.clip t.handle
let clip_preserve t = B.clip_preserve t.handle*)
let clip_rectangle t = B.clip_rectangle t.handle


(*FIXME: what about saving defined coordinates and killing the new
  ones when restoring?*)
let save t =
  B.save t.handle;
  Stack.push t.coordname t.history

let restore t =
  try
    t.coordname <- Stack.pop t.history;
    B.restore t.handle
  with Stack.Empty ->
    raise (Error No_saved_states)

let select_font_face t = B.select_font_face t.handle
let set_font_size t = B.set_font_size t.handle
let show_text t = B.show_text t.handle

let text_extents t = B.text_extents t.handle

let render t name =
  let marks = M.find Marks t.coords in
  let ctm = Coord.use t.handle marks in
  let rect = Pointstyle.render name t.handle in
  Coord.restore t.handle ctm;
  (*Now express [rect] in device coords*)
  let x', y' = Coord.to_device marks rect.B.x rect.B.y in
  let w', h' = Coord.to_device_distance marks rect.B.w rect.B.h in
  {B.x = x'; y = y'; w = w'; h = h'}

(*Local Variables:*)
(*compile-command: "ocamlopt -c -for-pack Archimedes coord_handler.ml && ocamlc -c -for-pack Archimedes coord_handler.ml"*)
(*End:*)
