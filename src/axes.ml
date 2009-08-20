(**Styles definitions to make axes*)
module C = Coord_handler
module B = Backend


exception Not_available

type axes =
    [`Rectangle of bool * bool
    | `Two_lines of float * float
    | `None of bool * bool]
    (*Style of axes.*)

let print_axes axes ~xmin ~xmax ~ymin ~ymax ch =
  match axes with
    `None (_,_) -> ()
  | `Rectangle(_, _) ->
      let x, y = min xmin xmax, min ymin ymax in
      let w, h = abs_float (xmax -. xmin), abs_float (ymax -. ymin) in
      C.rectangle ch x y w h
  | `Two_lines(x,y) ->
      (*Need to update -before- so that mins/maxs are correctly
        initialized for making the axes lines.*)
      let xmin,ymin = min x xmin, min y ymin
      and xmax,ymax = max x xmax, max y ymax in
      C.move_to ch xmin y;
      C.line_to ch xmax y;
      C.move_to ch x ymin;
      C.line_to ch x ymax
  | _ -> raise Not_available

let axes_meeting axes ~xmin ~xmax ~ymin ~ymax =
  match axes with
  | `None(bx, by)
  | `Rectangle(bx, by) ->
      (if bx then xmin else xmax),
      (if by then ymin else ymax)
  | `Two_lines(x,y) -> x,y
  | _ -> raise Not_available

type tic = [`P of Pointstyle.name]
    (*Style of tics.*)

let print_tic ch tic =
  match tic with `P name ->
    C.render ch name
  | _ -> raise Not_available

let tic_extents tic =
  (*Fixed dimension: 1/100 of normalized coordinates.*)
  match tic with `P name ->
    let rect = Pointstyle.extents name in
    {B.x = rect.B.x /. 100.; y = rect.B.y/. 100.;
     w = rect.B.w/. 100.; h = rect.B.h/. 100.}
  | _ -> raise Not_available


type label = {action:C.t -> unit; box:Backend.rectangle; rotation:float}

let tic_extents tic_rect label =
  match label with
    None -> tic_rect
  | Some label ->
      let box, angle = label.box, label.rotation in
      let matrix = Backend.Matrix.make_rotate angle in
      let wx, wy = Backend.Matrix.transform_distance matrix box.Backend.w 0.
      and hx, hy = Backend.Matrix.transform_distance matrix 0. box.Backend.h in
      let xmin, xmax =
        let x0 = tic_rect.Backend.x
        and x1 = box.Backend.x +. (min 0. wx) +. (min 0. hx) in
        let x2 = x0 +.tic_rect.Backend.w
        and x3 = x1 +. (abs_float wx) +. (abs_float hx) in
        min x0 x1, max x2 x3
      and ymin, ymax =
        let y0 = tic_rect.Backend.y
        and y1 = box.Backend.y +. (min 0. wy) +. (min 0. hy) in
        let y2 = y0 +.tic_rect.Backend.h
        and y3 = y1 +. (abs_float wy) +. (abs_float hy) in
        min y0 y1, max y2 y3
      in
      {Backend.x = xmin; y = ymin; w = xmax -. xmin; h = ymax -. ymin}


type 'a axis1 = {major:'a; minor:'a; positions:(float*float*label option) list}

type 'a axis =  float -> float -> float -> 'a axis1

type ('a,'b) t =
    {axes:'a; x:'b axis; y:'b axis}


let make_axis major minor pos x x' y=
  {major = major; minor = minor; positions = (pos x x' y)}

let make axes x y =
  {axes = axes; x = x; y = y}

let print_tics axis print_tic ch =
  let rec print = function
      [] -> ()
    | (x,y,label)::l ->
        C.move_to ch x y;
        match label with
          None -> print_tic ch axis.minor
        | Some label ->
            print_tic ch axis.major;
            label.action ch;
            print l
  in print axis.positions

let print t ~xmin ~xmax ~ymin ~ymax
    ?(print_axes = print_axes) ?(axes_meeting = axes_meeting)
    ?(print_tic = print_tic) ch =
  (*let xxx = ch and yyy = C.get_handle ch in*)
  let x,y = axes_meeting t.axes ~xmin ~xmax ~ymin ~ymax in
  print_axes t.axes ~xmin ~xmax ~ymin ~ymax ch;
  let coord = Coordinate.make_identity () in
  C.stroke_init ch;
  print_tics (t.x xmin xmax y) print_tic ch;
  print_tics (t.y ymin ymax x) print_tic ch;



(*Local variables:*)
(*compile-command: "ocamlopt -c -dtypes axes.ml && ocamlc -c -dtypes axes.ml"*)
(*End:*)
