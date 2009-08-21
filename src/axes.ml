(**Styles definitions to make axes*)
module B = Backend

exception Not_available

let rectangle_extents rect xmin xmax ymin ymax =
    let xmin, xmax =
        let x0 = rect.Backend.x in
        let x1 = x0 +. rect.Backend.w in
        min x0 xmin, max x1 xmax
      and ymin, ymax =
        let y0 = rect.Backend.y in
        let y1 = y0 +. rect.Backend.h in
        min y0 ymin, max y1 ymax
      in
      {Backend.x = xmin; y = ymin; w = xmax -. xmin; h = ymax -. ymin}


type axes =
    [`Rectangle of bool * bool
    | `Two_lines of float * float
    | `None of bool * bool]
    (*Style of axes.*)




let print_axes axes ~xmin ~xmax ~ymin ~ymax backend =
  match axes with
    `None (_,_) -> ()
  | `Rectangle(_, _) ->
      let x, y = min xmin xmax, min ymin ymax in
      let w, h = abs_float (xmax -. xmin), abs_float (ymax -. ymin) in
      B.rectangle backend x y w h
  | `Two_lines(x,y) ->
      (*Need to update -before- so that mins/maxs are correctly
        initialized for making the axes lines.*)
      let xmin,ymin = min x xmin, min y ymin
      and xmax,ymax = max x xmax, max y ymax in
      B.move_to backend xmin y;
      B.line_to backend xmax y;
      B.move_to backend x ymin;
      B.line_to backend x ymax
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

let print_tic backend tic =
  match tic with `P name ->
    Pointstyle.render name backend
  | _ -> raise Not_available

let tic_extents tic =
  (*Fixed dimension: 1/100 of normalized coordinates.*)
  match tic with `P name ->
    let rect = Pointstyle.extents name in
    {B.x = rect.B.x /. 100.; y = rect.B.y/. 100.;
     w = rect.B.w/. 100.; h = rect.B.h/. 100.}
  | _ -> raise Not_available



type label1 = {action:B.t -> unit; box:B.t -> Backend.rectangle}

type label = {label:float -> float -> label1; rotation:float}

let tic_label_extents tic_rect label x y b =
  match label with
    None -> tic_rect
  | Some label ->
      let box = (label.label x y).box b in
      let matrix = Backend.Matrix.make_rotate label.rotation in
      let wx, wy = Backend.Matrix.transform_distance matrix box.Backend.w 0.
      and hx, hy = Backend.Matrix.transform_distance matrix 0. box.Backend.h in
      let x1 = box.Backend.x +. (min 0. wx) +. (min 0. hx) in
      let x2 = x1 +. (abs_float wx) +. (abs_float hx) in
      let y1 = box.Backend.y +. (min 0. wy) +. (min 0. hy) in
      let y2 = y1 +. (abs_float wy) +. (abs_float hy) in
      rectangle_extents tic_rect x1 x2 y1 y2


type position =
     float -> float -> float -> float -> (float*float*label option) list

type data =
    [ `Label of label
    | `Text_label of string
    | `Abscissa
    | `Ordinate
    | `Expabscissa
    | `Expordinate ]


let make_box_from_text txt pos x y b =
  let rect = B.text_extents b txt in
  let rx, ry, w, h =
    rect.B.x +. x, rect.B.y +. y, rect.B.w, rect.B.h
  in
  let x = match pos with
    | Backend.CC | Backend.CT | Backend.CB -> rx -. 0.5 *. w
    | Backend.RC | Backend.RT | Backend.RB -> rx
    | Backend.LC | Backend.LT | Backend.LB -> rx -. w
  and y = match pos with
    | Backend.CC | Backend.RC | Backend.LC -> ry -. 0.5 *. h
    | Backend.CT | Backend.RT | Backend.LT -> ry -. h
    | Backend.CB | Backend.RB | Backend.LB -> ry
  in
  {B.x = x; y = y; w = w; h = h}

let get_label data =
  match data with
  | `Label l -> l
  | `Text_label(txt, rotate, pos) ->
      let f x y =
        {action = (fun b -> B.show_text b x y rotate pos txt);
         box = make_box_from_text txt pos x y}
      in {label = f; rotation = rotate}
  | `Abscissa ->
      let f x y =
        let txt = Printf.sprintf "%g" x in
        {action = (fun b -> B.show_text b x y 0. B.CB txt);
         box =make_box_from_text txt B.CB x y}
      in {label = f; rotation = 0.}
  | `Ordinate ->
      let f x y =
        let txt = Printf.sprintf "%g" y in
        {action = (fun b -> B.show_text b x y 0. B.LC txt);
         box =make_box_from_text txt B.LC x y}
      in {label = f; rotation = 0.}
  | `Expabscissa ->
      let f x y =
        let txt = Printf.sprintf "%g" (10.**x) in
        {action = (fun b -> B.show_text b x y 0. B.CB txt);
         box =make_box_from_text txt B.CB x y}
      in {label = f; rotation = 0.}
  | `Expordinate ->
      let f x y =
        let txt = Printf.sprintf "%g" (10.**y) in
        {action = (fun b -> B.show_text b x y 0. B.LC txt);
         box =make_box_from_text txt B.LC x y}
      in {label = f; rotation = 0.}
  | _ -> raise Not_available


type loc_tics =
    [ `Fixed_pos of (float * label option) list
    | `Fixed_numbers of int list * label list
    | `Regular of int * label array]

let get_position loc =
  match loc with
    `Fixed_pos list ->
      (fun x x' y y' ->
         let f x x' t = x +. t *. (x' -. x) in
         let g (t,label) = f x x' t, f y y' t, label in
         List.map g list)
  | `Fixed_numbers(numbers, labels) ->
      (*let nmajors = List.length numbers in*)
      let nall = List.fold_left (+) 0 numbers in
      let list_tics =
        let rec make_list_tics k nums labels ltics =
          match nums with
            [] -> ltics
          | i::l ->
              if i = 0 then
                (*Facing a major tic: get a label*)
                let label, rest =
                  match labels with
                    [] -> None, []
                  | s::l2 -> Some s, l2
                in make_list_tics (k+1) l rest ((k, label)::ltics)
              else (*facing a minor tic*)
                make_list_tics (k+1) ((i-1)::l) labels ((k, None)::ltics)
        in (*Initialisation: make the first major tic, then start.*)
        let firstlabel, labels =
          match labels with
          | [] -> None, []
          | s::l2 -> Some s, l2
        in make_list_tics 1 numbers labels [0, firstlabel]
      in
      (fun x x' y y' ->
         let xstep = (x' -. x) /. (float nall)
         and ystep = (y' -. y) /. (float nall) in
         List.rev_map
           (fun (i,label) ->
              let t = float i in
              x +. t *. xstep, y +. t *. ystep, label)
           list_tics
      )
  | `Regular(minors,array) ->
      let majors = Array.length array in
      let nall = minors * (majors - 1) + majors in
      let intlist =
        let rec make_list i list =
          if i >= nall then list
          else make_list (i-1) (i::list)
        in
        make_list 0 []
      in
      (fun x x' y y' ->
         let xstep = (x' -. x) /. (float nall)
         and ystep = (y' -. y) /. (float nall) in
         List.rev_map
           (fun i ->
              let t = float i in
              let label =
                let j,k = i/(minors + 1), i mod minors + 1 in
                if k = 0 then Some array.(j)
                else None
              in
              x +. t *. xstep, y +. t*. ystep, label)
           intlist)
  | _ -> raise Not_available

type 'a axis = (*'a for tic type*)
    {major:'a; minor:'a; positions:position}

type ('a,'b) t = (*'a for axes type, 'b for tic types*)
    {axes:'a; x:'b axis; y:'b axis}


let make_axis major minor ?(get_position = get_position) loc=
  let positions = get_position loc in
  {major = major; minor = minor; positions = positions}

let make axes x y =
  {axes = axes; x = x; y = y}

let print_tics axis vmin vmax wmin wmax print_tic backend =
  let rec print = function
      [] -> ()
    | (x,y,label)::l ->
        B.move_to backend x y;
        match label with
          None -> print_tic backend axis.minor
        | Some label ->
            print_tic backend axis.major;
            (label.label x y).action backend;
            print l
  in print (axis.positions vmin vmax wmin wmax)

let axis_margins axis xmin xmax ymin ymax tic_extents backend =
  let list = axis.positions xmin xmax ymin ymax in
  let rec make_rect list rect =
    match list with
      [] -> rect
    | (x,y,label)::l ->
        let tic = match label with None -> axis.minor | Some _ -> axis.major in
        let extents = tic_label_extents (tic_extents tic) label x y backend in
        let x1,y1 = extents.B.x, extents.B.y in
        let x2,y2 = x1 +. extents.B.w, y1 +. extents.B.h in
        let newrect = rectangle_extents rect
          (x1 +. x) (x2 +. x) (y1 +. y) (y2 +. y)
        in
        make_rect l newrect
  in make_rect list {B.x=xmin; y=ymin; w=0.;h=0.}


let get_margins t
    ?(axes_meeting = axes_meeting) ?(tic_extents = tic_extents)
    xmin xmax ymin ymax backend=
  let x,y = axes_meeting t.axes ~xmin ~xmax ~ymin ~ymax in
  let xmin,ymin = min x xmin, min y ymin
  and xmax,ymax = max x xmax, max y ymax in
  axis_margins t.x xmin xmax ymin ymax tic_extents backend,
  axis_margins t.y xmin xmax ymin ymax tic_extents backend


let print t ~lines ~xmin ~xmax ~ymin ~ymax
    ?(print_axes = print_axes) ?(axes_meeting = axes_meeting)
    ?(print_tic = print_tic) backend =
  (*let xxx = backend and yyy = B.get_handle backend in*)
  let x,y = axes_meeting t.axes ~xmin ~xmax ~ymin ~ymax in
  print_axes t.axes ~xmin ~xmax ~ymin ~ymax backend;
  let ctm = Coordinate.use backend lines in
  B.stroke backend;
  Coordinate.restore backend ctm;
  print_tics t.x xmin xmax y y print_tic backend; (*X axis*)
  print_tics t.y ymin ymax x x print_tic backend  (*Y axis*)
(*Local variables:*)
(*compile-command: "ocamlopt -c -dtypes axes.ml && ocamlc -c -dtypes axes.ml"*)
(*End:*)
