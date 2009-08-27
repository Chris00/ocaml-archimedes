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
    [ `None of bool * bool
    | `Rectangle of bool * bool
    | `Two_lines of float * float
    | `Two_lines_rel of float * float
    ]
    (*Style of axes.*)




let print_axes axes ranges backend =
  Backend.make_range_min1 ranges;
  match axes with
    `None (_,_) -> ()
  | `Rectangle(_, _) ->
      let rect = Backend.rect_of_ranges ranges in
      B.rectangle backend rect.B.x rect.B.y rect.B.w rect.B.h
  | `Two_lines(x,y) ->
      (*Need to update -before- so that mins/maxs are correctly
        initialized for making the axes lines.*)
      let xmin,ymin = min x ranges.B.x1, min y ranges.B.y1
      and xmax,ymax = max x ranges.B.x2, max y ranges.B.y2 in
      B.move_to backend xmin y;
      B.line_to backend xmax y;
      B.move_to backend x ymin;
      B.line_to backend x ymax
  | `Two_lines_rel(t,u) ->
      (*Need to update -before- so that mins/maxs are correctly
        initialized for making the axes lines.*)
      let x,y =
        ranges.B.x1 +. t *. (ranges.B.x2 -. ranges.B.x1),
        ranges.B.y1 +. u *. (ranges.B.y2 -. ranges.B.y1)
      in
      let xmin,ymin = min x ranges.B.x1, min y ranges.B.y1
      and xmax,ymax = max x ranges.B.x2, max y ranges.B.y2 in
      B.move_to backend xmin y;
      B.line_to backend xmax y;
      B.move_to backend x ymin;
      B.line_to backend x ymax
  | _ -> raise Not_available

let axes_meeting axes ranges =
  Backend.make_range_min1 ranges;
  match axes with
  | `None(bx, by)
  | `Rectangle(bx, by) ->
      (if bx then ranges.B.x1 else ranges.B.x2),
      (if by then ranges.B.y1 else ranges.B.y2)
  | `Two_lines(x,y) -> x,y
  | `Two_lines_rel(t,u) ->
      ranges.B.x1 +. t *. (ranges.B.x2 -. ranges.B.x1),
      ranges.B.y1 +. u *. (ranges.B.y2 -. ranges.B.y1)
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

type label =
    {action: float -> float -> B.text_position -> B.t -> unit;
     box:float -> float -> B.text_position -> B.t -> Backend.rectangle;
     rotation:float}

let tic_label_extents tic_extents label x y pos b =
  match label with
    None -> tic_extents
  | Some label ->
      let box = label.box x y pos b in
      let matrix = Backend.Matrix.make_rotate label.rotation in
      let wx, wy = Backend.Matrix.transform_distance matrix box.Backend.w 0.
      and hx, hy = Backend.Matrix.transform_distance matrix 0. box.Backend.h in
      let x1 = box.Backend.x +. (min 0. wx) +. (min 0. hx) in
      let x2 = x1 +. (abs_float wx) +. (abs_float hx) in
      let y1 = box.Backend.y +. (min 0. wy) +. (min 0. hy) in
      let y2 = y1 +. (abs_float wy) +. (abs_float hy) in
      rectangle_extents tic_extents x1 x2 y1 y2


type tic_position =
     Backend.xyranges -> (float*float*label option) list

type label_collection =
    Fixed of label array
  | Variable of label

type data =
    [ `Label of label array
    | `Text_label of string array * float
    | `Abscissa
    | `Ordinate
    | `Expabscissa
    | `Expordinate]

let make_box_from_text txt x y pos b =
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

let get_labels data =
  match data with
  | `Label l -> Fixed l
  | `Text_label(txts, rotate) ->
      let f txt =
        {action = (fun x y pos b -> B.show_text b rotate x y pos txt);
         box = make_box_from_text txt;
         rotation = rotate}
      in
      Fixed (Array.map f txts)
  | `Abscissa ->
      Variable ({action = (fun x y ->
                   let txt = Printf.sprintf "%g" x in
                   fun pos b -> B.show_text b x y 0. pos txt);
       box = (fun x y ->
                let txt = Printf.sprintf "%g" x in
                make_box_from_text txt x y);
       rotation = 0.})
  | `Ordinate ->
      Variable (
      {action = (fun x y ->
                   let txt = Printf.sprintf "%g" y in
                   fun pos b -> B.show_text b x y 0. pos txt);
       box = (fun x y ->
                let txt = Printf.sprintf "%g" y in
                make_box_from_text txt x y);
       rotation = 0.})
  | `Expabscissa ->
      Variable (
      {action = (fun x y ->
                   let txt = Printf.sprintf "%g" (10.**x) in
                   fun pos b -> B.show_text b x y 0. pos txt);
       box = (fun x y ->
                let txt = Printf.sprintf "%g" (10.**x) in
                make_box_from_text txt x y);
       rotation = 0.})
  | `Expordinate ->
      Variable (
      {action = (fun x y ->
                   let txt = Printf.sprintf "%g" (10.**y) in
                   fun pos b -> B.show_text b x y 0. pos txt);
       box = (fun x y ->
                let txt = Printf.sprintf "%g" (10.**y) in
                make_box_from_text txt x y);
       rotation = 0.})
  | _ -> raise Not_available


type loc_tics =
    [ `Fixed_pos of (float * bool) list
    | `Linear_variable of int array
    | `Linear of int * int
    | `Logarithmic of int * int
    ]

let get_position loc labels =
  match loc with
    `Fixed_pos t_list ->
      let f x x' t = x +. t *. (x' -. x) in
      let m = ref 0 in (*Used in case of Fixed labels*)
      let g ranges (t,b) =
        let labelopt =
          if b then (*major tic => add a label*)
            match labels with
              Variable l -> Some l
            | Fixed larray ->
                let label =
                  try Some larray.(!m)
                  with Invalid_argument _ -> None
                    (*too few labels : the last major tics won't
                      get any label.*)
                in
                m := !m + 1;
                label
          else (*Minor tic => no label*)
            None
        in
        f ranges.B.x1 ranges.B.x2 t, f ranges.B.y1 ranges.B.y2 t, labelopt
      in
      (fun ranges ->
         Backend.make_range_min1 ranges;
         List.map (g ranges) t_list)
  | `Linear_variable numbers ->
      let f (list, len) m =
        let rec do_minors list i =
          if i <= 0 then list
          else
            do_minors ((len + m - i, None)::list) (i-1)
        in
        let list = do_minors list m in
        let label =
          match labels with
            Variable l -> Some l
          | Fixed larray -> try Some larray.(m+1)
            with Invalid_argument _ -> None
        in
        ((len + m, label)::list, len + m + 1)
      in
      let list_tics, nall =
        let first_tic =
          [0,
           (match labels with
              Variable l -> Some l
            | Fixed larray -> try Some larray.(0)
              with Invalid_argument _ -> None)]
        in
        Array.fold_left f (first_tic, 1) numbers
      in
      (fun ranges ->
         Backend.make_range_min1 ranges;
         let xstep = (ranges.B.x2 -. ranges.B.x1) /. (float nall)
         and ystep = (ranges.B.y2 -. ranges.B.y1) /. (float nall) in
         List.rev_map
           (fun (i,label) ->
              let t = float i in
              ranges.B.x1 +. t *. xstep, ranges.B.y1 +. t *. ystep, label)
           list_tics
      )
  | `Linear(majors,minors) ->
      let nall = minors * (majors - 1) + majors in
      let rec make_list list i =
        if i >= nall then list
        else
          let label =
            let j,k = i/(minors + 1), i mod minors + 1 in
            if k = 0 then
              match labels with
                Variable l -> Some l
              | Fixed larray ->
                  try Some larray.(j)
                  with Invalid_argument _ -> None
            else None
          in
          let tuple = i, label in
          make_list (tuple::list) (i+1)
      in
      let list = make_list [] 0 in
      (*List of the form [(i, label);...]: [i]th tic gets
        [label]. Note that, for efficiency reasons (tail-recursivity
        of rev-mapping), the first label is the last element of the
        list.*)
      (fun ranges ->
         Backend.make_range_min1 ranges;
         let xstep = (ranges.B.x2 -. ranges.B.x1) /. (float nall)
         and ystep = (ranges.B.y2 -. ranges.B.y1) /. (float nall) in
         List.rev_map
           (fun (i,label) ->
              let t = float i in
              ranges.B.x1 +. t *. xstep, ranges.B.y1 +. t*. ystep, label)
           list)
  | `Logarithmic(majors, minors) ->
      let nall = minors * (majors - 1) + majors in
      let rec make_list list i =
        if i >= nall then list
        else
          let j,k = i/(minors + 1), i mod minors + 1 in
          let label =
            if k = 0 then
              match labels with
                Variable l -> Some l
              | Fixed larray ->
                  try Some larray.(j)
                  with Invalid_argument _ -> None
            else None
          in
          let tuple = j, k, label in
          make_list (tuple::list) (i+1)
      in
      let list = make_list [] 0 in
      (*List of the form [(j,k, label);...]: [j*(minors+1) + k]th tic
        gets [label].  Note that, for efficiency reasons
        (tail-recursivity of rev-mapping), the first label is the last
        element of the list.*)
      (fun ranges ->
         Backend.make_range_min1 ranges;
         let xmajorstep = (ranges.B.x2 -. ranges.B.x1) /. (float majors)
         and ymajorstep = (ranges.B.y2 -. ranges.B.y1) /. (float majors) in
         List.rev_map
           (fun (j,k,label) ->
              let t = float j in
              let x0 = ranges.B.x1 +. t *. xmajorstep
              and y0 = ranges.B.y1 +. t *. ymajorstep in
              let f mstep v0 =
                let ministep = mstep *. (float k) /. (float (minors + 1)) in
                let g st = log (1. +. st /. v0) in
                x0 +. mstep *. (g ministep) /. (g mstep)
              in
              f xmajorstep x0, f ymajorstep y0, label)
           list)
  | _ -> raise Not_available

       type 'a axis = (*'a for tic type*)
    {major:'a; minor:'a; positions:tic_position; label_position:B.text_position}

type ('a,'b) t = (*'a for axes type, 'b for tic types*)
    {axes:'a; x:'b axis; y:'b axis}


let make_axis major data label_position minor ?(get_labels = get_labels)
    ?(get_position = get_position) loc =
  let labels = get_labels data in
  let positions = get_position loc labels in
  {major = major; minor = minor; positions = positions;
   label_position = label_position}

let make axes x y =
  {axes = axes; x = x; y = y}

let print_tics axis ranges print_tic backend =
  let rec print = function
      [] -> ()
    | (x,y,label)::l ->
        B.move_to backend x y;
        match label with
          None -> print_tic backend axis.minor
        | Some label ->
            print_tic backend axis.major;
            label.action x y axis.label_position backend;
            print l
  in print (axis.positions ranges)

let axis_margins axis ranges tic_extents backend =
  Backend.make_range_min1 ranges;
  let list = axis.positions ranges in
  let rec make_rect list rect =
    match list with
      [] -> rect
    | (x,y,label)::l ->
        let tic = match label with None -> axis.minor | Some _ -> axis.major in
        let extents = tic_label_extents
          (tic_extents tic) label x y axis.label_position backend
        in
        let x1,y1 = extents.B.x, extents.B.y in
        let x2,y2 = x1 +. extents.B.w, y1 +. extents.B.h in
        let newrect = rectangle_extents rect
          (x1 +. x) (x2 +. x) (y1 +. y) (y2 +. y)
        in
        make_rect l newrect
  in make_rect list {B.x=ranges.B.x1; y=ranges.B.y1; w=0.;h=0.}


let get_margins t
    ?(axes_meeting = axes_meeting) ?(tic_extents = tic_extents)
    ranges backend=
  Backend.make_range_min1 ranges;
  let x,y = axes_meeting t.axes ranges in
  Backend.update_ranges ranges x y;(*
  let xmin,ymin = min x ranges.B.x1, min y ranges.B.y1
  and xmax,ymax = max x ranges.B.x2, max y ranges.B.y2 in
  let newranges = {B.x1 = xmin; y1 = ymin; x2 = xmax; y2 = ymax} in*)
  axis_margins t.x ranges tic_extents backend,
  axis_margins t.y ranges tic_extents backend


let print t ~lines ~ranges
    ?(print_axes = print_axes) ?(axes_meeting = axes_meeting)
    ?(print_tic = print_tic) backend =
  (*let xxx = backend and yyy = B.get_handle backend in*)
  let x,y = axes_meeting t.axes ranges in
  print_axes t.axes ranges backend;
  let ctm = Coordinate.use backend lines in
  B.stroke backend;
  Coordinate.restore backend ctm;
  let xrange = {B.x1 = ranges.B.x1; x2 = ranges.B.x2; y1 = y; y2 = y}
  and yrange = {B.x1 = x; x2 = x; y1 = ranges.B.y1; y2 = ranges.B.y2} in
  print_tics t.x xrange print_tic backend; (*X axis*)
  print_tics t.y yrange print_tic backend  (*Y axis*)
(*Local variables:*)
(*compile-command: "ocamlopt -c -dtypes axes.ml && ocamlc -c -dtypes axes.ml"*)
(*End:*)
