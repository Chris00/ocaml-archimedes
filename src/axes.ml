(* File: axes.ml

   Copyright (C) 2009

     Bertrand Desmons <Bertrand.Desmons@umons.ac.be>
     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(**Styles definitions to make axes*)

module Ranges =
struct
  type t =
      {mutable xmin:float;
       mutable ymin:float;
       mutable xmax:float;
       mutable ymax:float;}

  let make x y = {xmin = x;ymin = y; xmax = x; ymax = y}

  let update rg x y =
    if rg.xmin > x then rg.xmin <- x;
    if rg.xmax < x then rg.xmax <- x;
    if rg.ymin > y then rg.ymin <- y;
    if rg.ymax < y then rg.ymax <- y

  let of_rect rect =
    {xmin = rect.Backend.x; ymin = rect.Backend.y;
     xmax = rect.Backend.x +. rect.Backend.w;
     ymax = rect.Backend.x +. rect.Backend.h}

  let to_rect rg =
    {Backend.x = rg.xmin; y = rg.ymin;
     w = rg.xmax -. rg.xmin;
     h = rg.ymax -. rg.ymin}
end

type ranges = Ranges.t =
    private {mutable xmin:float;
             mutable ymin:float;
             mutable xmax:float;
             mutable ymax:float}

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
  match axes with
    `None (_,_) -> ()
  | `Rectangle(_, _) ->
      let rect = Ranges.to_rect ranges in
      Backend.rectangle backend
        rect.Backend.x rect.Backend.y rect.Backend.w rect.Backend.h
  | `Two_lines(x,y) ->
      (*Need to update -before- so that mins/maxs are correctly
        initialized for making the axes lines.*)
      let xmin,ymin = min x ranges.xmin, min y ranges.ymin
      and xmax,ymax = max x ranges.xmax, max y ranges.ymax in
      Backend.move_to backend xmin y;
      Backend.line_to backend xmax y;
      Backend.move_to backend x ymin;
      Backend.line_to backend x ymax
  | `Two_lines_rel(t,u) ->
      (*Need to update -before- so that mins/maxs are correctly
        initialized for making the axes lines.*)
      let x = ranges.xmin +. t *. (ranges.xmax -. ranges.xmin)
      and y = ranges.ymin +. u *. (ranges.ymax -. ranges.ymin) in
      let xmin = min x ranges.xmin and ymin = min y ranges.ymin
      and xmax = max x ranges.xmax and ymax = max y ranges.ymax in
      Backend.move_to backend xmin y;
      Backend.line_to backend xmax y;
      Backend.move_to backend x ymin;
      Backend.line_to backend x ymax
  | _ -> raise Not_available

let axes_meeting axes ranges =
  match axes with
  | `None(bx, by)
  | `Rectangle(bx, by) ->
      (if bx then ranges.xmin else ranges.xmax),
      (if by then ranges.ymin else ranges.ymax)
  | `Two_lines(x,y) -> x,y
  | `Two_lines_rel(t,u) ->
      ranges.xmin +. t *. (ranges.xmax -. ranges.xmin),
      ranges.ymin +. u *. (ranges.ymax -. ranges.ymin)
  | _ -> raise Not_available

type tic = [`P of Pointstyle.name]
    (*Style of tics.*)

let print_tic backend tic =
  match tic with `P name ->
    Pointstyle.render name backend
  | _ -> raise Not_available

let tic_extents tic =
  (*Fixed dimension: 1/100 of normalized coordinates.*)
  (*FIXME: coordinate transformation for tics?*)
  match tic with `P name ->
    let rect = Pointstyle.extents name in
    {Backend.x = rect.Backend.x /. 100.; y = rect.Backend.y/. 100.;
     w = rect.Backend.w/. 100.; h = rect.Backend.h/. 100.}
  | _ -> raise Not_available

type label =
    {action: float -> float -> text_position -> Backend.t -> unit;
     box:float ->float -> text_position ->Backend.t ->Backend.rectangle;
     rotation:float}

let tic_label_extents tic_extents label x y pos b =
  match label with
    None -> tic_extents
  | Some label ->
      let box = label.box x y pos b in
      let matrix = Matrix.make_rotate label.rotation in
      let wx, wy = Matrix.transform_distance matrix box.Backend.w 0.
      and hx, hy = Matrix.transform_distance matrix 0. box.Backend.h in
      let xmin = box.Backend.x +. (min 0. wx) +. (min 0. hx) in
      let xmax = xmin +. (abs_float wx) +. (abs_float hx) in
      let ymin = box.Backend.y +. (min 0. wy) +. (min 0. hy) in
      let ymax = ymin +. (abs_float wy) +. (abs_float hy) in
      rectangle_extents tic_extents xmin xmax ymin ymax

(*FIXME: really need a range?*)
type tic_position = ranges -> bool -> Backend.t ->
      (float*float*label option) list

type label_collection = int -> label

exception Too_few_labels

type data =
    [ `Text_label of string array * float
    | `Number
    | `Expnumber]

let make_box_from_text txt x y pos b =
  let rect = Backend.text_extents b txt in
  let rx, ry, w, h =
    rect.Backend.x +. x, rect.Backend.y +. y, rect.Backend.w, rect.Backend.h
  in
  let x = match pos with
    | CC | CT | CB -> rx -. 0.5 *. w
    | RC | RT | RB -> rx
    | LC | LT | LB -> rx -. w
  and y = match pos with
    | CC | RC | LC -> ry -. 0.5 *. h
    | CT | RT | LT -> ry -. h
    | CB | RB | LB -> ry
  in
  {Backend.x = x; y = y; w = w; h = h}

let get_labels x_axis data =
  match data with
  | `Text_label(txts, rotate) ->
      let f txt =
        {action = (fun x y pos b -> Backend.show_text b rotate x y pos txt);
         box = make_box_from_text txt;
         rotation = rotate}
      in
      let array = Array.map f txts in
      (fun i -> try array.(i) with Invalid_argument _ -> raise Too_few_labels)

  | `Number ->
      let txt x y = Printf.sprintf "%g" (if x_axis then x else y) in
      (fun _ ->
         {action = (fun x y pos b -> Backend.show_text b x y 0. pos (txt x y));
          box = (fun x y -> make_box_from_text (txt x y) x y);
          rotation = 0.})

  | `Expnumber ->
      let txt x y = Printf.sprintf "%g" (if x_axis then 10.**x else 10.**y) in
      (fun _ ->
         {action = (fun x y pos b -> Backend.show_text b x y 0. pos (txt x y));
          box = (fun x y -> make_box_from_text (txt x y) x y);
          rotation = 0.})

  | _ -> raise Not_available


type loc_tics =
    [ `Fixed_rel of (float * bool) list
    | `Fixed_abs of (float * bool) list
    | `Linear_variable of int array
    | `Linear of int * int
    | `Logarithmic of int * int
    | `Auto_linear
    ]

let get_position loc labels =
  match loc with
    `Fixed_rel t_list ->
      let f x x' t = x +. t *. (x' -. x) in
      let m = ref 0 in (*Used in case of Fixed labels*)
      let g ranges (t,b) =
        let labelopt =
          if b then (*major tic => add a label*)
            try
              let label = labels !m in
              m := !m + 1;
              Some label
            with Too_few_labels -> None
              (*too few labels : the last major tics won't get any
                label.*)
          else (*Minor tic => no label*)
            None
        in
        f ranges.xmin ranges.xmax t, f ranges.ymin ranges.ymax t, labelopt
      in
      (fun ranges _ _ ->
         List.map (g ranges) t_list)
  | `Fixed_abs t_list ->
      let m = ref 0 in (*Used in case of Fixed labels*)
      let g ranges x_axis (v,b) =
        let labelopt =
          if b then (*major tic => add a label*)
            try
              let label = labels !m in
              m := !m + 1;
              Some label
            with Too_few_labels -> None
          else (*Minor tic => no label*)
            None
        in
        let diffx = ranges.xmax -. ranges.xmin
        and diffy = ranges.ymax -. ranges.ymin in
        if x_axis then
          let t = (v -. ranges.xmin) /. diffx in
          let w = ranges.ymin +. t *. diffy in
          v, w, labelopt
        else
          let t = (v -. ranges.ymin) /. diffy in
          let w = ranges.xmin +. t *. diffx in
          w,v,labelopt
      in
      (fun ranges x_axis _ ->
         let fun_filter =
           let between a b (x,_) = a <= x && x<= b in
           if x_axis then between ranges.xmin ranges.xmax
           else between ranges.ymin ranges.ymax
         in
         List.map (g ranges x_axis) (List.filter fun_filter t_list))
  | `Linear_variable numbers ->
      let major_number = ref 0 in
      let f (list, len) m =
        incr major_number;
        let rec do_minors list i =
          if i <= 0 then list
          else
            do_minors ((len + m - i, None)::list) (i-1)
        in
        let list = do_minors list m in
        let labelopt =
          try Some (labels !major_number)
          with Too_few_labels -> None
        in
        ((len + m, labelopt)::list, len + m + 1)
      in
      let list_tics, nall =
        let first_tic =
          [0, (try Some (labels 0) with Too_few_labels -> None)]
        in
        Array.fold_left f (first_tic, 1) numbers
      in
      (fun ranges _ _ ->
         let xstep = (ranges.xmax -. ranges.xmin) /. (float nall)
         and ystep = (ranges.ymax -. ranges.ymin) /. (float nall) in
         List.rev_map
           (fun (i,label) ->
              let t = float i in
              ranges.xmin +. t *. xstep, ranges.ymin +. t *. ystep, label)
           list_tics
      )
  | `Linear(majors,minors) ->
      let nall = minors * (majors - 1) + majors in
      let rec make_list list i =
        if i >= nall then list
        else
          let label =
            let j,k = i/(minors + 1), i mod (minors + 1) in
            if k = 0 then
              try Some (labels j)
              with Too_few_labels -> None
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
      (fun ranges _ _ ->
         let xstep = (ranges.xmax -. ranges.xmin) /. (float nall)
         and ystep = (ranges.ymax -. ranges.ymin) /. (float nall) in
         List.rev_map
           (fun (i,label) ->
              let t = float i in
              ranges.xmin +. t *. xstep, ranges.ymin +. t*. ystep, label)
           list)
  | `Logarithmic(majors, minors) ->
      let nall = minors * (majors - 1) + majors in
      let rec make_list list i =
        if i >= nall then list
        else
          let j,k = i/(minors + 1), i mod (minors + 1) in
          let label =
            if k = 0 then
              try Some (labels j)
              with Too_few_labels -> None
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
      (fun ranges _ _ ->
         let xmajorstep = (ranges.xmax -. ranges.xmin) /. (float majors)
         and ymajorstep = (ranges.ymax -. ranges.ymin) /. (float majors) in
         List.rev_map
           (fun (j,k,label) ->
              let t = float j in
              let x0 = ranges.xmin +. t *. xmajorstep
              and y0 = ranges.ymin +. t *. ymajorstep in
              let f mstep v0 =
                let ministep = mstep *. (float k) /. (float (minors + 1)) in
                let g st = log10 (1. +. st /. v0) in
                x0 +. mstep *. (g ministep) /. (g mstep)
              in
              f xmajorstep x0, f ymajorstep y0, label)
           list)
  | `Auto_linear ->
      let distances =
        [|[1;2;5;10];
          [2;4;5;10;20];
          [2;3;5;6;10;15;30];
          [2;4;5;8;10;20;40];
          [2;5;10;25;50];
          [2;3;4;5;6;10;12;15;20;30;60];
          [2;5;7;10;14;35;70];
          [2;4;5;8;10;16;20;40;80];
          [2;3;5;6;9;10;15;18;30;45;90]|]
      in
      (fun ranges x_axis backend ->
         let get_dist v =
           let rect = Backend.text_extents backend (string_of_float v) in
           fun x_axis -> if x_axis then rect.Backend.w else rect.Backend.h
         in
         let vmin, vmax =
           if x_axis then ranges.xmin, ranges.xmax
           else ranges.ymin, ranges.ymax
         in
         let diff = vmax -. vmin in
         let dist_min = get_dist vmin x_axis in
         (*Get the significant digit in order to find which list
           of steps to use*)
         let diffexp = floor (log10 diff) in
         let order = 10. ** diffexp in
         let significant_digit = truncate (vmin /. order) in
         let dist = float (10 * significant_digit) in
         let distances = distances.(significant_digit -1) in
         (*In this list, find the smallest number for which the
           texts won't overlap; and return the corrsponding step to use.*)
         let rec find_minimal_dist dists =
           match dists with
             [] -> (*Overlap for all distances => labels only at the boundaries*)
               diff
           | d::dists ->
               let ddist = diff *. (float d) /. dist in
               let len = get_dist (vmin +. ddist *. order) x_axis in
               (*Condition of acceptance: if the two first boxes are as follows:

                 *Half of the first box;
                 *Space
                 *Half of the second box

                 then the space is at least equal to the space
                 occupied by the boxes in this interval.  *)
               if (len > (ddist +. dist_min)) then ddist
               else find_minimal_dist dists
         in
         let step = find_minimal_dist distances in
         let new_vmax = vmin +. dist *. order in
         let diffx = ranges.xmax -. ranges.xmin
         and diffy = ranges.ymax -. ranges.ymin in
         let rec make_list list i =
           let v = new_vmax -. (float i) *. step *. order in
           if v >= vmin then
             let label =
               { action =
                   (fun x y pos t ->
                      Backend.show_text t 0. x y pos (string_of_float v));
                 box = make_box_from_text (string_of_float v);
                 rotation = 0.}
             in
             let data =
               if x_axis then
                 let t = (v -. ranges.xmin) /. diffx in
                 let w = ranges.ymin +. t *. diffy in
                 v, w, Some label
               else
                 let t = (v -. ranges.ymin) /. diffy in
                 let w = ranges.xmin +. t *. diffx in
                 w,v, Some label
             in make_list (data::list) (i+1)
           else list
         in make_list [] 0
      )
  | _ -> raise Not_available

type 'a axis = (*'a for tic type*)
    {x_axis:bool;
     major:'a; minor:'a;
     positions:tic_position;
     label_position:text_position}

type ('a,'b) t = (*'a for axes type, 'b for tic types*)
    {axes:'a; x:'b axis; y:'b axis}


let make_axis x major data label_position minor ?(get_labels = get_labels)
    ?(get_position = get_position) loc =
  let labels = get_labels x data in
  let positions = get_position loc labels in
  {x_axis = x; major = major; minor = minor; positions = positions;
   label_position = label_position}

let make_xaxis m = make_axis true m
let make_yaxis m = make_axis false m

let make axes x y =
  if y.x_axis || not x.x_axis then invalid_arg "Axes.make";
  {axes = axes; x = x; y = y}

(*FIXME: need of a range??*)
let print_tics axis ranges print_tic backend =
  let rec print = function
      [] -> ()
    | (x,y,label)::l ->
        Backend.move_to backend x y;
        match label with
          None -> print_tic backend axis.minor
        | Some label ->
            print_tic backend axis.major;
            label.action x y axis.label_position backend;
            print l
  in print (axis.positions ranges axis.x_axis backend)

let axis_margins axis ranges tic_extents backend =
  let list = axis.positions ranges axis.x_axis backend in
  let rec make_rect list rect =
    match list with
      [] -> rect
    | (x,y,label)::l ->
        let tic = match label with None -> axis.minor | Some _ -> axis.major in
        let extents = tic_label_extents
          (tic_extents tic) label x y axis.label_position backend
        in
        let xmin,ymin = extents.Backend.x, extents.Backend.y in
        let xmax,ymax = xmin +. extents.Backend.w, ymin +. extents.Backend.h in
        let newrect = rectangle_extents rect
          (xmin +. x) (xmax +. x) (ymin +. y) (ymax +. y)
        in
        make_rect l newrect
  in make_rect list {Backend.x=ranges.xmin; y=ranges.ymin; w=0.;h=0.}


let get_margins t ?(axes_meeting = axes_meeting) ?(tic_extents = tic_extents)
    ranges backend =
  let x,y = axes_meeting t.axes ranges in
  Ranges.update ranges x y;

  let coord = Matrix.make_translate ranges.xmin ranges.ymin in
  Matrix.scale coord
    (ranges.xmax -. ranges.xmin) (ranges.ymax -. ranges.ymin);
  let initial = Backend.get_matrix backend in
  Backend.save backend;
  Backend.set_matrix backend (Matrix.mul coord initial);
  let margins =
    axis_margins t.x ranges tic_extents backend,
    axis_margins t.y ranges tic_extents backend
  in
  Backend.restore backend;
  margins

let print t ~lines ~ranges
    ?(print_axes = print_axes) ?(axes_meeting = axes_meeting)
    ?(print_tic = print_tic) backend =
  (*let xxx = backend and yyy = Backend.get_handle backend in*)
  let x,y = axes_meeting t.axes ranges in
  print_axes t.axes ranges backend;
  let ctm = Coordinate.use backend lines in
  Backend.stroke backend;
  Coordinate.restore backend ctm;
  let xrange = Ranges.make ranges.xmin y in
  Ranges.update xrange ranges.xmax y;
  let yrange = Ranges.make x ranges.ymin in
  Ranges.update yrange x ranges.ymax;
  print_tics t.x xrange print_tic backend; (*X axis*)
  print_tics t.y yrange print_tic backend  (*Y axis*)
