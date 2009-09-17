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

type ranges =
    {x1:float;x2:float;y1:float;y2:float}

module FixedRanges =
struct
  type t =
      {mutable xmin:float;
       mutable ymin:float;
       mutable xmax:float;
       mutable ymax:float;}

  let make x y = {xmin = x;ymin = y; xmax = x; ymax = y}

  let update rg x y =
    let really_update =
      rg.xmin > x || rg.xmax < x || rg.ymin > y || rg.ymax < y
    in
    if rg.xmin > x then rg.xmin <- x;
    if rg.xmax < x then rg.xmax <- x;
    if rg.ymin > y then rg.ymin <- y;
    if rg.ymax < y then rg.ymax <- y;
    really_update

  let copy ranges = {ranges with xmin = ranges.xmin}

  let of_rect rect =
    {xmin = rect.x; ymin = rect.y;
     xmax = rect.x +. rect.w;
     ymax = rect.x +. rect.h}

  let to_rect rg =
    {x = rg.xmin; y = rg.ymin;
     w = rg.xmax -. rg.xmin;
     h = rg.ymax -. rg.ymin}

  let of_ranges ur =
    let xmin = min ur.x1 ur.x2
    and ymin = min ur.y1 ur.y2
    and xmax = max ur.x1 ur.x2
    and ymax = max ur.y1 ur.y2
    in
    {xmin = xmin; xmax = xmax; ymin = ymin; ymax = ymax}

  let to_ranges ?(xswitch=false) ?(yswitch=false) ranges =
    let x,x' =
      if xswitch then ranges.xmax, ranges.xmin
      else ranges.xmin, ranges.xmax
    and y,y' =
      if yswitch then ranges.ymax, ranges.ymin
      else ranges.ymin, ranges.ymax
    in
    {x1 = x; x2 = x'; y1 = y; y2 = y'}

end

type fixed_ranges = FixedRanges.t =
    private {mutable xmin:float;
             mutable ymin:float;
             mutable xmax:float;
             mutable ymax:float}

exception Not_available

let rectangle_extents rect xmin xmax ymin ymax =
  let xmin, xmax =
    let x0 = rect.x in
    let x1 = x0 +. rect.w in
    min x0 xmin, max x1 xmax
  and ymin, ymax =
    let y0 = rect.y in
    let y1 = y0 +. rect.h in
    min y0 ymin, max y1 ymax
  in
  {x = xmin; y = ymin; w = xmax -. xmin; h = ymax -. ymin}


let translate_for_tic x y tic_ext marks pos =
  let w1 = tic_ext.x *. marks
  and h1 = tic_ext.y *. marks in
  let w2 = w1 +. tic_ext.w *. marks
  and h2 = h1 +. tic_ext.h *. marks in
  let x' =
    match pos with
    | LT | LC | LB -> x +. w1
    | CT | CC | CB -> x
    | RT | RC | RB -> x +. w2
  and y' =
    match pos with
    | LT | CT | RT -> y +. h2
    | LC | CC | RC -> y
    | LB | CB | RB -> y +. h1
  in
  Printf.printf "(%f,%f) -- (%f,%f)%!" x y x' y';
  x', y'

type axes =
    [ `None of bool * bool
    | `Rectangle of bool * bool
    | `Two_lines of float * float
    | `Two_lines_rel of float * float
    ]
    (*Style of axes.*)




let print_axes axes ranges backend =
  let xmin, xmax =
    if ranges.x1 > ranges.x2 then ranges.x2, ranges.x1
    else ranges.x1, ranges.x2
  and ymin, ymax =
    if ranges.y1 > ranges.y2 then ranges.y2, ranges.y1
    else ranges.y1, ranges.y2
  in
  match axes with
    `None (_,_) -> ()
  | `Rectangle(_, _) ->
      Backend.rectangle backend
        xmin ymin (xmax -. xmin) (ymax -. ymin)
  | `Two_lines(x,y) ->
      (*Need to update -before- so that mins/maxs are correctly
        initialized for making the axes lines.*)
      let xmin = min x xmin
      and ymin = min y ymin
      and xmax = max x xmax
      and ymax = max y ymax in
      Backend.move_to backend xmin y;
      Backend.line_to backend xmax y;
      Backend.move_to backend x ymin;
      Backend.line_to backend x ymax
  | `Two_lines_rel(t,u) ->
      (*Need to update -before- so that mins/maxs are correctly
        initialized for making the axes lines.*)
      let x = xmin +. t *. (xmax -. xmin)
      and y = ymin +. u *. (ymax -. ymin) in
      let xmin = min x xmin
      and ymin = min y ymin
      and xmax = max x xmax
      and ymax = max y ymax in
      Backend.move_to backend xmin y;
      Backend.line_to backend xmax y;
      Backend.move_to backend x ymin;
      Backend.line_to backend x ymax
  | _ -> raise Not_available

let axes_meeting axes ranges =
  let ranges = FixedRanges.of_ranges ranges in
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
  match tic with `P name ->
    Pointstyle.extents name
  | _ -> raise Not_available

type label =
    {action: float -> float -> text_position -> Backend.t -> unit;
     box: float -> float -> text_position -> Backend.t -> rectangle;
     rotation:float}

(*Extents as if the tic has been made at (0,0). [tic_ext] is given in
  'marks' coordinates.  We work (and give the result) in normalized
  coordinates.*)
let tic_label_extents tic_ext marks label x y x' y' pos b =
  let tic_ext =
    {x = tic_ext.x *. marks;
     y = tic_ext.y *. marks;
     w = tic_ext.w *. marks;
     h = tic_ext.h *. marks}
  in
  match label with
    None -> tic_ext
  | Some label ->
      (*Assert [b] is in normalized coords.*)
      let x, y = translate_for_tic x y tic_ext marks pos in
      let box = label.box x' y' pos b in
      let matrix = Matrix.make_rotate label.rotation in
      let final_box =
        Matrix.transform_rectangle ~dist_basepoint:false matrix box in
      (*Note: the base point need not to be transformed.*)
      let x'' = final_box.x +. final_box.w
      and y'' = final_box.y +. final_box.h in
      Printf.printf "Ext: tic %f %f w%f h%f; label %f, %f to %f, %f\n%!"
        tic_ext.x tic_ext.y tic_ext.w tic_ext.h final_box.x final_box.y x'' y'';
      rectangle_extents tic_ext final_box.x x'' final_box.y y''

(*FIXME: really need a range?*)
type tic_position = ranges -> bool -> Backend.t ->
      (float*float*label option) list

type label_collection = int -> label

exception Too_few_labels

type data =
    [ `Text_label of string array * float
    | `Number
    | `Expnumber]

let make_box_from_text txt pos b =
  let rect' = Backend.text_extents b txt in
  let rect =
    Matrix.inv_transform_rectangle ~dist_basepoint:true
      (Backend.get_matrix b) rect'
  in
  let x = match pos with
    | CC | CT | CB -> rect.x -. 0.5 *. rect.w
    | RC | RT | RB -> rect.x
    | LC | LT | LB -> rect.x -. rect.w
  and y = match pos with
    | CC | RC | LC -> rect.y -. 0.5 *. rect.h
    | CT | RT | LT -> rect.y
    | CB | RB | LB -> rect.y -. rect.h
  in
  Printf.printf "TE of %s : %f %f %f %f\nto %f %f %f %f -> base %f %f\n%!"
    txt rect'.x rect'.y rect'.w rect'.h rect.x rect.y rect.w rect.h x y;
  {x = x; y = y; w = rect.w; h = rect.h}

let make_action_from txt rotate x y pos t =
  Backend.show_text t rotate x y pos txt;
  let rect = make_box_from_text txt pos t in
  Backend.set_color t (Color.make ~a:0.3 1. 0.5 0.);
  Backend.rectangle t (x +. rect.x) (y+.rect.y) rect.w rect.h;
  Backend.set_color t Color.black

let get_labels x_axis data =
  match data with
  | `Text_label(txts, rotate) ->
      let f txt =
        {action = make_action_from txt rotate;
         box = (fun _ _  ->  make_box_from_text txt);
         rotation = rotate}
      in
      let array = Array.map f txts in
      (fun i -> try array.(i) with Invalid_argument _ -> raise Too_few_labels)

  | `Number ->
      let txt x y = Printf.sprintf "%g" (if x_axis then x else y) in
      (fun _ ->
         {action = (fun x y -> make_action_from (txt x y) 0. x y);
          box = (fun x y -> make_box_from_text (txt x y));
          rotation = 0.})

  | `Expnumber ->
      let txt x y = Printf.sprintf "%g" (if x_axis then 10.**x else 10.**y) in
      (fun _ ->
         {action = (fun x y -> make_action_from (txt x y) 0. x y);
          box = (fun x y -> make_box_from_text (txt x y));
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
      let g ranges x_axis (t,b) =
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
        if x_axis then t,0.,labelopt
        else 0.,t, labelopt
        (*f ranges.x1 ranges.x2 t, f ranges.y1 ranges.y2 t, labelopt*)
      in
      (fun ranges x_axis  _ ->
         List.map (g ranges x_axis) t_list)
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
        let diffx = ranges.x2 -. ranges.x1
        and diffy = ranges.y2 -. ranges.y1 in
        if x_axis then
          let t = (v -. ranges.x1) /. diffx in
          let w = ranges.y1 +. t *. diffy in
          t, 0., labelopt
        else
          let t = (v -. ranges.y1) /. diffy in
          let w = ranges.x1 +. t *. diffx in
          0.,t,labelopt
      in
      (fun ranges x_axis _ ->
         let fun_filter =
           let between a b (x,_) =
             (a <= b && a <= x && x<= b) || (a >= x && x >= b (* && a > b*))
           in
           if x_axis then between ranges.x1 ranges.x2
           else between ranges.y1 ranges.y2
         in
         List.map (g ranges x_axis) (List.filter fun_filter t_list))
  | `Linear_variable numbers ->
      let major_number = ref 0 in
      let f (list, len) m =
        (*List with elements of the form (i, label): [i]th tic gets
          [x] as label, provided [label] is [Some x]. [len] contains
          the number of tics already made. [m] is the number of minor
          tics wanted in the current interval.*)
        incr major_number;
        (*[major_number] is used to get the correct label when needed.*)
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
      (fun ranges x_axis _ ->
         let number_of_tics = float (nall - 1) in
         let xstep = (ranges.x2 -. ranges.x1) /. (float (nall - 1))
         and ystep = (ranges.y2 -. ranges.y1) /. (float (nall - 1)) in
         List.rev_map
           (fun (i,label) ->
              let t = float i /. number_of_tics in
              (*ranges.x1 +. t *. xstep, ranges.y1 +. t *. ystep, label)*)
              if x_axis then t,0.,label else 0.,t,label)
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
      (fun ranges x_axis _ ->
         let number_of_tics = float (nall - 1) in
         let xstep = (ranges.x2 -. ranges.x1) /. (float (nall - 1))
         and ystep = (ranges.y2 -. ranges.y1) /. (float (nall - 1)) in
         List.rev_map
           (fun (i,label) ->
              let t = float i /.number_of_tics in
              (*ranges.x1 +. t *. xstep, ranges.y1 +. t*. ystep, label)*)
              if x_axis then t,0.,label else 0.,t,label)
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
      (fun ranges x_axis _ ->
         let number_of_majors = float (majors - 1) in
         let xmajorstep = (ranges.x2 -. ranges.x1) /. (float (majors - 1))
         and ymajorstep = (ranges.y2 -. ranges.y1) /. (float (majors - 1))
         in
         List.rev_map
           (fun (j,k,label) ->
              let t = float j /. number_of_majors in
             (* let x0 = ranges.x1 +. t *. xmajorstep
              and y0 = ranges.y1 +. t *. ymajorstep in*)
              (*let f mstep v0 =
                let ministep = mstep *. (float k) /. (float (minors + 1)) in
                let g st = log10 (1. +. st /. v0) in
                x0 +. mstep *. (g ministep) /. (g mstep)
              in
                f xmajorstep x0, f ymajorstep y0, label *)
              let ministep = (float k) /.
                ((float (minors + 1)) *. number_of_majors)
              in
              let g st = log10 (1. +. st /. t) in
              let t' =
                (g ministep) /. (g (1./.number_of_majors) *. number_of_majors)
              in
              if x_axis then t+.t',0.,label
              else 0.,t+.t',label
           )
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
           fun x_axis -> if x_axis then rect.w else rect.h
         in
         let vmin, vmax =
           if x_axis then ranges.x1, ranges.x2
           else ranges.y1, ranges.y2
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
         let diffx = ranges.x2 -. ranges.x1
         and diffy = ranges.y2 -. ranges.y1 in
         let rec make_list list i =
           let v = new_vmax -. (float i) *. step *. order in
           if v >= vmin then
             let label =
               { action = make_action_from (Printf.sprintf "%g" v) 0.;
                 box = (fun _ _  -> make_box_from_text (Printf.sprintf "%g" v));
                 rotation = 0.}
             in
             let data =
               if x_axis then
                 let t = (v -. ranges.x1) /. diffx in
                 let w = ranges.y1 +. t *. diffy in
                 t,0., Some label
               else
                 let t = (v -. ranges.y1) /. diffy in
                 let w = ranges.x1 +. t *. diffx in
                 0.,t, Some label
             in make_list (data::list) (i+1)
           else list
         in make_list [] 0
      )
  | _ -> raise Not_available

type 'a axis = (*'a for tic type*)
    {x_axis:bool;
     major:'a; minor:'a;
     major_extents: rectangle; minor_extents: rectangle;
     positions:tic_position;
     label_position:text_position}

type ('a,'b) t = (*'a for axes type, 'b for tic types*)
    {axes:'a; xaxis:'b axis; yaxis:'b axis}


let make_axis x major data label_position minor ?(get_labels = get_labels)
    ?(get_position = get_position) ?(tic_extents = tic_extents) loc =
  let labels = get_labels x data in
  let positions = get_position loc labels in
  {x_axis = x; major = major; minor = minor;
   major_extents = tic_extents major; minor_extents = tic_extents minor;
   positions = positions; label_position = label_position}

let make_xaxis m = make_axis true m
let make_yaxis m = make_axis false m

let make axes x y =
  if y.x_axis || not x.x_axis then invalid_arg "Axes.make";
  {axes = axes; xaxis = x; yaxis = y}

(*FIXME: need of a range?*)
let print_tics axis ranges print_tic normalization marks font_size backend =
  let rec print = function
      [] -> ()
    | (x,y,label)::l ->
        let x' = ranges.x1 +. x *. (ranges.x2 -. ranges.x1)
        and y' = ranges.y1 +. y *. (ranges.y2 -. ranges.y1) in
        Backend.move_to backend x' y';
        Printf.printf "*%!";
        let user_coords = Coordinate.use backend normalization in
        Backend.scale backend marks marks;
        (
          match label with
            None ->
              Printf.printf "m%!";
              print_tic backend axis.minor;
              Coordinate.restore backend user_coords
          | Some label ->
              Printf.printf "M%!";
              print_tic backend axis.major;
              Coordinate.restore backend user_coords;
              let x1, y1 =
                translate_for_tic x y axis.major_extents marks
                  axis.label_position
              in
              let x1' = ranges.x1 +. x1 *. (ranges.x2 -. ranges.x1)
              and y1' = ranges.y1 +. y1 *. (ranges.y2 -. ranges.y1) in
              Printf.printf "l(%f,%f)(vp coords)%!" x1 y1;
              label.action x1' y1' axis.label_position backend);
        Printf.printf "-%!";
        print l
  in
  Backend.set_font_size backend font_size;
  Printf.printf ">%!";
  print (axis.positions ranges axis.x_axis backend)

type margins =
    {left: float; right: float; top: float; bottom: float}

let axis_margins normalization marks font_size ranges backend axis =
  Backend.set_font_size backend font_size;
  let list = axis.positions ranges axis.x_axis backend in
  let rec make_margins list left right top bottom =
    Printf.printf "M %f %f %f %f\n%!" left right top bottom;
    match list with
      [] -> Printf.printf "ok\n%!";
        {left = -.left; right = right; top = top; bottom = -.bottom}
    | (x,y,label)::l ->
        let x' = ranges.x1 +. x *. (ranges.x2 -. ranges.x1)
        and y' = ranges.y1 +. y *. (ranges.y2 -. ranges.y1) in
        let tic_ext =
          match label with
            None -> axis.minor_extents
          | Some _ -> axis.major_extents
        in
        (*let ctm = Coordinate.use backend normalization in*)
        let extents =
          tic_label_extents tic_ext marks label x y
            x' y' axis.label_position backend
        in
        (*Coordinate.restore backend ctm;*)
        Printf.printf "data: %f %f %f %f\n%!"
          extents.x extents.y extents.w extents.h;
        let xmin = min left extents.x
        and ymin = min bottom extents.y in
        let xmax = max (xmin +. extents.w) right
        and ymax = max (ymin +. extents.h) top in
        make_margins l xmin xmax ymax ymin
  in make_margins list 0. 0. 0. 0.

let get_margins t ?(axes_meeting = axes_meeting) ~normalization
    ~lines ~marks ~font_size ranges backend =
  let x,y = axes_meeting t.axes ranges in
  let axes_ranges = FixedRanges.make x y in
  ignore (FixedRanges.update axes_ranges ranges.x1 ranges.y1);
  ignore (FixedRanges.update axes_ranges ranges.x2 ranges.y2);
  (*let coord = Matrix.make_translate axes_ranges.xmin axes_ranges.ymin in
  Matrix.scale coord
    (axes_ranges.xmax -. axes_ranges.xmin) (axes_ranges.ymax -. axes_ranges.ymin);
  let initial = Backend.get_matrix backend in
  Backend.save backend;
  (*Put backend into graph coordinates.*)
  Backend.set_matrix backend (Matrix.mul initial coord);*)
  let axes_ranges = FixedRanges.to_ranges axes_ranges in
  let margins =
    axis_margins normalization marks font_size axes_ranges backend
  in
  let margins = margins t.xaxis, margins t.yaxis in
  margins

let print t ~normalization ~lines ~marks ~font_size ~ranges
    ?(print_axes = print_axes) ?(axes_meeting = axes_meeting)
    ?(print_tic = print_tic) backend =
  let x,y = axes_meeting t.axes ranges in
  print_axes t.axes ranges backend;
  let ctm = Coordinate.use backend normalization in
  Backend.set_line_width backend lines;
  Backend.stroke backend;
  Coordinate.restore backend ctm;
  let xrange = {x1=ranges.x1; x2 = ranges.x2; y1 = y; y2 = y} in
(*
 Ranges.make ranges.xmin y in
  ignore (Ranges.update xrange ranges.xmax y);*)
  let yrange = {x1=x; x2 = x; y1 = ranges.y1; y2 = ranges.y2} in
(*
 Ranges.make x ranges.ymin in
  ignore (Ranges.update yrange x ranges.ymax);*)
  Printf.printf "X%!";
  print_tics t.xaxis xrange print_tic normalization marks font_size backend;
  (*X axis*)
  Printf.printf "Y%!";
  print_tics t.yaxis yrange print_tic normalization marks font_size backend;
  (*Y axis*)
