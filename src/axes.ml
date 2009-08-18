(**Styles definitions to make axes*)
module C = Coord_handler

type axes = [`Rectangle of bool * bool | `Two_lines of float * float]
    (*Style of axes.*)
type tic = [`P of Pointstyle.name]
    (*Style of tics.*)
type loc_tics =
    [ `Linear
    | `Logarithmic]
      (*Positionment of tics.*)
type mode_tics =
    Automatic
  | Semi_automatic of float
  | Fixed of int * int
      (*Number of tics.*)

type data =
    [ `Numbers
    | `Other of string list ]
      (*What data to be printed 'near' a major tic.*)


type ('a,'b,'c) axis =
    {major:'a; minor:'a; loc:'b; mode:mode_tics; data:'c}

type ('a,'b,'c,'d) t =
    {axes:'a; x:('b,'c,'d) axis; y:('b,'c,'d) axis}


exception Not_available

let make_axis major minor loc mode data =
  {major = major; minor = minor; loc = loc; mode = mode; data = data}

let make axes x y =
  {axes = axes; x = x; y = y;}

let print_axes axes ~xmin ~xmax ~ymin ~ymax ch =
  match axes with
    `Rectangle(bx, by) ->
      let x, y = min xmin xmax, min ymin ymax in
      let w, h = abs_float (xmax -. xmin), abs_float (ymax -. ymin) in
      C.rectangle ch x y w h;
      (if bx then xmin else xmax),
      (if by then ymin else ymax)
  | `Two_lines(x,y) ->
      (*Need to update -before- so that mins/maxs are correctly
        initialized for making the axes lines.*)
      let xmin,ymin = min x xmin, min y ymin
      and xmax,ymax = max x xmax, max y ymax in
      C.move_to ch xmin y;
      C.line_to ch xmax y;
      C.move_to ch x ymin;
      C.line_to ch x ymax;
      x,y
  | _ -> raise Not_available

let get_funct loc  =
  match loc with
  | `Linear ->
      (fun n m ->
         let step = 1. /. (float (n*m)) in
         fun i -> (float i) *. step)
  | `Logarithmic ->
      (fun n m i ->
         log (1. +. 9. *. (float i) /. (float (n * m))))
  | _  -> raise Not_available

exception Inner_error

let get_labels data =
  match data with
    `Numbers -> raise Inner_error
  | `Other list ->
      let array = Array.of_list list in
      fun i ->
        (try array.(i)
        with Invalid_argument ioob ->
          failwith ("Iterator from get_labels `Other --"^ioob))
  | _ -> raise Not_available


let print_tic ch tic =
  match tic with `P name ->
    C.render ch name
  | _ -> raise Not_available



let inner_print_tics axis major minor ~vmin ~vmax ~vinv x_axis
    print_tic get_funct get_labels ch =
      let n = major * minor in
      let label =
        try get_labels axis.data
        with Inner_error ->
          fun i ->
            let x = vmin +. (vmax -. vmin) *. (float i) /. (float major) in
            Printf.sprintf "%F"
              (match axis.loc with
               | `Linear -> x
               | `Logarithmic -> 10.**x
               | _ -> x)
      in
      let rec maketic i =
        if i <= n then
          (let r = get_funct axis.loc major minor i in
           let v = vmin +. r *. (vmax -. vmin) in
           let x', y',  pos' =
             if x_axis then
               v, vinv, Backend.CB
             else vinv, v, Backend.LC
           in
           C.move_to ch x' y';
           if i mod minor = 0 then
             (let rect = print_tic ch axis.major in
              Printf.printf "Text on point %f %f : \"%s\"\n%!" x' y'
                (label (i/minor));
              let x'', y'' = match pos' with
                | _ -> x', y'
              in
              C.show_text ch 0. x'' y'' pos' (label (i/minor)))
           else ignore (print_tic ch axis.minor);
           maketic (i+1))
      in maketic 0
      (*Restoring to previous coordinates.*)



let print_tics axis ~vmin ~vmax ~vinv x_axis print_tic get_funct get_labels ch =
  let find_major spacing estim =
    let distx, disty =
      if x_axis then vmax -. vmin, 0.
      else 0., vmax -. vmin
    in
    let len1, len2 =
      C.to_device_distance ch distx disty
    in
    let length = if x_axis then len1 else len2 in
    let rmin, rmax =
      C.text_extents ch (string_of_float vmin),
      C.text_extents ch (string_of_float vmax)
    in
    let hmin, hmax = if x_axis then
      rmin.Backend.w, rmax.Backend.w
    else rmax.Backend.h, rmax.Backend.h
    in
    (*FIXME: All the stuff is in device coordinates.*)
    let estim_len = estim hmin hmax in
    let num = length -. estim_len in
    let frac1 = num *. 2. /. (hmax +. hmin) in
    let frac2 = (frac1 -. 1.) /. spacing in
    (truncate frac2) + 2
  in
  match axis.mode with
    Automatic ->
      inner_print_tics axis (find_major 2. max) 1 ~vmin ~vmax ~vinv x_axis
        print_tic get_funct get_labels ch
  | Semi_automatic(spacing) ->
      inner_print_tics axis (find_major spacing max) 1 ~vmin ~vmax ~vinv
        x_axis
        print_tic get_funct get_labels ch
  | Fixed(major, minor) ->
      inner_print_tics axis major minor ~vmin ~vmax ~vinv x_axis
        print_tic get_funct get_labels ch

let print t ~xmin ~xmax ~ymin ~ymax ?(print_axes = print_axes)
    ?(print_tic = print_tic) ?(get_funct = get_funct)
    ?(get_labels = get_labels) ch =
  (*let xxx = ch and yyy = C.get_handle ch in*)
  let x,y = print_axes t.axes ~xmin ~xmax ~ymin ~ymax ch in
  let coord = Coordinate.make_identity () in
  C.stroke_init ch;
  print_tics t.x ~vmin:xmin ~vmax:xmax ~vinv:y true
    print_tic get_funct get_labels ch;
  print_tics t.y ~vmin:ymin ~vmax:ymax ~vinv:x false
    print_tic get_funct get_labels ch;



(*Local variables:*)
(*compile-command: "ocamlopt -c -dtypes axes.ml && ocamlc -c -dtypes axes.ml"*)
(*End:*)
