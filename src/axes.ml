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
         fun i -> (float i) *. step, i mod m = 0)
  | `Logarithmic ->
      (fun n m i ->
         log (1. +. 9. *. (float i) /. (float (n * m))),
         i mod m = 0)
  | _  -> raise Not_available

exception Inner_error

let get_labels data =
  match data with
    `Numbers -> raise Inner_error
  | `Other list ->
      let u = ref list in
      (fun () -> match !u with
        [] -> failwith "No labels"
       | s::l -> u := l; s)
  | _ -> raise Not_available


let print_tic ch tic =
  match tic with `P name ->
    C.render ch name
  | _ -> raise Not_available


let print_tics_normalized axis vmin vmax x_axis
    print_tic get_funct get_labels ch =
  let loc_tic = get_funct axis.loc in
  match axis.mode with
    Automatic -> failwith "NYI"
  | Fixed(major, minor) ->
      let n = (major - 1) * minor + 1 in
      let f = loc_tic major minor in
      let next_label =
        try
          get_labels axis.data
        with Inner_error ->
          let i = ref 0 in
          fun () ->
            let x = vmin +. (vmax -. vmin) *. (float !i) /. (float n) in
            incr i;
            Printf.sprintf "%f"
              (match axis.loc with
               | `Linear -> x
               | `Logarithmic -> 10.**x
               | _ -> failwith"Undefined mode")
      in
      let rec maketic i =
        if i <= n then
          let v, is_major = f i in
          C.move_to ch v 0.;
          print_tic ch (if is_major then axis.major else axis.minor);
          if is_major then
            let rotate = if x_axis then 0. else (-2.) *. atan 1. in
            C.show_text ch rotate v 0.
              (if x_axis then Backend.CB else Backend.LC) (next_label ());
            maketic (i+1)
          else maketic (i+1)
      in maketic 0

let print_tics axis ~vmin ~vmax ~vinv x_axis print_tic get_funct get_labels ch =
  C.save ch;
  if x_axis then (*vmin = xmin,...*)
    C.add_translate ch "~axis" vmin vinv
  else (*vmin = ymin,... but we will rotate*)
    (C.add_translate ch "~axis" vinv vmin;
     C.rotate ch ~name:"~axis" ~angle:(2.*. atan 1.));
      (*FIXME: the rotation makes Y as new X but -X as new Y.*)

  (*Axis to be made is now on X axis, between abscissas 0. and (vmax
    -. vmin). We now scale it to put it between 0. and 1.*)
  C.set_coordinate ch "~axis";
  let diff = vmax -. vmin in
  C.scale ch diff diff;
  print_tics_normalized axis vmin vmax x_axis print_tic get_funct get_labels ch;
  (*Restoring to previous coordinates.*)
  C.restore ch

let pa = print_axes and pt = print_tic and gf = get_funct and gl = get_labels

let print t ~xmin ~xmax ~ymin ~ymax ?print_axes ?print_tic
    ?(get_funct=get_funct) ?get_labels ch =
  let print_axes = match print_axes with None -> pa | Some u -> u in
  let x,y = print_axes t.axes ~xmin ~xmax ~ymin ~ymax ch in
  let print_tic = match print_tic with None -> pt | Some u -> u
 (* and get_funct = match get_funct with None -> gf | Some u -> u*)
  and get_labels = match get_labels with None -> gl | Some u -> u
  in
  print_tics t.x ~vmin:xmin ~vmax:xmax ~vinv:y true
    print_tic get_funct get_labels ch;
  print_tics t.y ~vmin:ymin ~vmax:ymax ~vinv:x false
    print_tic get_funct get_labels ch


(*Local variables:*)
(*compile-command: "ocamlopt -c -for-pack Archimedes axes.ml && ocamlc -c -for-pack Archimedes axes.ml"*)
(*End:*)
