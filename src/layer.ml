module B = Backend
module Q = Queue
module Coord = B.Matrix

module T = Transform_coord

type limitation =
    Unlimited
  | Limited_out of float
  | Limited_in of float
  | Limited of float * float

type scaling =
  | Not_allowed
  | Uniform of limitation
  | Free of limitation * limitation

type styles =
    {(* mutable ps: B.pointstyle;*)
      mutable coord:B.matrix;
      mutable dash: float array * float;
      mutable lj: B.line_join;
      mutable lc: B.line_cap;
      (*  mutable pat:B.pattern;(*FIXME: ? Pattern.t;*)*)
      mutable color: Color.t;
      mutable lw: float;
      mutable font:string;
      mutable fsize:float;
      mutable fslant:B.slant;
      mutable fweight:B.weight;
      mutable settings:int;
      (*This last variable retains which of the above styles have been
        set.  Note that coord is not taken into account (set to
        identity). For the other styles, [settings] flags in the
        following way:

        [dash] set/unset <=> [settings] odd/even;
        [lj] set <=>  [settings] not divisible by 4;
        [lc] <-> div. by 8
        [color] <-> 16
        [lw] <-> 32
        [font], [fslant], [fweight] <-> 64
        [fsize] <-> 128

        See also the variables below.*)
    }
let setting_dash = 1
let setting_line_join = 2
let setting_line_cap = 4
let setting_color = 8
let setting_line_width = 16
let setting_font_face = 32
let setting_font_size = 64

type bkdep =
    TEXT of float * float * float * B.text_position * string

type data =
    BKDEP of bkdep
  | ORDER of (B.t -> unit)
  | SAVING_POINT

type layer_data =
    {
      mutable current: (float * float) option;
      mutable pxmin:float; mutable pxmax:float;
      mutable pymin:float; mutable pymax:float;
      mutable xmin: float; mutable xmax: float;
      mutable ymin: float; mutable ymax: float;
      mutable upmargin: float;
      mutable downmargin: float;
      mutable leftmargin: float;
      mutable rightmargin: float;
    }
type t =
    {
      mutable styles:styles;
      (*Styles to be applied to the orders. We call this when a [get_...] is
        performed.*)
      history: styles Stack.t;
      (*Saved states (as the same way a backend saves its states).*)
      mutable data:layer_data;
      (*All the layer needs to perform the flushes.*)
      saved_data: layer_data Stack.t;
      (*Saved data. This will allow to save, perform some orders
        before flushing somewhere, then retrieve the initial state.*)
      orders: data Q.t;
      bkdep_updates: data Q.t;
    }

type error =
    No_current_point
  | Restore_without_saving of string
  | No_current_path
  | Unset_style of string
  | Non_invertible_initial_matrix

exception Error of error

let string_of_error e = match e with
    No_current_point -> "No current point"
  | Restore_without_saving(s) ->
      "You tried to restore the "^s
      ^" for a layer which does not have saved states concerning "^s^"."
  | No_current_path -> "No current path"
  | Unset_style(s) ->
      "The style "^s
      ^" has not been set in the layer (flushing uses backend settings)"
  | Non_invertible_initial_matrix ->
      "You invoked a function that needs to invert backend's "
      ^"initial transformation matrix but it is non-invertible"


(*These variables are used when flushing, to remember the
  scalings. This permits to make a reverse transformation, to pass the
  'good' data to the backend used. They are used in particular to
  perform the default strokes -- those whose line width is expressed
  in backend coordinates, whereas the path is expressed in layer
  coordinates.*)

let inv_zoomx = ref 1. (*Inverse of scaling factor on X axis*)
let inv_zoomy = ref 1. (*Inverse of scaling factor on Y axis*)
let initial_bk_matrix = ref (Coord.identity ())

let make () =
  let styles =
    {
      coord = Coord.identity ();
      dash = [||], 0.; lj = B.JOIN_BEVEL; lc = B.BUTT;
      color = Color.make 0. 0. 0.; lw = 1.;
      font="Sans serif"; fsize=10.;
      fslant = B.Upright; fweight = B.Normal;
      settings = 0
    }
  and data =
    {
      current = None;
      pxmin = max_float; pxmax = -. max_float;
      pymin = max_float; pymax = -. max_float;
      xmin = max_float; xmax = -. max_float;
      ymin = max_float; ymax = -. max_float;
      upmargin = 0.; downmargin = 0.;
      leftmargin = 0.; rightmargin = 0.;
    }
  in
  {
    styles = styles;  history = Stack.create ();
    data = data;  saved_data = Stack.create ();
    orders = Q.create (); bkdep_updates = Q.create ()
  }

let update t path x y =
  let data = t.data in
  if x < data.xmin then data.xmin <- x;
  if x > data.xmax then data.xmax <- x;
  if y < data.ymin then data.ymin <- y;
  if y > data.ymax then data.ymax <- y;
  if path then(
    if x < data.pxmin then data.pxmin <- x;
    if x > data.pxmax then data.pxmax <- x;
    if y < data.pymin then data.pymin <- y;
    if y > data.pymax then data.pymax <- y)

let reinit_path_ext t =
  let data = t.data in
  data.pxmin <- max_float; data.pxmax <- -. max_float;
  data.pymin <- max_float; data.pymax <- -. max_float;
  data.current <- None

let add_order f t =
  Q.add (ORDER f) t.orders

let add_update f t =
  Q.add (BKDEP f) t.bkdep_updates

let save_stacks t =
  Q.add SAVING_POINT t.orders;
  Q.add SAVING_POINT t.bkdep_updates

let restore_stacks t =
  let rec pop_until_saving_point q =
    match Q.pop q with
      ORDER(_)
    | BKDEP(_) -> pop_until_saving_point q
    | SAVING_POINT  -> ()
  in
  pop_until_saving_point t.orders;
  pop_until_saving_point t.bkdep_updates

let translate t ~x ~y =
  add_order (fun t -> B.translate t x y) t;
  Coord.translate t.styles.coord x y

let scale t ~x ~y =
  add_order(fun t -> B.scale t x y) t;
  Coord.scale t.styles.coord x y

let rotate t ~angle =
  add_order(fun t -> B.rotate t angle) t;
  Coord.rotate t.styles.coord angle
(*
let transform t =  Coord.transform t.styles.coord
let transform_dist t = Coord.transform_dist t.styles.coord
let invert t = Coord.invert t.styles.coord
let inv_transform t =  Coord.inv_transform t.styles.coord
let inv_transform_dist t = Coord.inv_transform_dist t.styles.coord

let apply ~next t =
  (*add_order(fun t -> B.apply ~next t) t;*)
  Coord.apply ~next t.styles.coord
*)

let get_coord t = t.styles.coord
let reset_to_id t =
  add_order(fun t -> B.set_matrix t !initial_bk_matrix) t;
  Coord.reset_to_id t.styles.coord

(*
let set_pointstyle t s =
  add_order(fun t -> B.set_pointstyle t s) t;
  t.ps <- s

let set_pattern t s =
  add_order(fun t -> B.set_pattern t s) t;
  t.pat <- s
*)

(*
Reminder of the flags for settings:
        [dash] set/unset <=> [settings] odd/even;
        [lj] set <=>  [settings] not divisible by 4;
        [lc] <-> div. by 8
        [color] <-> 16
        [lw] <-> 32
        [font], [fslant], [fweight] <-> 64
        [fsize] <-> 128
  We make use of the [setting_*] variables to avoid errors.*)

let set_color t c =
  add_order (fun t -> B.set_color t c) t;
  t.styles.color <- c;
  let set = t.styles.settings
  and n = setting_color in
  if set mod (2 * n) = 0 then t.styles.settings <- set + n

let set_line_width t w =
  add_order (fun t -> B.set_line_width t w) t;
  t.styles.lw <- w;
  let set = t.styles.settings
  and n = setting_line_width in
  if set mod (2 * n) = 0 then t.styles.settings <- set + n


let set_line_cap t lc =
  add_order (fun t -> B.set_line_cap t lc) t;
  t.styles.lc <- lc;
  let set = t.styles.settings
  and n = setting_line_cap in
  if set mod (2 * n) = 0 then t.styles.settings <- set + n


let set_dash t ofs array =
  add_order(fun t -> B.set_dash t ofs array) t;
  t.styles.dash <- array, ofs;
  let set = t.styles.settings
  and n = setting_dash in
  if set mod (2 * n) = 0 then t.styles.settings <- set + n


let set_line_join t s =
  add_order (fun t -> B.set_line_join t s) t;
  t.styles.lj <- s;
  let set = t.styles.settings
  and n = setting_line_join in
  if set mod (2 * n) = 0 then t.styles.settings <- set + n


let set_matrix t matrix =
  add_order (fun t ->
               let m = Coord.copy matrix in
               Coord.apply ~result:m ~next:m !initial_bk_matrix;
               B.set_matrix t m) t;
  t.styles.coord <- matrix

let set_backend_matrix t matrix =
  add_order (fun t -> B.set_matrix t matrix) t;
  try
    let m = Coord.invert !initial_bk_matrix in
    Coord.apply ~next:matrix m;
    t.styles.coord <- m
  with Failure "invert" ->
    raise (Error Non_invertible_initial_matrix)

(*NOTE: the font settings are below, with text managing.*)


(*let get_pointstyle t = t.ps
let get_pattern t = t.pat*)
let get_line_width t =
  if (t.styles.settings mod (2 * setting_line_width)) = 0 then
    raise (Error (Unset_style "line_width"));
  t.styles.lw
let get_line_cap t =
  if (t.styles.settings mod (2 * setting_line_cap)) = 0 then
    raise (Error (Unset_style "line_cap"));
   t.styles.lc
let get_dash t =
  if (t.styles.settings mod (2 * setting_dash)) = 0 then
    raise (Error (Unset_style "line_dash"));
   t.styles.dash
let get_line_join t =
  if (t.styles.settings mod (2 * setting_line_join)) = 0 then
    raise (Error (Unset_style "line_join"));
   t.styles.lj
let get_matrix t = t.styles.coord

let move_to t ~x ~y =
  update t true x y;
  t.data.current <- Some (x,y);
  add_order (fun t -> B.move_to t x y) t

let line t ?x ?y x1 y1 =
  (match x,y with
     Some x, Some y -> move_to t x y
   | _, _ -> ());
  update t true x1 y1;
  add_order (fun t -> B.line_to t x1 y1) t;
  t.data.current <- Some(x1,y1)

let line_to t ~x ~y = line t x y

let get_point t =
  match t.data.current with
    Some(x,y) -> x,y
  | None -> raise (Error No_current_point)

let rel_move_to t ~x ~y =
  let x',y' = get_point t in
  let x',y' = (x+.x'), (y+.y') in
  update t true x' y';
  t.data.current <- Some (x',y');
  add_order (fun t -> B.rel_move_to t x y) t

let rel_line_to t ~x ~y =
  let x',y' = get_point t in
  let x',y' = (x+.x'), (y+.y') in
  update t true x' y';
  t.data.current <- Some(x',y');
  add_order (fun t -> B.rel_line_to t x y) t




let curve t ?x0 ?y0 ~x1 ~y1 ?x2 ?y2 x3 y3 =
  (match x0,y0 with
     Some x, Some y ->
       move_to t x y
   | _, _ -> ());
  let x2, y2 =
    match x2,y2 with
     Some x, Some y ->
       update t true x y;
       x, y
   | _, _ -> x1, y1
  in
  update t true x1 y1; update t true x3 y3;
  add_order (fun t -> B.curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3) t;
  t.data.current <- Some(x3,y3)

let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 = curve t ~x1 ~y1 ~x2 ~y2 x3 y3

let rel_curve_to t ~x1 ~y1 ?x2 ?y2 ~x3 ~y3 =
  let x,y = get_point t in
  let x1, y1, x3, y3 = x +. x1, y +. y1, x +. x3, y +. y3 in
  update t true x1 y1;
  update t true x3 y3;
  let x2, y2 = match x2,y2 with
      Some x', Some y' ->
        let x'' = (x+.x') and y'' = (y+.y') in
        update t true x'' y'';
        x'', y''
   | _, _ -> x1, y1
  in
  t.data.current <- Some(x3, y3);
  add_order (fun t -> B.curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3) t

(*
let ellipse_arc t ?(max_length=2.*. atan 1.) ?x ?y ~a ~b t1 t2 =
  let u,v =
    match x,y with
      Some x, Some y ->  x,y
    | _, _ -> get_point t
  in
  update t true (u+.a) (v+.b);
  update t true (u-.a) (v-.b);
  (*FIXME: to be more precise...*)
  let ct1 = cos t1
  and ct2 = cos t2
  and st1 = sin t1
  and st2 = sin t2 in
  let x1,y1 = u +. a *. ct1, v +. b *. st1
  and x2,y2 = u +. a *. ct2, v +. b *. st2 in
  t.data.current <- Some(x2,y2);
  add_order (fun t -> B.ellipse_arc t ?x ?y ~a ~b t1 t2) t

let ellipse t ?x ?y ~a ~b =
  let u,v =
    match x,y with
      Some x, Some y ->  x,y
    | _, _ -> get_point t
  in
  update t true (u+.a) (v+.b);
  update t true (u-.a) (v-.b);
  add_order (fun t -> B.ellipse t ?x ?y ~a ~b) t;
  t.data.current <- Some((u+.a),v)

let circle_arc t ?x ?y ~r t1 t2=
   let u,v =
    match x,y with
      Some x, Some y ->  x,y
    | _, _ -> get_point t
   in
   (*FIXME: cf. ellipse_arc*)
  update t true (u+.r) (v+.r);
  update t true (u-.r) (v-.r);
  let ct1 = cos t1
  and ct2 = cos t2
  and st1 = sin t1
  and st2 = sin t2 in
  let x1,y1 = u +. r *. ct1, v +. r *. st1
  and x2,y2 = u +. r *. ct2, v +. r *. st2 in
  t.data.current <- Some(x2,y2);
  add_order (fun t -> B.circle_arc t ?x ?y ~r t1 t2) t

let circle t ?x ?y ~r =
  let u,v =
    match x,y with
      Some x, Some y ->  x,y
    | _, _ -> get_point t
  in
  update t true (u+.r) (v+.r);
  update t true (u-.r) (v-.r);
  t.data.current <- Some(u+.r,v);
  add_order (fun t -> B.circle t ?x ?y ~r) t
*)

let arc t ~x ~y ~r ~a1 ~a2=
  (*FIXME: cf. ellipse_arc*)
  update t true (x+.r) (y+.r);
  update t true (x-.r) (y-.r);
  let ct1 = cos a1
  and ct2 = cos a2
  and st1 = sin a1
  and st2 = sin a2 in
  let x1,y1 = x +. r *. ct1, y +. r *. st1
  and x2,y2 = x +. r *. ct2, y +. r *. st2 in
  t.data.current <- Some(x2,y2);
  add_order (fun t -> B.arc t x y r a1 a2) t


let rectangle t ~x ~y ~w ~h =
  update t true x y;
  update t true (x+.w) (y+.h);
  add_order (fun t -> B.rectangle t ~x ~y ~w ~h) t


let save t =
  add_order B.save t;
  Stack.push { t.styles with coord = t.styles.coord} t.history

let restore t =
  add_order B.restore t;
  try
    t.styles <- Stack.pop t.history
  with Stack.Empty ->
    raise (Error (Restore_without_saving "settings"))


let close_path t =
  add_order B.close_path t

let clear_path t =
  add_order B.clear_path t;
  reinit_path_ext t

let path_extents t =
  let t = t.data in
  if t.pxmin > t.pxmax then raise (Error No_current_path)
  else
    {B.x = t.pxmin; y = t.pymin; w = t.pxmax -. t.pxmin; h = t.pymax -. t.pymin}


let stroke_fun t preserve backend =
  B.save backend;
  (*CTM in backend = t.coord * (TM flush) * (CTM before flush).
    To recover correct line width, we want as backend's CTM:
    t.coord * (CTM before flush).
    So we make the following operation:
    CTM := t.coord * (TM flush)^{-1} * (t.coord)^{-1} * CTM.
  *)
  (*
    print_string "stroke with inv_zooms: ";
    print_float !inv_zoomx;
    print_string " ";
    print_float !inv_zoomy;
    flush stdout;*)
  let debug = false in
  let print s name matrix =
    if debug then
      let sf = string_of_float in
      Printf.printf " %s -> %s : %s %s %s %s %s %s\n%!" s name
        (sf matrix.B.xx) (sf matrix.B.xy) (sf matrix.B.x0)
        (sf matrix.B.yx) (sf matrix.B.yy) (sf matrix.B.y0)
    (*else
      (ignore matrix.B.xx; ignore matrix.B.xy; ignore matrix.B.x0;
       ignore matrix.B.yx; ignore matrix.B.yy; ignore matrix.B.y0)*)
  in
  let coord_kill_transform = Coord.invert t.styles.coord in
  print "" "CKT" coord_kill_transform;
  Coord.scale coord_kill_transform !inv_zoomx !inv_zoomy;
  print "scale" "CKT" coord_kill_transform;
  Coord.apply ~next:t.styles.coord coord_kill_transform;
  print "apply coords" "CKT" coord_kill_transform;
  let matrix = B.get_matrix backend in
  print "" "Matrix" coord_kill_transform;
  Coord.apply ~next:coord_kill_transform matrix;
  print "Apply CKT" "Matrix" coord_kill_transform;
  B.set_matrix backend matrix;
  (if preserve then B.stroke_preserve else B.stroke) backend;
  B.restore backend

let stroke t =
  add_order (stroke_fun t false) t;
  reinit_path_ext t

let fill t =
  add_order B.fill t;
  reinit_path_ext t

(*
let clip t =
  add_order B.clip t;
  reinit_path_ext t
*)

let stroke_preserve t =
  add_order (stroke_fun t true) t

let fill_preserve t =
  add_order B.fill_preserve t

(*
let clip_preserve t =
  add_order B.clip_preserve t
*)

let clip_rectangle t ~x ~y ~w ~h =
  add_order (fun t -> B.clip_rectangle t x y w h) t

let stroke_layer t =
  add_order B.stroke t

let stroke_layer_preserve t =
  add_order B.stroke_preserve t


let select_font_face t slant weight family =
  let st = t.styles in
  st.fslant <- slant;
  st.fweight <- weight;
  st.font <- family;
  add_order (fun t -> B.select_font_face t slant weight family) t

let set_font_size t size =
  t.styles.fsize <- size;
  add_order (fun t -> B.set_font_size t size) t

let show_text t ~rotate ~x ~y pos str =
  update t false x y;
  add_order (fun t -> B.show_text t ~rotate ~x ~y pos str) t;
  add_update (TEXT(rotate,x,y,pos,str)) t





let save_layer t =
  Stack.push {t.data with xmin = t.data.xmin} t.saved_data;
  save_stacks t

let restore_layer t =
  try
    t.data <- Stack.pop t.saved_data;
    restore_stacks t
  with Stack.Empty ->
    raise (Error (Restore_without_saving "layer"))


let sign x = if x >= 0. then 1. else -1.

let adjust_scale sc limit =
  let scale = abs_float sc in
  sign sc , (match limit with
    Unlimited -> scale
  | Limited_in l_in -> max l_in scale
  | Limited_out l_out -> min l_out scale
  | Limited(l_in, l_out) -> max l_in (min l_out scale))


let get_ct ?(autoscale=(Uniform Unlimited))
    t ?(pos=B.LT) xmin xmax ymin ymax ~ofsx ~ofsy ~width ~height =
  let c = Coord.identity () in
  let scalx, scaly =
    match autoscale with
      Not_allowed -> 1.,1.
    | Uniform(limit) ->
        let (ex, scalx), (ey, scaly) =
          let scx = width /. (xmax -. xmin)
          and scy = height /. (ymax -. ymin) in
          adjust_scale scx limit, adjust_scale scy limit
        in
        (*We got two scales satisfying the limits. To get the scales
          which will be applied, we proceed like this:
          - Determine the minimal scale: it will be taken without sign.
          - Then we create the scalings by multiplying by (-1) if necessary.
        *)
        let scal = min (min scalx scaly) 1.E15 in
        Coord.scale c (ex *. scal) (ey *. scal);
        scal, scal
    | Free(limitx,limity) ->
        let (ex, scalx), (ey, scaly) =
          let scx = width /. (xmax -. xmin)
          and scy = height /. (ymax -. ymin) in
          adjust_scale scx limitx, adjust_scale scy limity
        in
        Coord.scale c (ex *. (min scalx 1.E15)) (ey *. (min scaly 1.E15));
        (min scalx 1.E15), (min scaly 1.E15)
  in
  let width', height' = abs_float width, abs_float height in
  Printf.printf
    "Box: %f, %f; w:%f h:%f\nLayer ext: %f %f to %f %f\nScales: %f %f\n"
    ofsx ofsy width height
    xmin ymin xmax ymax
    scalx scaly;
  let x =
    if scalx *. (xmax -. xmin) < width' then
      (Printf.printf "X ok.";
      match pos with
      | B.LT | B.LC | B.LB -> ofsx
      | B.CT | B.CC | B.CB ->
          ofsx +. 0.5 *. (sign width) *. (width' -. scalx*.(xmax -. xmin))
      | B.RT | B.RC | B.RB ->
          ofsx +. (sign width) *. (width' -. scalx*.(xmax -. xmin)))
    else ofsx (*Previous case not applicable because of rescalings*)
  and y =
    if scaly *. (ymax -. ymin) < height' then
      (Printf.printf "Y ok.";
      match pos with
      | B.LT | B.CT | B.RT -> ofsy
      | B.LC | B.CC | B.RC ->
          ofsy +. 0.5 *. (sign height) *. (height' -. scaly*.(ymax -. ymin))
      | B.LB | B.CB | B.RB ->
          ofsy +.(sign height) *. (height' -. scaly*.(ymax -. ymin)))
    else ofsy
  in
  Printf.printf "Translation: %f %f\n%!" x y;
  Coord.translate c x y;
  c

let get_coord_transform ?(autoscale=(Uniform Unlimited)) t =
  get_ct ~autoscale t t.data.xmin t.data.xmax t.data.ymin t.data.ymax

let update_text t handle r x y pos str width height autoscale =
  let st = t.styles in
  B.save handle;
  B.select_font_face handle st.fslant st.fweight st.font;
  B.set_font_size handle st.fsize;
  let rect = B.text_extents handle str in
  Printf.printf "Text extents of %s: %f, %f; w:%f; h:%f\n" str
    rect.B.x rect.B.y rect.B.w rect.B.h;
  let scx = width /. (t.data.xmax -. t.data.xmin)
  and scy = height /. (t.data.ymax -. t.data.ymin) in
  let scalx,scaly =
    (*We are interested in how distances are modified; we don't take
      the sign into account.*)
    match autoscale with
      Not_allowed -> 1.,1.
    | Uniform(limit) ->
        let (_, scalx) = adjust_scale scx limit
        and (_, scaly) = adjust_scale scy limit in
        let scal = min (min scalx scaly) 1.E15 in
        scal, scal
    | Free(limitx, limity) ->
        let (_, scalx) = adjust_scale scx limitx
        and (_, scaly) = adjust_scale scy limity in
        (min scalx 1.E15, min scaly 1.E15)
  in
        (*Get distance and points in layer coordinates*)
  (*let x' = (t.xmax -. t.xmin) *. x0
  and y' = (t.ymax -. t.ymin) *. y0 in*)
  let w' =  rect.B.w /. scalx
  and h' =  rect.B.h /. scaly in
  Printf.printf "In layer coords: w:%f; h:%f\n" w' h';
  let xmin = match pos with
    | Backend.CC | Backend.CT | Backend.CB -> x -. w' *. 0.5
    | Backend.RC | Backend.RT | Backend.RB -> x
    | Backend.LC | Backend.LT | Backend.LB -> x -. w'
  and ymin = match pos with
    | Backend.CC | Backend.RC | Backend.LC -> y -. h' *. 0.5
    | Backend.CT | Backend.RT | Backend.LT -> y -. h'
    | Backend.CB | Backend.RB | Backend.LB -> y
  in
  let xmax = xmin +. w' and ymax = ymin +. h' in
  Printf.printf "Bounds applied: %f, %f; %f, %f\n\n" xmin ymin xmax ymax;
  B.set_color handle (Color.make 1. 0. 0.);
  (*FIXME: the following command updates t.[xy][min|max], which we don't want
  in the end.*)
 (* rectangle t xmin ymin (xmax -. xmin) (ymax -. ymin);*)
  stroke_fun t false handle;
  B.restore handle;
  xmin, xmax, ymin, ymax


let make_updates handle t width height autoscale =
  let q = Q.copy t.bkdep_updates in
  let bounds (a,b,c,d) (w,x,y,z) =
    min a w, max b x, min c y, max d z in
  let rec update z =
    if not (Q.is_empty q) then
      match Q.pop q with
        SAVING_POINT -> update z
      | ORDER _ -> failwith "Found an order in bkdep_updates. This is a bug."
      | BKDEP bkdep ->
          match bkdep with
            TEXT(r,x,y,pos,txt) ->
              let p =
                update_text t handle r x y pos txt width height autoscale in
              let w,x,y,z' = bounds z p in
              (*  Printf.printf "Update min/max: %f %f to %f %f" w y x z';
                  Printf.printf "t.min/max: %f %f to %f %f\n"
                  t.data.xmin t.data.ymin t.data.xmax t.data.ymax;*)
              update (bounds z p)
    else z
  in
  let data = t.data in
  update (data.xmin,data.xmax,data.ymin,data.ymax)


let layer_extents ?(autoscale= Uniform Unlimited) ?handle t =
  let data = t.data in
  match handle with
    None ->
      {B.x = data.xmin; y = data.ymin;
       w = data.xmax -. data.xmin; h = data.ymax -. data.ymin}
  | Some handle ->
      let x,x',y,y' = make_updates handle t
        (B.width handle) (B.height handle) autoscale in
      {B.x = x; y = y; w = x' -. x; h = y' -. y}

(*FIXME: a flushed layer can be reusable; flush does not kill nor
  modify the previous orders.*)
let flush_backend ?(autoscale=(Uniform Unlimited))
    t ~ofsx ~ofsy ~width ~height ?pos handle =
  B.save handle;
  initial_bk_matrix := B.get_matrix handle;
  (*Printf.printf "Min/max1: %f %f to %f %f\n"
    t.data.xmin t.data.ymin t.data.xmax t.data.ymax;*)
  let xmin,xmax,ymin,ymax = make_updates handle t width height autoscale in
  (*Printf.printf "Min/max2: %f %f to %f %f\n"
    t.data.xmin t.data.ymin t.data.xmax t.data.ymax;
  Printf.printf "Min/max computed: %f %f to %f %f\n" xmin ymin xmax ymax;*)

  assert (t.data.xmin >= xmin);
  assert (t.data.xmax <= xmax);
  assert (t.data.ymin >= ymin);
  assert (t.data.ymax <= ymax);
  let matrix = B.get_matrix handle in
  let next =
    get_ct ~autoscale t ?pos xmin xmax ymin ymax ~ofsx ~ofsy ~width ~height in
  Coord.apply ~next matrix;
  let sf x = " "^(string_of_float x) in
  (*Printf.printf "Matrix%s%s%s%s%s%s%!" (sf matrix.B.xx) (sf matrix.B.xy)
    (sf matrix.B.x0) (sf matrix.B.yx) (sf matrix.B.yy) (sf matrix.B.y0);*)
  B.set_matrix handle matrix;
  B.translate handle (-.xmin) (-.ymin);
  inv_zoomx := 1. /. next.B.xx;
  inv_zoomy := 1. /. next.B.yy;
  let rec make_orders q =
    if not (Q.is_empty q) then
      match Q.pop q with
        SAVING_POINT -> make_orders q
      | BKDEP _ -> failwith "Found a bkdep in the orders. This is a bug."
      | ORDER f -> f handle;
          make_orders q
  in
  make_orders (Q.copy t.orders);

  B.set_color handle (Color.make ~a:0.5 0. 1. 1.);
  B.rectangle handle t.data.xmin t.data.ymin
    (t.data.xmax -. t.data.xmin) (t.data.ymax -. t.data.ymin);
  stroke_fun t false handle;

  B.set_color handle (Color.make ~a:0.5 1. 0. 0.);
  B.rectangle handle xmin ymin (xmax -. xmin) (ymax -. ymin);
  stroke_fun t false handle;
  B.restore handle

let flush ?(autoscale=(Uniform Unlimited)) t ~ofsx ~ofsy ~width ~height ?pos
    handle =
  let matrix = T.get_matrix handle in
  let handle = T.get_handle handle in
  B.save handle;
  let init_transform = B.get_matrix handle in
  Coord.apply ~next:matrix init_transform;
  B.set_matrix handle init_transform;
  (*Backend handle has now the same transformation coordinates as the
    initial handle applied to it.*)
  flush_backend ~autoscale t ~ofsx ~ofsy ~width ~height ?pos handle;
  B.restore handle

(*Local Variables:*)
(*compile-command: "ocamlc -c -for-pack Archimedes layer.ml && ocamlopt -c -for-pack Archimedes layer.ml"*)
(*End:*)
