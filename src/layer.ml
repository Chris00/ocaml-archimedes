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
      mutable fweight:B.weight;}

type bkdep =
    TEXT of float * float * float * B.text_position * string

type t =
    {
      mutable current: (float * float) option;
      mutable styles:styles;
      history: styles Stack.t;

      mutable pxmin:float; mutable pxmax:float;
      mutable pymin:float; mutable pymax:float;
      mutable xmin: float; mutable xmax: float;
      mutable ymin: float; mutable ymax: float;
      mutable upmargin: float;
      mutable downmargin: float;
      mutable leftmargin: float;
      mutable rightmargin: float;
      orders: (B.t -> unit) Q.t;
      bkdep_updates: bkdep Q.t}

type error =
    No_current_point
  | Restore_without_saving

exception Error of error

(*These variables are used when flushing, to remember the
  scalings. This permits to make a reverse transformation, to pass the
  'good' data to the backend used.*)

let inv_zoomx = ref 1. (*Inverse of scaling factor on X axis*)
let inv_zoomy = ref 1. (*Inverse of scaling factor on Y axis*)

(*IDEA: optional argument "clipping in this rectangle"?*)

let make () =
  let styles =
    {
      coord = Coord.identity ();
      dash = [||], 0.; lj = B.JOIN_BEVEL; lc = B.BUTT;
      color = Color.make 0. 0. 0.; lw = 1.;
      font="Sans serif"; fsize=10.;
      fslant = B.Upright; fweight = B.Normal;
    }
  in
    {
      current = None; styles = styles;  history = Stack.create ();
      pxmin = max_float; pxmax = -. max_float;
      pymin = max_float; pymax = -. max_float;
      xmin = max_float; xmax = -. max_float;
      ymin = max_float; ymax = -. max_float;
      upmargin = 0.; downmargin = 0.;
      leftmargin = 0.; rightmargin = 0.;
      orders = Q.create (); bkdep_updates = Q.create ()
    }

let update t path x y =
  if x < t.xmin then t.xmin <- x;
  if x > t.xmax then t.xmax <- x;
  if y < t.ymin then t.ymin <- y;
  if y > t.ymax then t.ymax <- y;
  if path then(
    if x < t.pxmin then t.pxmin <- x;
    if x > t.pxmax then t.pxmax <- x;
    if y < t.pymin then t.pymin <- y;
    if y > t.pymax then t.pymax <- y)

let reinit_path_ext t =
  t.pxmin <- max_float; t.pxmax <- -. max_float;
  t.pymin <- max_float; t.pymax <- -. max_float;
  t.current <- None

let translate t ~x ~y =
  Q.add (fun t -> B.translate t x y) t.orders;
  Coord.translate t.styles.coord x y

let scale t ~x ~y =
  Q.add (fun t -> B.scale t x y) t.orders;
  Coord.scale t.styles.coord x y

let rotate t ~angle =
  Q.add (fun t -> B.rotate t angle) t.orders;
  Coord.rotate t.styles.coord angle


let transform t =  Coord.transform t.styles.coord
let transform_dist t = Coord.transform_dist t.styles.coord
let invert t = Coord.invert t.styles.coord
let inv_transform t =  Coord.inv_transform t.styles.coord
let inv_transform_dist t = Coord.inv_transform_dist t.styles.coord
let apply ~next t =
  (*Q.add (fun t -> B.apply ~next t) t.orders;*)
  Coord.apply ~next t.styles.coord

let get_coord t = t.styles.coord
let reset_to_id t =
  (*Q.add (fun t -> B.reset_to_id t) t.orders;*)
  Coord.reset_to_id t.styles.coord

(*
let set_pointstyle t s =
  Q.add (fun t -> B.set_pointstyle t s) t.orders;
  t.ps <- s

let set_pattern t s =
  Q.add (fun t -> B.set_pattern t s) t.orders;
  t.pat <- s
*)

let set_color t c =
  Q.add (fun t -> B.set_color t c) t.orders;
  t.styles.color <- c

let set_line_width t w =
  Q.add
    (fun t -> B.set_line_width t w)
    t.orders;
  t.styles.lw <- w

let set_line_cap t lc =
  Q.add (fun t -> B.set_line_cap t lc) t.orders;
  t.styles.lc <- lc

let set_dash t ofs array =
  Q.add (fun t -> B.set_dash t ofs array) t.orders;
  t.styles.dash <- array, ofs

let set_line_join t s =
  Q.add (fun t -> B.set_line_join t s) t.orders;
  t.styles.lj <- s

let set_matrix t matrix =
  Q.add (fun t -> B.set_matrix t matrix) t.orders;
  t.styles.coord <- matrix

(*let get_pointstyle t = t.ps
let get_pattern t = t.pat*)
let get_line_width t = t.styles.lw
let get_line_cap t = t.styles.lc
let get_dash t = t.styles.dash
let get_line_join t = t.styles.lj
let get_matrix t = t.styles.coord

let move_to t ~x ~y =
  update t true x y;
  t.current <- Some (x,y);
  Q.add (fun t -> B.move_to t x y) t.orders

let line t ?x ?y x1 y1 =
  (match x,y with
     Some x, Some y -> move_to t x y
   | _, _ -> ());
  update t true x1 y1;
  Q.add (fun t -> B.line_to t x1 y1) t.orders;
  t.current <- Some(x1,y1)

let line_to t ~x ~y = line t x y

let get_point t =
  match t.current with
    Some(x,y) -> x,y
  | None -> raise (Error No_current_point)

let rel_move_to t ~x ~y =
  let x',y' = get_point t in
  let x',y' = (x+.x'), (y+.y') in
  update t true x' y';
  t.current <- Some (x',y');
  Q.add (fun t -> B.rel_move_to t x y) t.orders

let rel_line_to t ~x ~y =
  let x',y' = get_point t in
  let x',y' = (x+.x'), (y+.y') in
  update t true x' y';
  t.current <- Some(x',y');
  Q.add (fun t -> B.rel_line_to t x y) t.orders




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
  Q.add (fun t -> B.curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3) t.orders;
  t.current <- Some(x3,y3)

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
  t.current <- Some(x3, y3);
  Q.add (fun t -> B.curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3) t.orders

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
  t.current <- Some(x2,y2);
  Q.add (fun t -> B.ellipse_arc t ?x ?y ~a ~b t1 t2) t.orders

let ellipse t ?x ?y ~a ~b =
  let u,v =
    match x,y with
      Some x, Some y ->  x,y
    | _, _ -> get_point t
  in
  update t true (u+.a) (v+.b);
  update t true (u-.a) (v-.b);
  Q.add (fun t -> B.ellipse t ?x ?y ~a ~b) t.orders;
  t.current <- Some((u+.a),v)

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
  t.current <- Some(x2,y2);
  Q.add (fun t -> B.circle_arc t ?x ?y ~r t1 t2) t.orders

let circle t ?x ?y ~r =
  let u,v =
    match x,y with
      Some x, Some y ->  x,y
    | _, _ -> get_point t
  in
  update t true (u+.r) (v+.r);
  update t true (u-.r) (v-.r);
  t.current <- Some(u+.r,v);
  Q.add (fun t -> B.circle t ?x ?y ~r) t.orders
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
  t.current <- Some(x2,y2);
  Q.add (fun t -> B.arc t x y r a1 a2) t.orders


let rectangle t ~x ~y ~w ~h =
  update t true x y;
  update t true (x+.w) (y+.h);
  Q.add (fun t -> B.rectangle t ~x ~y ~w ~h) t.orders


let save t =
  Q.add B.save t.orders;
  Stack.push { t.styles with coord = t.styles.coord} t.history

let restore t =
  Q.add B.restore t.orders;
  try
    t.styles <- Stack.pop t.history
  with Stack.Empty ->
    raise (Error Restore_without_saving)


let close_path t =
  Q.add B.close_path t.orders

let clear_path t =
  Q.add B.clear_path t.orders;
  reinit_path_ext t

let path_extents t =
  if t.pxmin > t.pxmax then failwith "path_extents"
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
  Q.add (stroke_fun t false) t.orders;
  reinit_path_ext t

let fill t =
  Q.add B.fill t.orders;
  reinit_path_ext t

(*
let clip t =
  Q.add B.clip t.orders;
  reinit_path_ext t
*)

let stroke_preserve t =
  Q.add (stroke_fun t true) t.orders

let fill_preserve t =
  Q.add B.fill_preserve t.orders

(*
let clip_preserve t =
  Q.add B.clip_preserve t.orders
*)

let clip_rectangle t ~x ~y ~w ~h =
  Q.add (fun t -> B.clip_rectangle t x y w h) t.orders

let stroke_layer t =
  Q.add B.stroke t.orders

let stroke_layer_preserve t =
  Q.add B.stroke_preserve t.orders


let select_font_face t slant weight family =
  let st = t.styles in
  st.fslant <- slant;
  st.fweight <- weight;
  st.font <- family;
  Q.add (fun t -> B.select_font_face t slant weight family) t.orders

let set_font_size t size =
  t.styles.fsize <- size;
  Q.add (fun t -> B.set_font_size t size) t.orders

let show_text t ~rotate ~x ~y pos str =
  update t false x y;
  Q.add (fun t -> B.show_text t ~rotate ~x ~y pos str) t.orders;
  Q.add (TEXT(rotate,x,y,pos,str)) t.bkdep_updates

let layer_extents t =
  {B.x = t.xmin; y = t.ymin; w = (t.xmax -. t.xmin); h = (t.ymax -. t.ymin)}

let sign x = if x >= 0. then 1. else -1.

let adjust_scale sc limit =
  let scale = abs_float sc in
  sign sc , (match limit with
    Unlimited -> scale
  | Limited_in l_in -> max l_in scale
  | Limited_out l_out -> min l_out scale
  | Limited(l_in, l_out) -> max l_in (min l_out scale))


let get_ct ?(autoscale=(Uniform Unlimited))
    t xmin xmax ymin ymax ~ofsx ~ofsy ~width ~height =
    let c = Coord.identity () in
    (match autoscale with
      Not_allowed -> ()
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
        Coord.scale c (ex *. scal) (ey *. scal)
    | Free(limitx,limity) ->
        let (ex, scalx), (ey, scaly) =
          let scx = width /. (xmax -. xmin)
          and scy = height /. (ymax -. ymin) in
          adjust_scale scx limitx, adjust_scale scy limity
        in
        Coord.scale c (ex *. (min scalx 1.E15)) (ey *. (min scaly 1.E15)));
    Coord.translate c ofsx ofsy;
    c

let get_coord_transform ?(autoscale=(Uniform Unlimited)) t =
  get_ct ~autoscale t t.xmin t.xmax t.ymin t.ymax

let update_text t handle r x y pos str width height =
  let st = t.styles in
  B.save handle;
  B.select_font_face handle st.fslant st.fweight st.font;
  B.set_font_size handle st.fsize;
  let rect = B.text_extents handle str in
  Printf.printf "Text extents of %s: %f, %f; w:%f; h:%f\n" str
    rect.B.x rect.B.y rect.B.w rect.B.h;
  (*Get ratios of width/height *)
  (*let x0 = rect.B.x /. width
  and y0 =  rect.B.y /. height in*)
  let w = rect.B.w /. width
  and h = rect.B.h /. height in
  move_to t x y; (*to initialize, if necessary*)
  (*Get distance and points in layer coordinates*)
  (*let x' = (t.xmax -. t.xmin) *. x0
  and y' = (t.ymax -. t.ymin) *. y0 in*)
  let w' = (t.xmax -. t.xmin) *. w
  and h' = (t.ymax -. t.ymin) *. h in
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
  rectangle t xmin ymin (xmax -. xmin) (ymax -. ymin);
  stroke_fun t false handle;
  B.restore handle;
  xmin, xmax, ymin, ymax


let make_updates handle t width height =
  let q = Q.copy t.bkdep_updates in
  let bounds (a,b,c,d) (w,x,y,z) =
    min a w, max b x, min c y, max d z in
  let rec update z =
    if not (Q.is_empty q) then
      match Q.pop q with
        TEXT(r,x,y,pos,txt) ->
          let p = update_text t handle r x y pos txt width height in
          let w,x,y,z' = bounds z p in
          Printf.printf "Update min/max: %f %f to %f %f" w y x z';
          update (bounds z p)
    else z
  in update (t.xmin,t.xmax,t.ymin,t.ymax)

(*FIXME: a flushed layer can be reusable; flush does not kill nor
  modify the previous orders.*)
let flush_backend ?(autoscale=(Uniform Unlimited))
    t ~ofsx ~ofsy ~width ~height handle =
  B.save handle;
  Printf.printf "Min/max1: %f %f to %f %f\n" t.xmin t.ymin t.xmax t.ymax;
  let xmin,xmax,ymin,ymax = make_updates handle t width height in
  Printf.printf "Min/max2: %f %f to %f %f\n" t.xmin t.ymin t.xmax t.ymax;
  Printf.printf "Min/max computed: %f %f to %f %f\n" xmin ymin xmax ymax;

  assert (t.xmin >= xmin);
  assert (t.xmax <= xmax);
  assert (t.ymin >= ymin);
  assert (t.ymax <= ymax);
  let matrix = B.get_matrix handle in
  let next =
    get_ct ~autoscale t xmin xmax ymin ymax ~ofsx ~ofsy ~width ~height in
  Coord.apply ~next matrix;
  let sf x = " "^(string_of_float x) in
  (*Printf.printf "Matrix%s%s%s%s%s%s%!" (sf matrix.B.xx) (sf matrix.B.xy)
    (sf matrix.B.x0) (sf matrix.B.yx) (sf matrix.B.yy) (sf matrix.B.y0);*)
  B.set_matrix handle matrix;
  B.translate handle (-.xmin) (-.ymin);
  inv_zoomx := 1. /. next.B.xx;
  inv_zoomy := 1. /. next.B.yy;
  let rec make_orders s q =
    if not (Q.is_empty q) then
      (Q.pop q handle;
       (*Printf.printf "%s%!" s; *)
       make_orders s q)
  in
  make_orders "" (Q.copy t.orders);

  B.set_color handle (Color.make ~a:0.5 0. 1. 1.);
  B.rectangle handle t.xmin t.ymin (t.xmax -. t.xmin) (t.ymax -. t.ymin);
  stroke_fun t false handle;

  B.set_color handle (Color.make ~a:0.5 1. 0. 0.);
  B.rectangle handle xmin ymin (xmax -. xmin) (ymax -. ymin);
  stroke_fun t false handle;
  B.restore handle

let flush ?(autoscale=(Uniform Unlimited)) t ~ofsx ~ofsy ~width ~height handle =
  let matrix = T.get_matrix handle in
  let handle = T.get_handle handle in
  B.save handle;
  let init_transform = B.get_matrix handle in
  Coord.apply ~next:matrix init_transform;
  B.set_matrix handle init_transform;
  (*Backend handle has now the same transformation coordinates as the
    initial handle applied to it.*)
  flush_backend ~autoscale t ~ofsx ~ofsy ~width ~height handle;
  B.restore handle

(*Local Variables:*)
(*compile-command: "ocamlc -c -for-pack Archimedes layer.ml && ocamlopt -c -for-pack Archimedes layer.ml"*)
(*End:*)
