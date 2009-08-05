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
      orders: (B.t -> unit) Q.t;
      bkdep_updates: bkdep Q.t}
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
    }


type error =
    No_current_point
  | Restore_without_saving

exception Error of error

(*These variables are used when flushing, to remember the
  scalings. This permits to make a reverse transformation, to pass the
  'good' data to the backend used. They are used in particular to
  perform the default strokes -- those whose line width is expressed
  in backend coordinates, whereas the path is expressed in layer
  coordinates.*)

let inv_zoomx = ref 1. (*Inverse of scaling factor on X axis*)
let inv_zoomy = ref 1. (*Inverse of scaling factor on Y axis*)

let make () =
  let styles =
    {
      coord = Coord.identity ();
      dash = [||], 0.; lj = B.JOIN_BEVEL; lc = B.BUTT;
      color = Color.make 0. 0. 0.; lw = 1.;
      font="Sans serif"; fsize=10.;
      fslant = B.Upright; fweight = B.Normal;
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
      orders = Q.create (); bkdep_updates = Q.create ()
    }
  in
  {
    styles = styles;  history = Stack.create ();
    data = data;  saved_data = Stack.create ();
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

let translate t ~x ~y =
  Q.add (fun t -> B.translate t x y) t.data.orders;
  Coord.translate t.styles.coord x y

let scale t ~x ~y =
  Q.add (fun t -> B.scale t x y) t.data.orders;
  Coord.scale t.styles.coord x y

let rotate t ~angle =
  Q.add (fun t -> B.rotate t angle) t.data.orders;
  Coord.rotate t.styles.coord angle


let transform t =  Coord.transform t.styles.coord
let transform_dist t = Coord.transform_dist t.styles.coord
let invert t = Coord.invert t.styles.coord
let inv_transform t =  Coord.inv_transform t.styles.coord
let inv_transform_dist t = Coord.inv_transform_dist t.styles.coord
let apply ~next t =
  (*Q.add (fun t -> B.apply ~next t) t.data.orders;*)
  Coord.apply ~next t.styles.coord

let get_coord t = t.styles.coord
let reset_to_id t =
  (*Q.add (fun t -> B.reset_to_id t) t.data.orders;*)
  Coord.reset_to_id t.styles.coord

(*
let set_pointstyle t s =
  Q.add (fun t -> B.set_pointstyle t s) t.data.orders;
  t.ps <- s

let set_pattern t s =
  Q.add (fun t -> B.set_pattern t s) t.data.orders;
  t.pat <- s
*)

let set_color t c =
  Q.add (fun t -> B.set_color t c) t.data.orders;
  t.styles.color <- c

let set_line_width t w =
  Q.add
    (fun t -> B.set_line_width t w)
    t.data.orders;
  t.styles.lw <- w

let set_line_cap t lc =
  Q.add (fun t -> B.set_line_cap t lc) t.data.orders;
  t.styles.lc <- lc

let set_dash t ofs array =
  Q.add (fun t -> B.set_dash t ofs array) t.data.orders;
  t.styles.dash <- array, ofs

let set_line_join t s =
  Q.add (fun t -> B.set_line_join t s) t.data.orders;
  t.styles.lj <- s

let set_matrix t matrix =
  Q.add (fun t -> B.set_matrix t matrix) t.data.orders;
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
  t.data.current <- Some (x,y);
  Q.add (fun t -> B.move_to t x y) t.data.orders

let line t ?x ?y x1 y1 =
  (match x,y with
     Some x, Some y -> move_to t x y
   | _, _ -> ());
  update t true x1 y1;
  Q.add (fun t -> B.line_to t x1 y1) t.data.orders;
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
  Q.add (fun t -> B.rel_move_to t x y) t.data.orders

let rel_line_to t ~x ~y =
  let x',y' = get_point t in
  let x',y' = (x+.x'), (y+.y') in
  update t true x' y';
  t.data.current <- Some(x',y');
  Q.add (fun t -> B.rel_line_to t x y) t.data.orders




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
  Q.add (fun t -> B.curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3) t.data.orders;
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
  Q.add (fun t -> B.curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3) t.data.orders

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
  Q.add (fun t -> B.ellipse_arc t ?x ?y ~a ~b t1 t2) t.data.orders

let ellipse t ?x ?y ~a ~b =
  let u,v =
    match x,y with
      Some x, Some y ->  x,y
    | _, _ -> get_point t
  in
  update t true (u+.a) (v+.b);
  update t true (u-.a) (v-.b);
  Q.add (fun t -> B.ellipse t ?x ?y ~a ~b) t.data.orders;
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
  Q.add (fun t -> B.circle_arc t ?x ?y ~r t1 t2) t.data.orders

let circle t ?x ?y ~r =
  let u,v =
    match x,y with
      Some x, Some y ->  x,y
    | _, _ -> get_point t
  in
  update t true (u+.r) (v+.r);
  update t true (u-.r) (v-.r);
  t.data.current <- Some(u+.r,v);
  Q.add (fun t -> B.circle t ?x ?y ~r) t.data.orders
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
  Q.add (fun t -> B.arc t x y r a1 a2) t.data.orders


let rectangle t ~x ~y ~w ~h =
  update t true x y;
  update t true (x+.w) (y+.h);
  Q.add (fun t -> B.rectangle t ~x ~y ~w ~h) t.data.orders


let save t =
  Q.add B.save t.data.orders;
  Stack.push { t.styles with coord = t.styles.coord} t.history

let restore t =
  Q.add B.restore t.data.orders;
  try
    t.styles <- Stack.pop t.history
  with Stack.Empty ->
    raise (Error Restore_without_saving)


let close_path t =
  Q.add B.close_path t.data.orders

let clear_path t =
  Q.add B.clear_path t.data.orders;
  reinit_path_ext t

let path_extents t =
  let t = t.data in
  if t.pxmin > t.pxmax then failwith "path_extents: no current path"
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
  Q.add (stroke_fun t false) t.data.orders;
  reinit_path_ext t

let fill t =
  Q.add B.fill t.data.orders;
  reinit_path_ext t

(*
let clip t =
  Q.add B.clip t.data.orders;
  reinit_path_ext t
*)

let stroke_preserve t =
  Q.add (stroke_fun t true) t.data.orders

let fill_preserve t =
  Q.add B.fill_preserve t.data.orders

(*
let clip_preserve t =
  Q.add B.clip_preserve t.data.orders
*)

let clip_rectangle t ~x ~y ~w ~h =
  Q.add (fun t -> B.clip_rectangle t x y w h) t.data.orders

let stroke_layer t =
  Q.add B.stroke t.data.orders

let stroke_layer_preserve t =
  Q.add B.stroke_preserve t.data.orders


let select_font_face t slant weight family =
  let st = t.styles in
  st.fslant <- slant;
  st.fweight <- weight;
  st.font <- family;
  Q.add (fun t -> B.select_font_face t slant weight family) t.data.orders

let set_font_size t size =
  t.styles.fsize <- size;
  Q.add (fun t -> B.set_font_size t size) t.data.orders

let show_text t ~rotate ~x ~y pos str =
  update t false x y;
  Q.add (fun t -> B.show_text t ~rotate ~x ~y pos str) t.data.orders;
  Q.add (TEXT(rotate,x,y,pos,str)) t.data.bkdep_updates

let layer_extents t =
  let t = t.data in
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
  let data = t.data in
  let q = Q.copy data.bkdep_updates in
  let bounds (a,b,c,d) (w,x,y,z) =
    min a w, max b x, min c y, max d z in
  let rec update z =
    if not (Q.is_empty q) then
      match Q.pop q with
        TEXT(r,x,y,pos,txt) ->
          let p = update_text t handle r x y pos txt width height autoscale in
          let w,x,y,z' = bounds z p in
        (*  Printf.printf "Update min/max: %f %f to %f %f" w y x z';
          Printf.printf "t.min/max: %f %f to %f %f\n"
            t.data.xmin t.data.ymin t.data.xmax t.data.ymax;*)
          update (bounds z p)
    else z
  in update (data.xmin,data.xmax,data.ymin,data.ymax)

(*FIXME: a flushed layer can be reusable; flush does not kill nor
  modify the previous orders.*)
let flush_backend ?(autoscale=(Uniform Unlimited))
    t ~ofsx ~ofsy ~width ~height handle =
  B.save handle;
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
  make_orders "" (Q.copy t.data.orders);

  B.set_color handle (Color.make ~a:0.5 0. 1. 1.);
  B.rectangle handle t.data.xmin t.data.ymin
    (t.data.xmax -. t.data.xmin) (t.data.ymax -. t.data.ymin);
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
