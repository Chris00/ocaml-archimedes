open Archimedes
module B = Backend
module Q = Queue
module Coord = B.Coordinate

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
      mutable color: Archimedes.Color.t;
      mutable lw: float;
      mutable font:string;
      mutable fsize:int;
      mutable fslant:B.slant;
      mutable fweight:B.weight;}


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
      orders: (B.t -> unit) Q.t}

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
      font="Sans serif"; fsize=10;
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
      orders = Q.create ()
    }

let update t x y =
  if x < t.xmin then t.xmin <- x;
  if x > t.xmax then t.xmax <- x;
  if y < t.ymin then t.ymin <- y;
  if y > t.ymax then t.ymax <- y;
  if x < t.pxmin then t.pxmin <- x;
  if x > t.pxmax then t.pxmax <- x;
  if y < t.pymin then t.pymin <- y;
  if y > t.pymax then t.pymax <- y

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
  update t x y;
  t.current <- Some (x,y);
  Q.add (fun t -> B.move_to t x y) t.orders

let line t ?x ?y x1 y1 =
  (match x,y with
     Some x, Some y -> move_to t x y
   | _, _ -> ());
  update t x1 y1;
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
  update t x' y';
  t.current <- Some (x',y');
  Q.add (fun t -> B.rel_move_to t x y) t.orders

let rel_line_to t ~x ~y =
  let x',y' = get_point t in
  let x',y' = (x+.x'), (y+.y') in
  update t x' y';
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
       update t x y;
       x, y
   | _, _ -> x1, y1
  in
  update t x1 y1; update t x3 y3;
  Q.add (fun t -> B.curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3) t.orders;
  t.current <- Some(x3,y3)

let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 = curve t ~x1 ~y1 ~x2 ~y2 x3 y3

let rel_curve_to t ~x1 ~y1 ?x2 ?y2 ~x3 ~y3 =
  let x,y = get_point t in
  let x1, y1, x3, y3 = x +. x1, y +. y1, x +. x3, y +. y3 in
  update t x1 y1;
  update t x3 y3;
  let x2, y2 = match x2,y2 with
      Some x', Some y' ->
        let x'' = (x+.x') and y'' = (y+.y') in
        update t x'' y'';
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
  update t (u+.a) (v+.b);
  update t (u-.a) (v-.b);
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
  update t (u+.a) (v+.b);
  update t (u-.a) (v-.b);
  Q.add (fun t -> B.ellipse t ?x ?y ~a ~b) t.orders;
  t.current <- Some((u+.a),v)

let circle_arc t ?x ?y ~r t1 t2=
   let u,v =
    match x,y with
      Some x, Some y ->  x,y
    | _, _ -> get_point t
   in
   (*FIXME: cf. ellipse_arc*)
  update t (u+.r) (v+.r);
  update t (u-.r) (v-.r);
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
  update t (u+.r) (v+.r);
  update t (u-.r) (v-.r);
  t.current <- Some(u+.r,v);
  Q.add (fun t -> B.circle t ?x ?y ~r) t.orders
*)

let arc t ~x ~y ~r ~a1 ~a2=
  (*FIXME: cf. ellipse_arc*)
  update t (x+.r) (y+.r);
  update t (x-.r) (y-.r);
  let ct1 = cos a1
  and ct2 = cos a2
  and st1 = sin a1
  and st2 = sin a2 in
  let x1,y1 = x +. r *. ct1, y +. r *. st1
  and x2,y2 = x +. r *. ct2, y +. r *. st2 in
  t.current <- Some(x2,y2);
  Q.add (fun t -> B.arc t x y r a1 a2) t.orders


let rectangle t ~x ~y ~w ~h =
  update t x y;
  update t (x+.w) (y+.h);
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
    let coord_kill_transform = Coord.invert t.styles.coord in
    Coord.scale coord_kill_transform !inv_zoomx !inv_zoomy;
    Coord.apply ~next_t:t.styles.coord coord_kill_transform;
    Coord.apply ~next_t:coord_kill_transform (B.get_matrix backend);*)
  print_string "stroke: ";
  print_float !inv_zoomx;
  print_string " ";
  print_float !inv_zoomy;
  flush stdout;
  (*B.scale backend !inv_zoomx !inv_zoomy;*)
  let coord_kill_transform = Coord.invert t.styles.coord in
  Coord.scale coord_kill_transform !inv_zoomx !inv_zoomy;
  Coord.apply ~next:t.styles.coord coord_kill_transform;
  let matrix = B.get_matrix backend in
  Coord.apply ~next:coord_kill_transform matrix;
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
  Q.add (fun t -> B.select_font_face t slant weight family) t.orders

let set_font_size t size =
  Q.add (fun t -> B.set_font_size t size) t.orders

let show_text t ~rotate ~x ~y pos str =
  Q.add (fun t -> B.show_text t ~rotate ~x ~y pos str) t.orders


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


let get_coord_transform ?(autoscale=(Uniform Unlimited))
    t ~ofsx ~ofsy ~width ~height=
    let c = Coord.identity () in
    Coord.translate c ofsx ofsy;
    match autoscale with
      Not_allowed -> c
    | Uniform(limit) ->
        let (ex, scalx), (ey, scaly) =
          let scx = width /. (t.xmax -. t.xmin)
          and scy = height /. (t.ymax -. t.ymin) in
          adjust_scale scx limit, adjust_scale scy limit
        in
        (*We got two scales satisfying the limits. To get the scales
          which will be applied, we proceed like this:
          - Determine the minimal scale: it will be taken without sign.
          - Then we create the scalings by multiplying by (-1) if necessary.
        *)
        let scal = min (min scalx scaly) 1.E15 in
        Coord.scale c (ex *. scal) (ey *. scal);
        c
    | Free(limitx,limity) ->
        let (ex, scalx), (ey, scaly) =
          let scx = width /. (t.xmax -. t.xmin)
          and scy = height /. (t.ymax -. t.ymin) in
          adjust_scale scx limitx, adjust_scale scy limity
        in
        Coord.scale c (ex *. (min scalx 1.E15)) (ey *. (min scaly 1.E15)); c

(*FIXME: a flushed layer can be reusable; flush does not kill nor
  modify the previous orders.*)
let flush_backend ?(autoscale=(Uniform Unlimited))
    t ~ofsx ~ofsy ~width ~height handle =
  let q = Q.copy t.orders in
  let rec make_orders () =
    if not (Q.is_empty q) then
      (Q.pop q handle;
       make_orders ())
  in
  B.save handle;
  let matrix = B.get_matrix handle in
  let next = get_coord_transform ~autoscale t ~ofsx ~ofsy ~width ~height in
  Coord.apply ~next matrix;
  inv_zoomx := 1. /. next.B.xx;
  inv_zoomy := 1. /. next.B.yy;
  B.set_matrix handle matrix;
  B.translate handle (-.t.xmin) (-.t.ymin);
  make_orders ();
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


(*
let make_axes t ?color_axes ?color_labels datax datay mode =
  (match color_axes with
     Some c -> save t; set_color t c
   | None -> ());
  (*Axes*)
  let ofsx, ofsy, ticx, ticy =
    match mode with
      Axes.Rectangle(tx,ty) ->
        rectangle t ~x:t.xmin ~y:t.ymin (t.xmax -. t.xmin) (t.ymax -. t.ymin);
        t.xmin, t.ymin, tx, ty
    | Axes.Two_lines(x,y,tx,ty) ->
        (*Need to update -before- so that mins/maxs are correctly
          initialized for making the axes lines.*)
        update t x y;
        move_to t t.xmin y;
        line_to t t.xmax y;
        move_to t x t.ymin;
        line_to t x t.ymax;
        x,y, tx, ty
  in
  (*Tics or like*)
  (*We don't want the mins/maxs be modified during placing text,
    stroking and so on -> storing the current values. Done now because of
    previous axes.*)
  let xmin = t.xmin and ymin = t.ymin in
  let xmax = t.xmax and ymax = t.ymax in

  let make_data data ticmode x_axis =
    match data with
      Axes.Graph(major,minor) ->
        let step =
          let diff = if x_axis then xmax -. xmin else ymax -. ymin in
          diff /. (float major) in
        let ministep = step /. (float minor) in
        for i = 0 to major do (*major tics in X axis*)
          let ofs = (float i) *. step in
          let x, y =
            if x_axis then xmin +. ofs, ofsy
            else ofsx, ymin +. ofs
          in
          (*Tic to put, centered in (x, y), with label 'x' or 'y' as
            given by x_axis.*)
          Axes.tic true x y x_axis ticmode;
          if i < major then
            for j = 1 to minor - 1 do
              let x,y =
                if x_axis then x +. (float j) *. ministep, y
                else x, y +. (float j) *. ministep
              in
              Axes.tic false x y x_axis ticmode
            done
        done
    | Axes.Tics(minor,num_minors) ->
        let rec make_tic x y x_axis ticmode n =
          if not ((x_axis && x > xmax) || (not x_axis && y > ymax)) then
            let xnew, ynew =
              if x_axis then
                x+.minor, y
              else x, y+.minor
            in
            (Axes.tic (n=num_minors) x y x_axis ticmode;
             make_tic xnew ynew ((n mod num_minors)+1))

        in make_tic xmin ymin true ticx 0;
        make_tic xmin ymin false ticy 0
  in
  (*Make data for X axis*)
  make_data datax ticx true;
  (*Make data for Y axis*)
  make_data datay ticy false;
  stroke t;
  (match color_axes with
     Some c -> restore t
   | None -> ())
*)

(*Local Variables:*)
(*compile-command: "ocamlc -c layer.ml && ocamlopt -c layer.ml"*)
(*End:*)
