module B = Backend
module Q = Queue
module Coord = Coordinate

type limitation =
    Unlimited
  | Limited_out of float
  | Limited_in of float
  | Limited of float * float

type scaling =
  | Not_allowed
  | Uniform of limitation
  | Free of limitation * limitation

type styles = (float * float array) * B.line_join * B.line_cap
    * float * Coord.t * string * int * B.slant * B.weight


type t =
    {
      mutable current: (float * float) option;
      coord:Coord.t;
      (* mutable ps: B.pointstyle;*)
      mutable dash: float * float array;
      mutable lj: B.line_join;
      mutable lc: B.line_cap;
      (*  mutable pat:B.pattern;(*FIXME: ? Pattern.t;*)*)
      mutable color: Color.t;
      mutable lw: float;
      stack: styles Stack.t;
      mutable font:string;
      mutable fsize:int;
      mutable fslant:B.slant; mutable fweight:B.weight;
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


(*IDEA: optional argument "clipping in this rectangle"?*)

let make () =
    {
      current = None; coord = Coord.identity ();
      dash = 0., [||]; lj = B.JOIN_BEVEL; lc = B.BUTT;
      color = Color.color 0. 0. 0.; lw = 1.;
      stack = Stack.create (); font="Sans serif"; fsize=10;
      fslant = B.Upright; fweight = B.Normal;
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

let translate t x y =
  Q.add (fun t -> B.translate t x y) t.orders;
  Coord.translate t.coord x y

let scale t x y =
  Q.add (fun t -> B.scale t x y) t.orders;
  Coord.scale t.coord x y
(*
let rotate t a =
  Q.add (fun t -> B.rotate t a) t.orders;
  Coord.rotate t.coord a
*)

let transform t =  Coord.transform t.coord
let transform_dist t = Coord.transform_dist t.coord
let invert t = Coord.invert t.coord
let inv_transform t =  Coord.inv_transform t.coord
let inv_transform_dist t = Coord.inv_transform_dist t.coord
let apply ~next t =
  (*Q.add (fun t -> B.apply ~next t) t.orders;*)
  Coord.apply ~next_t:next t.coord

let get_coord t = t.coord
let reset_to_id t =
  (*Q.add (fun t -> B.reset_to_id t) t.orders;*)
  Coord.reset_to_id t.coord

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
  t.color <- c

let set_line_width t w =
  Q.add (fun t -> B.set_line_width t w) t.orders;
  t.lw <- w

let set_line_cap t lc =
  Q.add (fun t -> B.set_line_cap t lc) t.orders;
  t.lc <- lc

let set_dash t s =
  let ofs, array = s in
  Q.add (fun t -> B.set_dash t ofs array) t.orders;
  t.dash <- s

let set_line_join t s =
  Q.add (fun t -> B.set_line_join t s) t.orders;
  t.lj <- s

(*let get_pointstyle t = t.ps
let get_pattern t = t.pat*)
let get_line_width t = t.lw
let get_line_cap t = t.lc
let get_dash t = t.dash
let get_line_join t = t.lj

let move_to t x y =
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

let line_to t x1 y1 = line t x1 y1

let get_point t =
  match t.current with
    Some(x,y) -> x,y
  | None -> raise (Error No_current_point)

let rel_move_to t dx dy =
  let x,y = get_point t in
  let x',y' = (x+.dx), (y+.dy) in
  update t x' y';
  t.current <- Some (x',y');
  Q.add (fun t -> B.rel_move_to t dx dy) t.orders

let rel_line_to t dx dy =
  let x,y = get_point t in
  let x',y' = (x+.dx), (y+.dy) in
  update t x' y';
  t.current <- Some(x',y');
  Q.add (fun t -> B.rel_line_to t dx dy) t.orders




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

let curve_to t ~x1 ~y1 ~x2 ~y2 x3 y3 = curve t ~x1 ~y1 ~x2 ~y2 x3 y3

let rel_curve t ~x1 ~y1 ?x2 ?y2 x3 y3 =
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

let rectangle t ?x ?y width height =
  let x', y' = match x,y with
      Some x', Some y' ->
        update t x' y';
        x',y'
    | _,_ ->  get_point t
  in
  update t (x'+.width) (y'+.height);
  Q.add (fun t -> B.rectangle t ~x:x' ~y:y' ~width ~height) t.orders


let save t =
  Q.add B.save t.orders;
  Stack.push
    (t.dash, t.lj, t.lc, t.lw, t.coord, t.font, t.fsize, t.fslant, t.fweight)
    t.stack

let restore t =
  Q.add B.restore t.orders;
  try
    let dash, lj, lc, lw, coord, fname, fsize, fslant, fweight =
      Stack.pop t.stack
    in
    t.dash <- dash;
    t.lj <- lj;
    t.lc <- lc;
    t.lw <- lw;
    t.font <- fname;
    t.fsize <- fsize;
    t.fslant <- fslant;
    t.fweight <- fweight;
    Coord.reset_to_id t.coord;
    Coord.apply ~next_t:coord t.coord
  with Stack.Empty ->
    raise (Error Restore_without_saving)


let close_path t =
  Q.add B.close_path t.orders


type rectangle = B.rectangle =
    {x:float; y:float; w:float; h:float}

let path_extents t =
  if t.pxmin > t.pxmax then failwith "path_extents"
  else
    {x = t.pxmin; y = t.pymin; w = t.pxmax -. t.pxmin; h = t.pymax -. t.pymin}

let stroke t =
  Q.add B.stroke t.orders;
  reinit_path_ext t

let fill t =
  Q.add B.fill t.orders;
  reinit_path_ext t

let clip t =
  Q.add B.clip t.orders;
  reinit_path_ext t


let stroke_preserve t =
  Q.add B.stroke_preserve t.orders

let fill_preserve t =
  Q.add B.fill_preserve t.orders

let clip_preserve t =
  Q.add B.clip_preserve t.orders


let text t ~size ~x ~y str =
  Q.add (fun t -> B.text t ~size ~x ~y str) t.orders

let adjust_scale scale limit =
  match limit with
    Unlimited -> scale
  | Limited_in l_in -> max l_in scale
  | Limited_out l_out -> min l_out scale
  | Limited(l_in, l_out) -> max l_in (min l_out scale)


(*FIXME: Can a flushed layer be reusable?*)
let flush ?(autoscale=(Uniform Unlimited)) t ~ofsx ~ofsy ~width ~height handle=
  let rec make_orders () =
    if not (Q.is_empty t.orders) then
      (Q.pop t.orders handle;
       make_orders ())
  in
  let c = Coord.identity () in
  B.save handle;
  B.translate handle ofsx ofsy;
  Coord.translate c ofsx ofsy;
  let scalopt =
    match autoscale with
      Not_allowed -> None
    | Uniform(limit) ->
        let scalx, scaly =
          let scx =
            width /. (t.xmax -. t.xmin)
          and scy =
            height /. (t.ymax -. t.ymin)
          in
          adjust_scale scx limit, adjust_scale scy limit
        in
        let scal = min (min scalx scaly) 1.E15 in
        Some(scal, scal)
    | Free(limitx,limity) ->
        let scalx, scaly =
          let scx =
            width /. (t.xmax -. t.xmin)
          and scy =
            height /. (t.ymax -. t.ymin)
          in
          adjust_scale scx limitx, adjust_scale scy limity
        in
        Some((min scalx 1.E15), (min scaly 1.E15));
        (*      let tx, ty = Coord.inv_transform_dist c t.leftmargin t.downmargin in
                B.translate handle tx ty*)
  in
  (match scalopt with
    Some(scalx, scaly) ->
      B.scale handle scalx scaly;
      Coord.scale c scalx scaly;
      (*let tx, ty = Coord.inv_transform_dist c t.leftmargin t.downmargin in
         B.translate handle tx ty;*)
      B.translate handle (-.t.xmin) (-.t.ymin)
  | None -> ());
  make_orders ();
  B.restore handle;

(*Local Variables:*)
(*compile-command: "ocamlc -c layer.ml && ocamlopt -c layer.ml"*)
(*End:*)
