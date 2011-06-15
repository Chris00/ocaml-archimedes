module Sizes:
sig
  type t
  val make_root: float -> float -> float -> float -> t
  val child: t -> float -> float -> float -> t
  val get_lw: t -> float
  val get_ts: t -> float
  val get_marks: t -> float
  val set_rel_lw: t -> float -> unit
  val set_rel_ts: t -> float -> unit
  val set_rel_marks: t -> float -> unit
  val set_abs_lw: t -> float -> unit
  val set_abs_ts: t -> float -> unit
  val set_abs_marks: t -> float -> unit
end
  =
struct
  type u =
      ABSOLUTE of float
    | REL_NOT_UPDATED of float
    | REL_UPDATED of float * float

  type t =
      {parent: t;
       mutable lw: u; mutable ts: u; mutable marks:u;
       mutable children: t list}

  (*A node is 'up to date' (concerning a style) if "style".global is of the form [Some _].

    INVARIANT(S):

    - if a node is up to date, then so is its parent; conversely, if a
    node is not up to date, then so are all its children.

    - if a node is up_to_date, then "style" is ABSOLUTE(x) or
    REL_UPDATED(h,x); in this latter case, we have that:

    *the parent of the current node is also ABSOLUTE(y) or REL_UPDATED(_,y);
    *and x = h *. y.
  *)

  let make_root init lines text marks =
    let rec real_root =
      {parent = real_root;
       lw = ABSOLUTE init;
       ts = ABSOLUTE init;
       marks = ABSOLUTE 1.;
       (*Note: to make a mark we place ourselves in normalized
         coords. There is thus no need to premultiply.*)
       children = []}
    in
    (*real_root is not accessible to the user, so there's no risk to
      change its variables. All the following code just mustn't modify
      parent's data of a given node.*)
    let root =
      {parent = real_root;
       lw = REL_UPDATED (lines, init *. lines);
       ts = REL_UPDATED (text, init *. text);
       marks = REL_UPDATED (marks, marks);
       children = []}
    in
    real_root.children <- [root];
    root

  let add_child parent child =
    parent.children <- child :: parent.children

  let prod x = function
      ABSOLUTE(y)
    | REL_UPDATED(_,y) -> REL_UPDATED (x, x*.y)
    | REL_NOT_UPDATED _ -> REL_NOT_UPDATED x

  let child parent lines text marks =
    let child =
      {parent = parent;
       lw = prod lines parent.lw;
       ts = prod text parent.ts;
       marks = prod marks parent.marks;
       children = []}
    in
    add_child parent child;
    child

  let rec update_lw node =
    match node.lw with
      ABSOLUTE(_)
    | REL_UPDATED(_,_) -> ()
    | REL_NOT_UPDATED w ->
        let parent = node.parent in
        update_lw parent;(*ensures invariant 1 ok.*)
        node.lw <- prod w parent.lw
          (*ensures invariant 2*)

  let rec update_ts node =
    match node.ts with
      ABSOLUTE(_)
    | REL_UPDATED(_,_) -> ()
    | REL_NOT_UPDATED s ->
        let parent = node.parent in
        update_ts parent;(*ensures invariant 1 ok.*)
        node.ts <- prod s parent.ts
          (*ensures invariant 2*)

  let rec update_marks node =
    match node.marks with
      ABSOLUTE(_)
    | REL_UPDATED(_,_) -> ()
    | REL_NOT_UPDATED m ->
        let parent = node.parent in
        update_marks parent;(*ensures invariant 1 ok.*)
        node.marks <- prod m parent.marks
          (*ensures invariant 2*)

  let rec set_none_lw node =
    let need_iter =
      match node.lw with
        REL_UPDATED(this,_) ->
          node.lw <- REL_NOT_UPDATED this;
          true
      | ABSOLUTE _ -> true
      | REL_NOT_UPDATED _ -> false
    in
    (*Now un_update all children if needed => invariant 1.*)
    if need_iter then List.iter set_none_lw node.children

  let rec set_none_ts node =
    let need_iter =
      match node.ts with
        REL_UPDATED(this,_) ->
          node.ts <- REL_NOT_UPDATED this;
          true
      | ABSOLUTE _ -> true
      | REL_NOT_UPDATED _ -> false
    in
    (*Now un_update all children if needed => invariant 1.*)
    if need_iter then List.iter set_none_ts node.children

  let rec set_none_marks node =
    let need_iter =
      match node.marks with
        REL_UPDATED(this,_) ->
          node.marks <- REL_NOT_UPDATED this;
          true
      | ABSOLUTE _ -> true
      | REL_NOT_UPDATED _ -> false
    in
    (*Now un_update all children if needed => invariant 1.*)
    if need_iter then List.iter set_none_marks node.children

  let get_lw node =
    update_lw node;
    match node.lw with
      REL_NOT_UPDATED _ -> failwith "get_lw: internal error"
    | ABSOLUTE x
    | REL_UPDATED(_,x) -> x

  let get_ts node =
    update_ts node;
    match node.ts with
      REL_NOT_UPDATED _ -> failwith "get_ts: internal error"
    | ABSOLUTE x
    | REL_UPDATED(_,x) -> x

  let get_marks node =
    update_marks node;
    match node.marks with
      REL_NOT_UPDATED _ -> failwith "get_marks: internal error"
    | ABSOLUTE x
    | REL_UPDATED(_,x) -> x

  (*Note: the Failures have a priori no way to happen, due to
    updates. If that is the case, then there is a bug.*)

  let set_rel_lw node size =
    node.lw <- REL_NOT_UPDATED size;
    set_none_lw node

  let set_rel_ts node size =
    node.ts <- REL_NOT_UPDATED size;
    set_none_ts node

  let set_rel_marks node size =
    node.marks <- REL_NOT_UPDATED size;
    set_none_marks node

  let set_abs_lw node size =
    node.lw <- ABSOLUTE size;
    set_none_lw node

  let set_abs_ts node size =
    node.ts <- ABSOLUTE size;
    set_none_ts node

  let set_abs_marks node size =
    node.marks <- ABSOLUTE size;
    set_none_marks node
end

exception No_current_point

let is_nan_or_inf (x:float) = x <> x || 1. /. x = 0.

(*Viewport data*)
type viewport =
    {
      (*A viewport is always associated to a handle.*)
      handle : t;
      (*For text, line width, marks size*)
      scalings: Sizes.t;
      (*To update the ranges correctly, we need the current point.*)
      mutable current_pt: (float * float) option;
      mutable axes: viewport -> unit;
      (*What has to be plotted in this viewport. Mutable so that if we
        want to do the orders but preserve the queue, we replace it by a
        (created and updated) copy.*)
      mutable vp_orders: (viewport -> unit) Queue.t;
      mutable graph_orders: (viewport -> unit) Queue.t;
      (*For saving and restoring states*)
      scalings_hist : (float * float * float) Stack.t;
      vp_device: Coordinate.t;
      (* Transformation from the viewport (seen as [0,1]²) to the device. *)
      graph_box: Coordinate.t;
      (* Tranformation defining the area reserved for the graph (seen
         as [0,1]²) in terms of the [vp_device] coordinates. *)
      data_coord: Coordinate.t;
      (* Coordinates used to plot the data points (depend on the
         [graph_box] ones) *)
      mutable axes_fitting:Axes.margins;
      graph_device: Coordinate.t;
      (*Flag indicating if this viewport has been stored in the handle.*)
      mutable stored : bool;
      (* Automatic or manual ranges settings.  *)
      mutable xmin : float;     mutable xmin_auto: bool;
      mutable xmax: float;      mutable xmax_auto: bool;
      mutable ymin : float;     mutable ymin_auto: bool;
      mutable ymax: float;      mutable ymax_auto: bool;
    }
(*Handle to work with.*)
and t =
    {backend: Backend.t;
     rawcoords: Coordinate.t;
     (*Raw coordinates: those which initially affect the backend*)
     normalized: Coordinate.t;
     (*Normalized coords: the biggest square included in the surface,
       whose upper left corner coincide with the upper left corner of
       the surface, is the unit square in these coordinates.*)
     square_side: float;
     (*The quantity used to define normalized by scaling. This is
       needed for text size handling.*)
     initial_vp: viewport;(*Initial viewport -- the one which is
                            associated with the coordinates "all the
                            surface is the unit square" *)
     mutable vp : viewport;(*Current viewport*)
     mutable used_vp: viewport Queue.t;
     (*Viewports that have been used. We can ensure that only one copy
       of each used viewport will be in this queue -- because of the
       viewport flag "used".

       Note: mutability of this field is needed for efficiency reasons
       -- immediate drawing needs to do the orders, but preserve them;
       to avoid recopying the orders, we simply replace them by a
       (created and updated) copy.*)
     mutable immediate_drawing:bool;
     (*Says whether we draw immediately or wait for closing.*)
     mutable only_immediate : bool;
       (*Says whether an order has only to be made immediately, or
         must be stored. This field is not modifiable by users.*)
     mutable only_extents: bool;
     (*Says whether we draw or only compute the extents to update the
       coordinate systems. This field is not modifiable by users.*)
    }

(*Default line width, text size, mark size: if the height is less than
  width, then height is by default equal to:

  * 500 superposed lines, or
  * 50 lines of text, or
  * 100 boxes of 1x1 marks.
*)
let def_lw, def_ts, def_marks = 0.002, 0.024, 0.01

(*User transformations. To get the previous defaults, the user will
  enter resp. 1, 12, 1. (12 is determined experimentally using cairo:
  a 500 x 500 pixels output can contain 50 lines of text, putting font
  size as 12.) *)
let usr_lw, usr_ts, usr_marks = 500., 500., 100.

(*Initial scale of the user_to_viewport coordinates.*)
let initial_scale = 1.

let backend h = h.backend

let make ~dirs name w h =
  let backend = Backend.make ~dirs name w h in
  let init = Coordinate.make_root (Backend.get_matrix backend) in
  let vp_device = Coordinate.make_scale init w h in
  (* The dependencies of coordinate systems matter! *)
  let graph_box = Coordinate.make_identity vp_device in
  let data_coord =
    Coordinate.make_scale graph_box initial_scale initial_scale in
  let square_side = min w h in
  let norm = Coordinate.make_scale init square_side square_side in
  let rec initial_vp =
    {handle = handle;
     scalings = Sizes.make_root square_side def_lw def_ts def_marks;
     axes = (fun _ -> ());
     current_pt = None;
     vp_orders = Queue.create ();
     graph_orders = Queue.create ();
     scalings_hist = Stack.create ();
     vp_device = vp_device;
     graph_box = graph_box;
     data_coord = data_coord;
     axes_fitting = {Axes.left = 0.;right = 0.;top = 0.;bottom=0.};
     graph_device = Coordinate.copy data_coord;
     stored = true;(*will be... in the end of [make].*)
     xmin = nan;    xmin_auto = true;
     xmax = nan;    xmax_auto = true;
     ymin = nan;    ymin_auto = true;
     ymax = nan;    ymax_auto = true;
    }
  and handle =
    {backend=backend;
     rawcoords = init;
     normalized = norm;
     square_side = square_side;
     initial_vp = initial_vp;
     vp = initial_vp;
     used_vp = Queue.create ();
     immediate_drawing = true;
     only_immediate = false;
     only_extents = false;
    }
  in
  (*The initial viewport is in use, so must be in the [used_vp].*)
  Queue.push initial_vp handle.used_vp;
  handle

let check t =
  (*A handle is closed iff there's no stored viewports (initialization
    contains the initial viewport).*)
  if Queue.is_empty t.used_vp then
    failwith "Archimedes.Handle: closed"

let do_viewport_orders ?orders_queue vp backend =
  let rec make_orders vporders orders =
    if not (Queue.is_empty orders) then (
      let order = Queue.pop orders in
      order vp;
      (match orders_queue with
       | None -> ()
       | Some q -> Queue.push order q);
      make_orders vporders orders)
    else match orders_queue with
    | None -> ()
    | Some q ->
        if vporders then vp.vp_orders <- q
        else vp.graph_orders <- q
  in
  let ctm = Coordinate.use backend vp.data_coord in
  (* Axes drawing (if necessary, possibly does nothing) *)
  vp.axes vp;
  make_orders true vp.vp_orders;
  Coordinate.restore backend ctm;
  let ctm = Coordinate.use backend vp.graph_device in
  make_orders false vp.graph_orders;
  Coordinate.restore backend ctm

let do_orders t preserve =
  let vp_storing_queue = Queue.create () in
  let rec do_viewports_orders () =
    if not (Queue.is_empty t.used_vp) then (
      let vp = Queue.pop t.used_vp in
      let orders_queue =
        if preserve then Some (Queue.create ())
        else None
      in
      do_viewport_orders ?orders_queue vp t.backend;
      if preserve then Queue.push vp vp_storing_queue;
      do_viewports_orders ())
    else
      (*Note: if preserve then vp_storing_queue contains all viewports; if
        not, it is a (fresh) empty queue. In any cases, we can do the replacement.*)
      t.used_vp <- vp_storing_queue;
  in do_viewports_orders ()

let close t =
  do_orders t false;
  Backend.close t.backend

let immediate t b =
  if b then do_orders t true;
  t.immediate_drawing <- b

let xrange t xmin xmax =
  let vp = t.vp in
  vp.xmin <- xmin;    vp.xmin_auto <- is_nan_or_inf xmin;
  vp.xmax <- xmax;    vp.xmax_auto <- is_nan_or_inf xmax

let yrange t ymin ymax=
  let vp = t.vp in
  vp.ymin <- ymin;    vp.ymin_auto <- is_nan_or_inf ymin;
  vp.ymax <- ymax;    vp.ymax_auto <- is_nan_or_inf ymax

let xmin t = t.vp.xmin
let xmax t = t.vp.xmax
let ymin t = t.vp.ymin
let ymax t = t.vp.ymax

(*Easy update Axes.ranges options and coordinates*)
let update_coords t x y =
  let vp = t.vp in
  let updated = ref false in
  (* Update viewport values *)
  if vp.xmin_auto then
    if is_nan_or_inf vp.xmin || x < vp.xmin then
      (vp.xmin <- x; updated := true);
  if vp.xmax_auto then
    if is_nan_or_inf vp.xmax || x > vp.xmax then
      (vp.xmax <- x; updated := true);
  if vp.ymin_auto then
    if is_nan_or_inf vp.ymin || y < vp.ymin then
      (vp.ymin <- y; updated := true);
  if vp.ymax_auto then
    if is_nan_or_inf vp.ymax || y > vp.ymax then
      (vp.ymax <- y; updated := true);
  if !updated then (
    let scalx, tr_x =
      if vp.xmin = vp.xmax then
        initial_scale, -. vp.xmin -. 0.5 *. initial_scale
      else 1. /. (vp.xmax -. vp.xmin), -. vp.xmin
    and scaly, tr_y =
      if vp.ymin = vp.ymax then
        initial_scale, -. vp.ymin -. 0.5 *. initial_scale
      else 1. /. (vp.ymax -. vp.ymin), -. vp.ymin
    in
    let data_to_graph = Matrix.make_scale scalx scaly in
    Matrix.translate data_to_graph tr_x tr_y;
    Coordinate.transform vp.data_coord data_to_graph;

    (*In case of immediate drawing, needs to replot everything
      for this viewport.*)
    if t.immediate_drawing then (
      (*Deletes the drawing by covering.*)
      (*FIXME: how to manage with transparent backgrounds?*)
      let ctm = Coordinate.use t.backend t.vp.vp_device in
      Backend.save t.backend;
      (*FIXME: viewport's background?*)
      Backend.set_color t.backend Color.white;
      Backend.rectangle t.backend 0. 0. 1. 1.;
      Backend.fill t.backend;
      Backend.restore t.backend;
      Coordinate.restore t.backend ctm;
      (*Now, replot in the new coordinates.*)
      do_viewport_orders t.vp t.backend
    ))



module Viewport =
struct
  let inner_make handle vp_device =
    let graph_box = Coordinate.make_identity vp_device in
    let data_coord =
      Coordinate.make_scale graph_box initial_scale initial_scale in
    { handle = handle;
      (*Line width and text and mark sizes are preserved, but are new ones,
        depending on the previous ones.*)
      scalings = Sizes.child handle.vp.scalings 1. 1. 1.;
      axes = (fun _ -> ());
      current_pt = None;
      vp_orders = Queue.create ();
      graph_orders = Queue.create ();
      (*Not in use*)
      scalings_hist = Stack.create();
      vp_device = vp_device;
      graph_box = graph_box;
      data_coord = data_coord;
      axes_fitting = {Axes.left = 0.;right = 0.; top = 0.; bottom = 0.};
      graph_device = Coordinate.copy data_coord;
      stored = false;
      xmin = nan;   xmin_auto = true;
      xmax = nan;   xmax_auto = true;
      ymin = nan;   ymin_auto = true;
      ymax = nan;   ymax_auto = true;
    }

  let sub_rect vp ~x ~y ~w ~h =
    let ndrawings = Coordinate.make_translate vp.vp_device x y in
    Coordinate.scale ndrawings w h;
    inner_make vp.handle ndrawings

  let make_rect handle =
    sub_rect handle.vp

  let sub vp ~xmin ~xmax ~ymin ~ymax =
    sub_rect vp xmin ymin (xmax -. xmin) (ymax -.ymin)

  let make handle =
    sub handle.vp

  let use vp =
    (*Need to change the current viewport and coordinates of the
      context, but also flag the viewport, if it is not, and add it to
      the [used_vp] field of the context.*)
    let handle = vp.handle in
    handle.vp <- vp;
    ignore (Coordinate.use handle.backend vp.data_coord);
    if not vp.stored then (
      vp.stored <- true;
      Queue.push vp handle.used_vp
    )

  (*{2 Convenience functions to create viewports}*)
  let rows handle n =
    let step = 1./.(float n) in
    let f i = make_rect handle 0. ((float i)*. step)  1. step in
    Array.init n f

  let columns handle n =
    let step = 1./.(float n) in
    let f i = make_rect handle ((float i)*. step) 0. step 1. in
    Array.init n f

  let grid handle n m =
    let stepx = 1. /. (float n)
    and stepy = 1. /. (float m) in
    let f i j = make_rect handle
      ((float i)*. stepx) ((float j) *. stepy) stepx stepy in
    let make_row i = Array.init m (f i) in
    Array.init n make_row

  let sub_rows vp n =
    let step = 1. /. (float n) in
    let f i = sub_rect vp 0. ((float i) *. step) 1. step in
    Array.init n f

  let sub_columns vp n =
    let step = 1. /. (float n) in
    let f i = sub_rect vp ((float i) *. step) 0. step 1. in
    Array.init n f

  let sub_matrix vp n m =
    let stepx = 1. /. (float n)
    and stepy = 1. /. (float m) in
    let f i j = sub_rect vp
      ((float i) *. stepx) ((float j) *. stepy) stepx stepy in
    let make_row i = Array.init m (f i) in
    Array.init n make_row
end

(* Data that depends directly on viewports
 **********************************************************************)
let use = Viewport.use

let use_initial t = t.vp <- t.initial_vp

(*Local settings -- in a viewport*)
let set_line_width t w =
  Sizes.set_abs_lw t.vp.scalings
    (if w <= 0. then def_lw *. t.square_side else w /. usr_lw*. t.square_side)

let set_mark_size t m =
  Sizes.set_abs_marks t.vp.scalings
    (if m <= 0. then def_marks *. t.square_side else m /. usr_marks*. t.square_side)
let set_font_size t s =
  Sizes.set_abs_ts t.vp.scalings
    (if s <= 0. then def_ts *. t.square_side else s /. usr_ts*. t.square_side)

let set_rel_line_width t w =
  Sizes.set_rel_lw t.vp.scalings (if w <= 0. then 1. else w)
let set_rel_mark_size t m =
  Sizes.set_rel_marks t.vp.scalings (if m <= 0. then 1. else m)
let set_rel_font_size t s =
  Sizes.set_rel_ts t.vp.scalings (if s <= 0. then 1. else s)


(*Global settings -- affect all viewports*)
let set_global_line_width t w =
  Sizes.set_abs_lw t.initial_vp.scalings
    (if w <= 0. then def_lw else w /. usr_lw *. t.square_side)
let set_global_mark_size t m =
  Sizes.set_abs_marks t.initial_vp.scalings
    (if m <= 0. then def_marks else m /. usr_marks)
    (* NOTE: Remind that marks scales need not be premultiplied. *)
let set_global_font_size t s =
  Sizes.set_abs_ts t.initial_vp.scalings
    (if s <= 0. then def_ts else s /. usr_ts  *. t.square_side)

(*Getters*)
let get_line_width t = (Sizes.get_lw t.vp.scalings) *. usr_lw/.t.square_side
let get_mark_size t = (Sizes.get_marks t.vp.scalings) *. usr_marks
let get_font_size t = (Sizes.get_ts t.vp.scalings) *. usr_ts/.t.square_side



let from_backend f t = f t.backend

(* Backend primitives (not overriden by viewport system)
 **********************************************************************)
let add_order ?graph f t =
  if not t.only_extents then (
    if t.only_immediate then f t.vp
    else (
      let queue =
        match graph with
          Some true -> t.vp.graph_orders
        | _ -> t.vp.vp_orders
      in
      Queue.add f queue;
      if t.immediate_drawing then f t.vp
    )
  )

let get_current_pt t = match t.vp.current_pt with
    None -> raise No_current_point
  | Some (x,y) -> x,y

let set_current_pt t x y =
  if not t.only_extents then t.vp.current_pt <- Some (x,y)


(*FIXME: needed?*)
let width t = Backend.width t.backend
let height t = Backend.height t.backend
let set_color t c =
  add_order (fun _ -> Backend.set_color t.backend c) t;
  Backend.set_color t.backend c

let set_line_cap t lc =
  add_order (fun _ -> Backend.set_line_cap t.backend lc) t;
  Backend.set_line_cap t.backend lc

let set_dash t x y =
  add_order (fun _ -> Backend.set_dash t.backend x y) t;
  Backend.set_dash t.backend x y

let set_line_join t join=
  add_order (fun _ -> Backend.set_line_join t.backend join) t;
  Backend.set_line_join t.backend join

let get_line_cap t = Backend.get_line_cap t.backend
let get_dash t = Backend.get_dash t.backend
let get_line_join t = Backend.get_line_join t.backend

let move_to t ~x ~y =
  set_current_pt t x y;
  update_coords t x y;
  add_order (fun _ -> Backend.move_to t.backend x y) t

let line_to t ~x ~y =
  set_current_pt t x y;
  update_coords t x y;
  add_order (fun _ -> Backend.line_to t.backend x y) t

let rel_move_to t ~x ~y =
  let x', y' = get_current_pt t in
  update_coords t (x +. x') (y +. y');
  set_current_pt t (x +. x') (y +. y');
  add_order (fun _ -> Backend.rel_move_to t.backend x y) t

let rel_line_to t ~x ~y =
  let x', y' = get_current_pt t in
  update_coords t (x +. x') (y +. y');
  set_current_pt t (x +. x') (y +. y');
  add_order (fun _ -> Backend.rel_line_to t.backend x y) t

let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  update_coords t x1 y1;
  update_coords t x2 y2;
  update_coords t x3 y3;
  set_current_pt t x3 y3;
  add_order (fun _ -> Backend.curve_to t.backend x1 y1 x2 y2 x3 y3) t

let rectangle t ~x ~y ~w ~h =
  update_coords t x y;
  update_coords t (x+.w) (y+.h);
  add_order (fun _ -> Backend.rectangle t.backend x y w h) t

let arc t ~r ~a1 ~a2 =
  (*FIXME: better bounds for the arc can be found.*)
  let x, y = get_current_pt t in
  let x' = x -. r *. cos a1
  and y' = y -. r *. sin a1 in
  update_coords t (x'+.r) (y'+.r);
  update_coords t (x'-.r) (y'-.r);
  add_order (fun _ -> Backend.arc t.backend r a1 a2) t

let close_path t =
 add_order (fun _ -> Backend.close_path t.backend) t

let clear_path t =
 add_order (fun _ -> Backend.clear_path t.backend) t
(*let path_extents t = Backend.path_extents t.backend*)

(*Stroke when using current coordinates.*)
let stroke_current t =
  add_order (fun _ -> Backend.stroke t.backend) t
let stroke_current_preserve t =
  add_order (fun _ -> Backend.stroke_preserve t.backend) t


let stroke t =
  let lw = Sizes.get_lw t.vp.scalings in
  let f _ =
    let ctm = Coordinate.use t.backend t.normalized in
    Backend.set_line_width t.backend lw;
    Backend.stroke t.backend;
    Coordinate.restore t.backend ctm
  in
  add_order f t

let stroke_preserve t =
  let lw = Sizes.get_lw t.vp.scalings in
  let f _ =
    let ctm = Coordinate.use t.backend t.normalized in
    Backend.set_line_width t.backend lw;
    Backend.stroke_preserve t.backend;
    Coordinate.restore t.backend ctm
  in
  add_order f t

let fill t =
  add_order (fun _ -> Backend.fill t.backend) t
let fill_preserve t =
  add_order (fun _ -> Backend.fill_preserve t.backend) t

let clip_rectangle t ~x ~y ~w ~h =
  add_order (fun _ -> Backend.clip_rectangle t.backend x y w h) t

let save_vp t =
  let f vp =
    Backend.save t.backend;
    let sizes =
      Sizes.get_lw vp.scalings,
      Sizes.get_ts vp.scalings,
      Sizes.get_marks vp.scalings
    in
    Stack.push sizes vp.scalings_hist
  in
  add_order f t

let restore_vp t =
  let f vp =
    try
      let lw, ts, marks = Stack.pop vp.scalings_hist in
      Sizes.set_abs_lw vp.scalings lw;
      Sizes.set_abs_ts vp.scalings ts;
      Sizes.set_abs_marks vp.scalings marks;
      Backend.restore t.backend
    with Stack.Empty -> ()
  in
  add_order f t

let select_font_face t slant weight family =
  add_order (fun _ -> Backend.select_font_face t.backend slant weight family) t

(*FIXME: when [Backend.show_text], the backend temporarily returns to
  raw coordinates -- but converts its input.*)
let show_text t ~rotate ~x ~y pos txt=
  let font_size = Sizes.get_ts t.vp.scalings in
  let f _ =
    Backend.set_font_size t.backend font_size;
    Backend.show_text t.backend ~rotate ~x ~y pos txt;
  in
  let ctm = Coordinate.use t.backend t.normalized in
  Backend.set_font_size t.backend font_size;
  let rect = Backend.text_extents t.backend txt in
  Coordinate.restore t.backend ctm;
  update_coords t rect.Matrix.x rect.Matrix.y;
  update_coords t (rect.Matrix.x +. rect.Matrix.w)
    (rect.Matrix.y +. rect.Matrix.h);
  add_order f t

let text_extents t txt =
  let font_size = Sizes.get_ts t.vp.scalings in
  let ctm = Coordinate.use t.backend t.normalized in
  Backend.set_font_size t.backend font_size;
  let rect = Backend.text_extents t.backend txt in
  Coordinate.restore t.backend ctm;
  rect

let render t name =
  let marks = Sizes.get_marks t.vp.scalings in
  let f vp =
    let ctm = Coordinate.use t.backend t.normalized in
    Backend.scale t.backend marks marks;
    Pointstyle.render name t.backend;
    Coordinate.restore t.backend ctm;
  in
  (* FIXME: extents are expressed in "marks-normalized" coords. We need
     to have it in user coords in order to determine the extents. *)
  (* let extents = Pointstyle.extents name in
     let marks' = marks *. t.square_side in
     let x',y' = get_current_pt t in
     Printf.printf "initial marks: %f %f %f %f %f" marks t.square_side marks' x' y';
     let axpmw x w = x +. w *. marks' in
     update_coords t (axpmw x' extents.Matrix.x) (axpmw y' extents.Matrix.y);
     update_coords t (axpmw x' (extents.Matrix.x +. extents.Matrix.w))
     (axpmw y' (extents.Matrix.y +.extents.Matrix.h));*)
  add_order f t

(*
let render_extents t name =
  let ctm = Coordinate.use t.backend t.normalized in
  let marks = Sizes.get_marks t.coords.scalings in
  Backend.scale t.backend marks marks;
  let rect = Pointstyle.render_extents name t.backend in
  Coordinate.restore t.backend ctm;
  (*Now express [rect] in device coords*)
  (*let x', y' = Coordinate.to_device marks rect.x rect.y in
  let w', h' =
    Coordinate.to_device_distance marks rect.w rect.h
  in
  {x = x'; y = y'; w = w'; h = h'}*)
  rect*)
(*
let mark_extents t name =
  let rect = Pointstyle.extents name in
  let marks = Sizes.get_marks t.vp.scalings in
  {x = rect.x *. marks *. t.square_side; y = rect.y *. marks *. t.square_side;
   w = rect.w *. marks *. t.square_side; h = rect.h *. marks *. t.square_side}
*)


let f_line_to t (x,y) = line_to t x y
let f_finish = stroke

let xyf t ?color ?nsamples ?min_step
    ?(do_with = f_line_to) ?(finish = f_finish) f a b =
  let _, ranges , fct = Functions.samplefxy f ?nsamples ?min_step b a in
  update_coords t ranges.Axes.xmin ranges.Axes.ymin;
  update_coords t ranges.Axes.xmax ranges.Axes.ymax;
  let f vp =
    Backend.save t.backend;
    ignore(Coordinate.use t.backend vp.data_coord);
    (match color with
       Some color -> Backend.set_color t.backend color
     | None -> ());
    t.only_immediate <- true;
    fct (fun () -> do_with t) ();
    finish t;
    t.only_immediate <- false;
    Backend.restore t.backend;
  in
  t.only_extents <- true;
  fct (fun () -> do_with t) ();
  finish t;
  t.only_extents <- false;
  add_order f t

let xy_mark mark t x y =
  move_to t x y;
  render t mark

let xy_finish t = ()

let xy t ?color ?axes ?(mark = "X") ?(do_with = (xy_mark mark)) iter =
  let rec iterate do_with =
    match Iterator.next iter with
      None -> xy_finish t
    | Some(x,y) ->
        do_with t x y;
        iterate do_with
  in
  let f vp =
    Backend.save t.backend;
    ignore(Coordinate.use t.backend vp.data_coord);
    (match color with
       Some color -> Backend.set_color t.backend color
     | None -> ());
    t.only_immediate <- true;
    Iterator.reset iter;
    iterate do_with;
    t.only_immediate <- false;
    Backend.restore t.backend;
  in
  let extents = Iterator.extents iter in
(* Note: graph coordinates have been already determined... by ranges. *)
  update_coords t extents.Axes.xmin extents.Axes.ymin;
  update_coords t extents.Axes.xmax extents.Axes.ymax;
  t.only_extents <- true;
  iterate do_with;
  t.only_extents <- false;
  add_order f t

let make_xaxis = Axes.make_xaxis
let make_yaxis = Axes.make_yaxis

let direct_axes t ?color type_axes ?type_axes_printer ?axes_meeting ?print_tic
    xaxis yaxis ranges =
  let print_axes = type_axes_printer in
  let scalings = t.vp.scalings in
  let font_size = Sizes.get_ts scalings in
  let lines = Sizes.get_lw scalings
  and marks = Sizes.get_marks scalings in
  let ctm = Coordinate.use t.backend t.vp.vp_device in
  let margins =
    Axes.axes t.backend ~normalization:t.normalized ~lines ~marks:def_marks ~font_size
      ?color type_axes ?print_axes ?axes_meeting ?print_tic xaxis yaxis ranges
  in
  Coordinate.restore t.backend ctm;
  t.vp.axes_fitting <- margins;
  (* With the aid of margins and ranges, compute the graph_device
     coordinate transformation. *)
  let matrix = Matrix.make_translate margins.Axes.left margins.Axes.bottom in
  Matrix.scale matrix
    (1. -. margins.Axes.left -. margins.Axes.right)
    (1. -. margins.Axes.top -. margins.Axes.bottom);
  (* Matrix now translates the graph zone as square [0,1]^2. Now go to user coords. *)
  let diffx =
    let x = ranges.Axes.x2 -. ranges.Axes.x1 in
    if x = 0. then 1. else x
  and diffy =
    let y = ranges.Axes.y2 -. ranges.Axes.y1 in
    if y = 0. then 1. else y
  in
  Matrix.scale matrix (1. /. diffx) (1. /. diffy);
  Matrix.translate matrix (-.ranges.Axes.x1) (-.ranges.Axes.y1);
  Coordinate.transform t.vp.graph_device matrix

let axes t ?(color = Color.black) ?type_axes_printer ?axes_meeting
    ?print_tic axes =
  let print_axes =
    match type_axes_printer with
      None -> Axes.print_axes
    | Some f -> fun axes ranges _ -> f axes ranges t
  and print_tic =
    match print_tic with
      None -> Axes.print_tic
    | Some f -> fun _ -> f t
  in
  let draw_axes vp =
    let scalings = vp.scalings in
    let font_size = Sizes.get_ts scalings in
    let lines = Sizes.get_lw scalings
    and marks = Sizes.get_marks scalings in
    let ctm = Coordinate.use t.backend vp.vp_device in
    let ranges = { Axes.x1 = vp.xmin; x2 = vp.xmax;
                   y1 = vp.ymin; y2 = vp.ymax } in
    let xmargin, ymargin =
      Axes.get_margins axes ?axes_meeting ~normalization:t.normalized
        ~lines ~marks:def_marks ~font_size ranges t.backend  in
    Coordinate.restore t.backend ctm;
    let xx1 = xmargin.Axes.left
    and xx2 = xmargin.Axes.right
    and xy1 = xmargin.Axes.bottom
    and xy2 = xmargin.Axes.top
    and yx1 = ymargin.Axes.left
    and yx2 = ymargin.Axes.right
    and yy1 = ymargin.Axes.bottom
    and yy2 = ymargin.Axes.top
    in
    let left = max xx1 yx1
    and right = max xx2 yx2
    and top = max xy2 yy2
    and bottom = max xy1 yy1 in
    let margin_x = left +. right
    and margin_y = top +. bottom in
    let margins_ok = margin_x < 1. && margin_y < 1. in
    if margins_ok then (
      Coordinate.transform vp.graph_box
        { Matrix.xx = 1. -. right -. left;  xy = 0.;  yx = 0.;
          Matrix.yy = 1. -. top -. bottom;
          x0 = left; y0 = bottom };
      (* t.only_immediate <- true; *)
      (* Coordinate.use t.backend vp.graph_box; *)
      (* rectangle t 0. 0. 1. 1.; *)
      (* set_color t (Color.rgba 0. 0. 1. 0.2); *)
      (* fill t; *)
      (* t.only_immediate <- false; *)
      if t.immediate_drawing then (
        (*Deletes the drawing by covering.*)
        (*FIXME: how to manage with transparent backgrounds?*)
        let ctm = Coordinate.use t.backend t.vp.vp_device in
        Backend.save t.backend;
        (*FIXME: viewport's background?*)
        Backend.set_color t.backend Color.white;
        Backend.rectangle t.backend 0. 0. 1. 1.;
        Backend.fill t.backend;
        Backend.restore t.backend;
        Coordinate.restore t.backend ctm);
      Backend.save t.backend;
      ignore(Coordinate.use t.backend vp.data_coord);
      Backend.set_color t.backend color;
      Axes.print axes ~normalization:t.normalized
        ~lines ~marks:def_marks ~font_size ~ranges
        ~print_axes ?axes_meeting ~print_tic t.backend;
      Backend.restore t.backend
    )
    else  (* Labels take too big margins to plot correctly => no scaling.*)
      failwith "Archimedes.Handle.axes: the tics labels are too large."
  in
  t.vp.axes <- draw_axes

let current_vp t = t.vp
