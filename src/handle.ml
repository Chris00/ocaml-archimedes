module Sizes:
sig
  type t
  val make_root: float -> float -> float -> t
  val child: t -> float -> float -> float -> t
  val get_lw: t -> float
  val get_ts: t -> float
  val get_marks: t -> float
  val set_lw: t -> float -> unit
  val set_ts: t -> float -> unit
  val set_marks: t -> float -> unit
end
  =
struct
  type u =
      {mutable this: float;
       mutable global: float option;}

  type t =
      {parent: t;
       lw: u; ts: u; marks:u;
       mutable children: t list}

  (*A node is 'up to date' (concerning a style) if "style".global is of the form [Some _].

    INVARIANT(S):

    - if a node is up to date, then so is its parent; conversely, if a
    node is not up to date, then so are all its children.

    - if a node is up_to_date, then we have the following facts:
    *"style".global = Some x (for some float x);
    *parent."style".global = Some y;
    *The equality [x = this *. y] is true.
  *)

  let make_root lines text marks =
    let make_uniform x =
      {this = x; global = Some x}
    in
    let rec real_root =
      {parent = real_root;
       lw = make_uniform 1.;
       ts = make_uniform 1.;
       marks = make_uniform 1.;
       children = []}
    in
    (*real_root is not accessible to the user, so there's no risk to
      change its variables. All the following code just mustn't modify
      parent's data of a given node.*)
    let root =
      {parent = real_root;
       lw = make_uniform lines;
       ts = make_uniform text;
       marks = make_uniform marks;
       children = []}
    in
    real_root.children <- [root];
    root

  let add_child parent child =
    parent.children <- child :: parent.children

  let prod x = function
      None -> None
    | Some y -> Some (x *. y)

  let child parent lines text marks =
    let child =
      {parent = parent;
       lw = {this = lines; global = prod lines parent.lw.global};
       ts = {this = text; global = prod text parent.ts.global};
       marks = {this = marks; global = prod marks parent.marks.global};
       children = []}
    in
    add_child parent child;
    child

  let rec update_lw node =
    match node.lw.global with
      Some _ -> ()
    | None ->
        let parent = node.parent in
        update_lw parent;(*ensures invariant 1 ok.*)
        node.lw.global <- prod node.lw.this parent.lw.global
          (*ensures invariant 2*)

  let rec update_ts node =
    match node.ts.global with
      Some _ -> ()
    | None ->
        let parent = node.parent in
        update_ts parent;(*ensures invariant 1 ok.*)
        node.ts.global <- prod node.ts.this parent.ts.global
          (*ensures invariant 2*)

  let rec update_marks node =
    match node.marks.global with
      Some _ -> ()
    | None ->
        let parent = node.parent in
        update_marks parent;(*ensures invariant 1 ok.*)
        node.marks.global <- prod node.marks.this parent.marks.global
          (*ensures invariant 2*)

  let rec set_none_lw node =
    node.lw.global <- None;
    (*Now un_update all children => invariant 1.*)
    List.iter set_none_lw node.children

  let rec set_none_ts node =
    node.ts.global <- None;
    (*Now un_update all children => invariant 1.*)
    List.iter set_none_ts node.children

  let rec set_none_marks node =
    node.marks.global <- None;
    (*Now un_update all children => invariant 1.*)
    List.iter set_none_marks node.children

  let get_lw node =
    update_lw node;
    match node.lw.global with
      None -> failwith "get_lw: internal error"
    | Some x -> x

  let get_ts node =
    update_ts node;
    match node.ts.global with
      None -> failwith "get_ts: internal error"
    | Some x -> x

  let get_marks node =
    update_marks node;
    match node.marks.global with
      None -> failwith "get_marks: internal error"
    | Some x -> x

  (*Note: the Failures have a priori no way to happen, due to
    updates. If that is the case, then there is a bug.*)

  let set_lw node size =
    node.lw.this <- size;
    set_none_lw node

  let set_ts node size =
    node.ts.this <- size;
    set_none_ts node

  let set_marks node size =
    node.marks.this <- size;
    set_none_marks node
end

(*Context-independent viewport data*)
type viewport =
    { (*For text, line width, marks size*)
      scalings: Sizes.t;
      (*Bounds*)
      bounds: Backend.xyranges;
      (*What has to be plotted in this viewport*)
      orders: (unit -> unit) Queue.t;
      (*For saving and restoring states*)
      scalings_hist : (float * float * float) Stack.t;
    }

type t =
    {backend: Backend.t;
     rawcoords: Coordinate.t;
     normalized: Coordinate.t;
     initial_coord : Coordinate.t;(*Initial coordinate system --
                                  the one which makes the surface as a unit square*)
     initial_vp: viewport;(*Initial viewport --
                         the one which is associated with the initial coordinates.*)
     mutable current_coord: Coordinate.t;(*Currently used coordinate
                                            transformation.*)
     mutable current_vp : viewport;(*Current viewport*)
     used_vp: viewport Queue.t; (*Viewports that have been used. We
                                  can ensure that only one copy of
                                  each used viewport will be in this
                                  queue -- because of the viewport
                                  flag.*)
    }

(*Default line width, text size, mark size: if the height is less than
  width, then height is by default equal to:

  * 500 superposed lines, or
  * 20 lines of text, or
  * 100 boxes of 1x1 marks.
*)
let def_lw, def_ts, def_marks = 0.002, 0.05, 0.01

(*User transformations. To get the previous defaults, the user will
  enter resp. 1, 10, 1 (which are the "usual" defaults in a drawing). *)
let usr_lw, usr_ts, usr_marks = 500., 200., 100.


let make ~dirs name w h =
  let backend = Backend.make ~dirs name w h in
  let init = Coordinate.make_identity () in
  let coord = Coordinate.make_scale init w h in
  let norm =
    let z = min w h in
    Coordinate.make_scale init z z
  in
  let initial =
    (*On every viewport, the first action to perform is to change the
      backend coordinates, to draw on this viewport.*)
    let orders = Queue.create () in
    (*Here, we're setting transformation to initially draw on [0,1]^2*)
    Queue.push (fun () -> ignore (Coordinate.use backend coord)) orders;
    {scalings = Sizes.make_root def_lw def_ts def_marks;
     (*No drawings yet*)
     bounds =
        {Backend.x1 = max_float; x2 = -.max_float;
         y1 = max_float; y2 = -.max_float;};
     orders = orders;
     scalings_hist = Stack.create ();
    }
  in
  (*The initial viewport is in use, so must be in the [used_vp].*)
  let init_used_vp = Queue.create () in
  Queue.push initial init_used_vp;
  {backend=backend;
   rawcoords = init;
   normalized = norm;
   initial_coord = coord;
   initial_vp = initial;
   current_coord = coord;
   current_vp = initial;
   used_vp = init_used_vp;
  }

let close t =
  let rec make_viewports () =
    if not (Queue.is_empty t.used_vp) then
      let rec make_orders orders =
        if not (Queue.is_empty orders) then
          (Queue.pop orders ();
           make_orders orders)
      in
      let vp = Queue.pop t.used_vp in
      (make_orders vp.orders;
       make_viewports ())
  in make_viewports ();
  Backend.close t.backend

let check t =
  if Queue.is_empty t.used_vp then
    failwith "Archimedes.Handle: closed"

module Viewport =
struct
  type vp =
      {context: t;
       coord: Coordinate.t;
       data:viewport;
       (*Flag indicating if this viewport has been used in an Archimedes context*)
       mutable used : bool;}

  let make arch ~xmin ~xmax ~ymin ~ymax =
    let ndrawings = Coordinate.make_translate arch.current_coord xmin ymin in
    Coordinate.scale ndrawings (xmax -. xmin) (ymax -. ymin);
    let init_orders = Queue.create () in
    Queue.push (fun () -> ignore (Coordinate.use arch.backend ndrawings))
      init_orders;
    let data =
      { (*Line width and text and mark sizes are preserved, but are new ones,
          depending on the previous ones.*)
        scalings = Sizes.child arch.current_vp.scalings 1. 1. 1.;
        (*No drawings yet on this viewport.*)
        bounds =
          {Backend.x1 = max_float; x2 = -.max_float;
           y1 = max_float; y2 = -.max_float;};
        (*A first order: changement of coordinates.*)
        orders = init_orders;
        (*Not in use*)
        scalings_hist = Stack.create();
      }
    in
    { context = arch; coord = ndrawings; data = data;used = false;}

  let sub vp ~xmin ~xmax ~ymin ~ymax =
    let ndrawings = Coordinate.make_translate vp.coord xmin ymin in
    Coordinate.scale ndrawings (xmax -. xmin) (ymax -. ymin);
    let init_orders = Queue.create () in
    Queue.push (fun () -> ignore (Coordinate.use vp.context.backend ndrawings))
      init_orders;
    let data =
      { (*Line width and text and mark sizes are preserved, but are new ones,
          depending on the previous ones.*)
        scalings = Sizes.child vp.data.scalings 1. 1. 1.;
        (*No drawings yet on this viewport.*)
        bounds =
          {Backend.x1 = max_float; x2 = -.max_float;
           y1 = max_float; y2 = -.max_float;};
        (*A first order: changement of coordinates.*)
        orders = init_orders;
        scalings_hist = Stack.create();
      }
    in
    { context = vp.context; coord = ndrawings; data = data; used = false;}

  let use vp =
    (*Need to change the current viewport and coordinates of the
      context, but also flag the viewport, if it is not, and add it to
      the [used_vp] field of the context.*)
    let arch = vp.context in
    arch.current_coord <- vp.coord;
    arch.current_vp <- vp.data;
    if not vp.used then (
      vp.used <- true;
      Queue.push vp.data arch.used_vp)

  (*{2 Convenience functions to create viewports}*)
  let rows arch n =
    let step = 1./.(float n) in
    let f i = make arch 0. 1. ((float i)*. step) ((float (i+1) *. step)) in
    Array.init n f

  let columns arch n =
    let step = 1./.(float n) in
    let f i = make arch ((float i)*. step) ((float (i+1) *. step)) 0. 1. in
    Array.init n f

  let matrix arch n m =
    let stepx = 1./.(float n)
    and stepy = 1./.(float m) in
    let f i j = make arch
      ((float i)*. stepx) ((float (i+1) *. stepx))
      ((float j)*. stepy) ((float (j+1) *. stepy)) in
    let make_row i = Array.init m (f i) in
    Array.init n make_row

  let sub_rows vp n =
    let step = 1./.(float n) in
    let f i = sub vp 0. 1. ((float i)*. step) ((float (i+1) *. step)) in
    Array.init n f

  let sub_columns vp n =
    let step = 1./.(float n) in
    let f i = sub vp ((float i)*. step) ((float (i+1) *. step)) 0. 1. in
    Array.init n f

  let sub_matrix vp n m =
    let stepx = 1./.(float n)
    and stepy = 1./.(float m) in
    let f i j = sub vp
      ((float i)*. stepx) ((float (i+1) *. stepx))
      ((float j)*. stepy) ((float (j+1) *. stepy)) in
    let make_row i = Array.init m (f i) in
    Array.init n make_row
end

(* Data that depends directly on viewports
 **********************************************************************)
let use = Viewport.use

let use_initial t =
  t.current_coord <- t.initial_coord;
  t.current_vp <- t.initial_vp

(*Local settings -- in a viewport*)
let set_line_width t w =
  Sizes.set_lw t.current_vp.scalings
    (if w <= 0. then def_lw else w /. usr_lw)
let set_mark_size t m =
  Sizes.set_marks t.current_vp.scalings
    (if m <= 0. then def_marks else m /. usr_marks)
let set_font_size t s =
  Sizes.set_ts t.current_vp.scalings
    (if s <= 0. then def_ts else s /. usr_ts)

(*Global settings -- affect all viewports*)
let set_global_line_width t w =
  Sizes.set_lw t.initial_vp.scalings
    (if w <= 0. then def_lw else w /. usr_lw)
let set_global_mark_size t m =
  Sizes.set_marks t.initial_vp.scalings
    (if m <= 0. then def_marks else m /. usr_marks)
let set_global_font_size t s =
  Sizes.set_ts t.initial_vp.scalings
    (if s <= 0. then def_ts else s /. usr_ts)

(*Getters*)
let get_line_width t = (Sizes.get_lw t.current_vp.scalings) *. usr_lw
let get_mark_size t = (Sizes.get_marks t.current_vp.scalings) *. usr_marks


(* Backend primitives (not overriden by viewport system)
 **********************************************************************)
let add_order f t = Queue.add f t.current_vp.orders

(*FIXME: needed?*)
let width t = Backend.width t.backend
let height t = Backend.height t.backend
let set_color t c =
  add_order (fun () -> Backend.set_color t.backend c) t;
  Backend.set_color t.backend c

let set_line_cap t lc =
  add_order (fun () -> Backend.set_line_cap t.backend lc) t;
  Backend.set_line_cap t.backend lc

let set_dash t x y =
  add_order (fun () -> Backend.set_dash t.backend x y) t;
  Backend.set_dash t.backend x y

let set_line_join t join=
  add_order (fun () -> Backend.set_line_join t.backend join) t;
  Backend.set_line_join t.backend join

let get_line_cap t = Backend.get_line_cap t.backend
let get_dash t = Backend.get_dash t.backend
let get_line_join t = Backend.get_line_join t.backend

let move_to t ~x ~y =
  add_order (fun () -> Backend.move_to t.backend x y) t

let line_to t ~x ~y =
  add_order (fun () -> Backend.line_to t.backend x y) t

let rel_move_to t ~x ~y =
  add_order (fun () -> Backend.rel_move_to t.backend x y) t

let rel_line_to t ~x ~y =
  add_order (fun () -> Backend.rel_line_to t.backend x y) t

let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  add_order (fun () -> Backend.curve_to t.backend x1 y1 x2 y2 x3 y3) t

let rectangle t ~x ~y ~w ~h =
  add_order (fun () -> Backend.rectangle t.backend x y w h) t

let arc t ~x ~y ~r ~a1 ~a2 =
  add_order (fun () -> Backend.arc t.backend x y r a1 a2) t

let close_path t =
 add_order (fun () -> Backend.close_path t.backend) t
let clear_path t =
 add_order (fun () -> Backend.clear_path t.backend) t
(*let path_extents t = Backend.path_extents t.backend*)

(*Stroke when using current coordinates.*)
let stroke_current t =
  add_order (fun () -> Backend.stroke t.backend) t
let stroke_current_preserve t =
  add_order (fun () -> Backend.stroke_preserve t.backend) t

let stroke t =
  let f () =
    let ctm = Coordinate.use t.backend t.normalized in
    Backend.set_line_width t.backend (Sizes.get_lw t.current_vp.scalings);
    Backend.stroke t.backend;
    Coordinate.restore t.backend ctm
  in
  add_order f t

let stroke_preserve t =
  let f () =
    let ctm = Coordinate.use t.backend t.normalized in
    Backend.set_line_width t.backend (Sizes.get_lw t.current_vp.scalings);
    Backend.stroke_preserve t.backend;
    Coordinate.restore t.backend ctm
  in
  add_order f t

let fill t =
  add_order (fun () -> Backend.fill t.backend) t
let fill_preserve t =
  add_order (fun () -> Backend.fill_preserve t.backend) t

(*let clip t = Backend.clip t.backend
let clip_preserve t = Backend.clip_preserve t.backend*)

let clip_rectangle t ~x ~y ~w ~h =
  add_order (fun () -> Backend.clip_rectangle t.backend x y w h) t

let save_vp t =
  let f () =
    Backend.save t.backend;
    let vp = t.current_vp in
    let sizes =
      Sizes.get_lw vp.scalings,
      Sizes.get_ts vp.scalings,
      Sizes.get_marks vp.scalings
    in
    Stack.push sizes vp.scalings_hist
  in
  add_order f t

let restore_vp t =
  let f () =
    let vp = t.current_vp in
    try
      let lw, ts, marks = Stack.pop vp.scalings_hist in
      Sizes.set_lw vp.scalings lw;
      Sizes.set_ts vp.scalings ts;
      Sizes.set_marks vp.scalings marks;
      Backend.restore t.backend
    with Stack.Empty -> ()
  in
  add_order f t

let select_font_face t slant weight family =
  add_order (fun () -> Backend.select_font_face t.backend slant weight family) t

(*FIXME: when [Backend.show_text], the backend temporarily returns to
  raw coordinates -- but converts its input.*)
let show_text t ~rotate ~x ~y pos txt=
  let f () =
    let ctm = Coordinate.use t.backend t.normalized in
    Backend.set_font_size t.backend (Sizes.get_ts t.current_vp.scalings);
    Backend.show_text t.backend ~rotate ~x ~y pos txt;
    Coordinate.restore t.backend ctm
  in
  add_order f t

let text_extents t =
  Backend.set_font_size t.backend (Sizes.get_ts t.current_vp.scalings);
  Backend.text_extents t.backend

let render t name =
  let f () =
    let ctm = Coordinate.use t.backend t.normalized in
    let marks = Sizes.get_marks t.current_vp.scalings in
    Backend.scale t.backend marks marks;
    Pointstyle.render name t.backend;
    Coordinate.restore t.backend ctm
  in
  add_order f t

(*
let render_extents t name =
  let ctm = Coordinate.use t.backend t.normalized in
  let marks = Sizes.get_marks t.coords.scalings in
  Backend.scale t.backend marks marks;
  let rect = Pointstyle.render_extents name t.backend in
  Coordinate.restore t.backend ctm;
  (*Now express [rect] in device coords*)
  (*let x', y' = Coordinate.to_device marks rect.Backend.x rect.Backend.y in
  let w', h' =
    Coordinate.to_device_distance marks rect.Backend.w rect.Backend.h
  in
  {Backend.x = x'; y = y'; w = w'; h = h'}*)
  rect*)

let mark_extents t name =
  let rect = Pointstyle.extents name in
  (*(*Trick to get the transformation matrix*)
  let ctm = Coordinate.use t.backend t.coords.marks in
  let marks = Backend.get_matrix t.backend in
  Coordinate.restore t.backend ctm;
  (*Now working with [marks], which is the transformation matrix of marks.*)
  let x',y' =
    Backend.Matrix.transform_point marks rect.Backend.x rect.Backend.y
  in
  let wx, wy = Backend.Matrix.transform_distance marks rect.Backend.w 0.
  and hx, hy = Backend.Matrix.transform_distance marks 0. rect.Backend.h in
  assert (wx > 0. && wy = 0.);
  assert (hx = 0. && hy > 0.);*)
  (*i.e. assert no rotation.*)
  (*
  (*These are the general formulae*)
    let neg x = min x 0. and absf = abs_float in
    let xmin = x' +. (neg wx) +. (neg hx)
    and ymin = y' +. (neg wy) +. (neg hy) in
    let wnew = (absf wx) +. (absf hx) and hnew = (absf hy) +. (absf wy) in
    (*Extents: xmin,ymin,wnew,hnew*)
  *)
  let marks = Sizes.get_marks t.current_vp.scalings in
  {Backend.x = rect.Backend.x *. marks; y = rect.Backend.y *. marks;
   w = rect.Backend.w *. marks; h = rect.Backend.h *. marks}


let plotfx t ?axes ?nsamples ?min_step f a b =
  let f () =
    let _, ranges , fct =
      Functions.samplefxy (fun t -> (t,f t)) ?nsamples ?min_step b a
    in
    fct (fun () (x,y) -> Backend.line_to t.backend x y) ();
    match axes with
      None -> ()
    | Some axes ->
        let lw = Sizes.get_lw t.current_vp.scalings in
        let lines = Coordinate.make_scale t.normalized lw lw in
        Axes.print axes ~lines ~ranges t.backend
  in
  add_order f t

let f t mark x y =
  move_to t x y;
  render t mark

let plotxy t ?axes ?(f = f) ?(mark = "X") iter =
  let f () =
    let rec plot () =
      match Iterator.next iter with
        None -> ()
      | Some(x,y) ->
          f t mark x y;
          plot ()
    in plot ();
    match axes with
      None -> ()
    | Some axes ->
        let lw = Sizes.get_lw t.current_vp.scalings in
        let lines = Coordinate.make_scale t.normalized lw lw in
        let ranges = Iterator.extents iter in
        Axes.print axes ~lines ~ranges t.backend
  in
  add_order f t


