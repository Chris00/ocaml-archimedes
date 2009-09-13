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

exception No_current_point

(*Context-independent viewport data*)
type viewport =
    {
      (*For text, line width, marks size*)
      scalings: Sizes.t;
      (*Bounds (only mutable so set it to Some... when starting to draw.*)
      mutable ranges: Axes.ranges option;
      (*To update the ranges correctly, we need the current point.*)
      mutable current_pt: (float * float) option;
      (*What has to be plotted in this viewport. Mutable so that if we
        want to do the orders but preserve the queue, we replace it by a
        (created and updated) copy.*)
      mutable orders: (unit -> unit) Queue.t;
      (*For saving and restoring states*)
      scalings_hist : (float * float * float) Stack.t;
    }

(*Used coordinate transformations*)
type coords =
    {vp_device:Coordinate.t;
     mutable user_device:Coordinate.t
       (*Note: only modified in case of adding a second point.*)}

type t =
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
     initial_coords : coords;(*Initial coordinate system -- the
                               vp_to_device field is the one
                               which makes the surface as a unit
                               square*)
     initial_vp: viewport;(*Initial viewport --
                            the one which is associated with the initial coordinates.*)
     mutable vp : viewport;(*Current viewport*)
     mutable coords: coords; (*Currently used coordinate
                                           transformations*)
     mutable used_vp: (coords * viewport) Queue.t;
     (*Viewports that have been used. We can ensure that only one copy
       of each used viewport will be in this queue -- because of the
       viewport flag "used".

       Note: mutability of this field is needed for efficiency reasons
       -- immediate drawing needs to do the orders, but preserve them;
       to avoid recopying the orders, we simply replace them by a
       (created and updated) copy.*)
     mutable immediate_drawing:bool;
     (*Says whether we draw immediately or wait for closing.*)
     mutable only_immediate : bool
       (*Says whether an order has only to be made immediately, or
         must be stored. This field is not modifiable by users.*)
    }

(*Default line width, text size, mark size: if the height is less than
  width, then height is by default equal to:

  * 500 superposed lines, or
  * 10 lines of text, or
  * 100 boxes of 1x1 marks.
*)
let def_lw, def_ts, def_marks = 0.002, 0.12, 0.01

(*User transformations. To get the previous defaults, the user will
  enter resp. 1, 12, 1. (12 is determined experimentally using cairo:
  a 100 x 100 pixels output can contain 10 lines of text, putting font
  size as 12.) *)
let usr_lw, usr_ts, usr_marks = 500., 100., 100.

(*Initial scale of the user_to_viewport coordinates.*)
let initial_scale = 1.

let make ~dirs name w h =
  let backend = Backend.make ~dirs name w h in
  let init = Coordinate.make_identity () in
  let init2 = Coordinate.make_scale init initial_scale initial_scale in
  let coord = Coordinate.make_scale init w h in
  let coord2 = Coordinate.make_scale coord initial_scale initial_scale in
  let square_side = min w h in
  let norm = Coordinate.make_scale init square_side square_side in
  let initial =
    {scalings = Sizes.make_root def_lw def_ts def_marks;
     (*No drawings yet*)
     ranges = None;
     current_pt = None;
     orders = Queue.create ();
     scalings_hist = Stack.create ();
    }
  in
  (*The initial viewport is in use, so must be in the [used_vp].*)
  let init_used_vp = Queue.create () in
  let coords ={vp_device = coord; user_device = coord2} in
  Queue.push (coords, initial) init_used_vp;
  {backend=backend;
   rawcoords = init;
   normalized = norm;
   square_side = square_side;
   initial_coords = {vp_device = init; user_device = init2};
   initial_vp = initial;
   coords = coords;
   vp = initial;
   used_vp = init_used_vp;
   immediate_drawing = false;
   only_immediate = false;
  }


let check t =
  (*A handle is closed iff there's no stored viewports (initialization
    contains the initial viewport).*)
  if Queue.is_empty t.used_vp then
    failwith "Archimedes.Handle: closed"

let do_viewport_orders ?orders_queue coords vp backend =
  match vp.ranges with
    None -> (*No ranges, so no drawings *)
      ()
  | Some ranges ->
      let ctm = Coordinate.use backend coords.user_device in
      let m1 = coords.vp_device.Coordinate.ctm in
      let m2 = coords.user_device.Coordinate.ctm in
      let m3 = coords.user_device.Coordinate.tm in
      Printf.printf "vp_device: %f %f %f %f %f %f\n%!"
        m1.xx m1.xy m1.yx m1.yy m1.x0 m1.y0;
      Printf.printf "user_device: %f %f %f %f %f %f\n%!"
        m2.xx m2.xy m2.yx m2.yy m2.x0 m2.y0;
      Printf.printf "user_vp: %f %f %f %f %f %f\n%!"
        m3.xx m3.xy m3.yx m3.yy m3.x0 m3.y0;
      let rec make_orders orders =
        if not (Queue.is_empty orders) then (
          Printf.printf "*%!";
          let order = Queue.pop orders in
          order ();
          (match orders_queue with None -> ()
           | Some q -> Queue.push order q);
          make_orders orders)
        else match orders_queue with
          None -> ()
        | Some q -> vp.orders <- q;
      in
      make_orders vp.orders;
      Coordinate.restore backend ctm


let do_orders t preserve =
  let vp_storing_queue = Queue.create () in
  let rec do_viewports_orders () =
    if not (Queue.is_empty t.used_vp) then (
      Printf.printf "\nPop a vp%!";
      let coords, vp = Queue.pop t.used_vp in
      let orders_queue =
        if preserve then Some (Queue.create ())
        else None
      in
      do_viewport_orders ?orders_queue coords vp t.backend;
      if preserve then Queue.push (coords, vp) vp_storing_queue;
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

(*Easy update Axes.ranges options and coordinates*)
let update_coords t x y =
  match t.vp.ranges with
    None ->
      t.vp.ranges <- Some (Axes.Ranges.make x y);
      Coordinate.translate t.coords.user_device
        (x+.initial_scale/.2.) (y+.initial_scale/.2.)
  | Some ranges ->
      let xmin = ranges.Axes.xmin
      and xmax = ranges.Axes.xmax
      and ymin = ranges.Axes.ymin
      and ymax = ranges.Axes.ymax in
      let one_point = xmin = xmax && ymin = ymax in
      let updated = Axes.Ranges.update ranges x y in
      Printf.printf "Ranges: %f %f to %f %f, point %f %f\n%!"
        xmin ymin xmax ymax x y;
      if updated then (
        (*Coordinate changement*)
        let xmin = min x xmin
        and xmax = max x xmax
        and ymin = min y ymin
        and ymax = max y ymax in
        let diffx = xmax -. xmin
        and diffy = ymax -. ymin in
        let new_matrix =
          Matrix.make_scale (1. /.diffx) (1. /.diffy)
        in
        Matrix.translate new_matrix ( -.xmin) ( -.ymin);
        Printf.printf "New user_vp: %f %f %f %f %f %f\n%!"
          new_matrix.xx new_matrix.xy new_matrix.yx new_matrix.yy
          new_matrix.x0 new_matrix.y0;
        Coordinate.transform t.coords.user_device new_matrix;
        (*
           (
        (*Translate the current coordinate to the minimal values;
           then make the scale so that "all is taken".*)
           Coordinate.translate t.coords.user_device (min x xmin) (min y ymin);
           let scal vmin vmax v =
           let diff = vmax -. vmin
           and diff0 = v -. vmin
           and diff1 = vmax -. v in
           let maxi = max (max diff0 diff1) diff in
           maxi /. diff
           in
           Coordinate.scale t.coords.user_device
           (scal xmin xmax x) (scal ymin ymax y)
           ) *)
        (*In case of immediate drawing, needs to replot everything for this viewport.*)
        if t.immediate_drawing then (
          (*Deletes the drawing by covering.*)
          (*FIXME: how to manage with transparent backgrounds?*)
          let ctm = Coordinate.use t.backend t.coords.vp_device in
          Backend.save t.backend;
          (*FIXME: viewport's background?*)
          Backend.set_color t.backend Color.white;
          Backend.rectangle t.backend 0. 0. 1. 1.;
          Backend.fill t.backend;
          Backend.restore t.backend;
          Coordinate.restore t.backend ctm;
          (*Now, replot in the new coordinates.*)
          do_viewport_orders t.coords t.vp t.backend
        )
      )





module Viewport =
struct
  type vp =
      {handle: t;
       tcs: coords;
       data:viewport;
       (*Flag indicating if this viewport has been used in an Archimedes context*)
       mutable used : bool;}

  let inner_make handle coord =
    let data =
      { (*Line width and text and mark sizes are preserved, but are new ones,
          depending on the previous ones.*)
        scalings = Sizes.child handle.vp.scalings 1. 1. 1.;
        (*No drawings yet on this viewport.*)
        ranges = None;
        current_pt = None;
        orders = Queue.create ();
        (*Not in use*)
        scalings_hist = Stack.create();
      }
    in
    let coord2 = Coordinate.make_scale coord initial_scale initial_scale in
    { handle = handle;
      tcs = {vp_device = coord; user_device = coord2};
      data = data;
      used = false;}

  let make handle ~xmin ~xmax ~ymin ~ymax =
    Printf.printf "New viewport:%f %f to %f %f\n%!" xmin ymin xmax ymax;
    let ndrawings = Coordinate.make_translate handle.coords.vp_device xmin ymin in
    Coordinate.scale ndrawings (xmax -. xmin) (ymax -. ymin);
    inner_make handle ndrawings


  let sub vp ~xmin ~xmax ~ymin ~ymax =
    Printf.printf "Sub viewport:%f %f to %f %f\n%!" xmin ymin xmax ymax;
    let ndrawings = Coordinate.make_translate vp.tcs.vp_device xmin ymin in
    Coordinate.scale ndrawings (xmax -. xmin) (ymax -. ymin);
    inner_make vp.handle ndrawings


  let make_rect handle ~x ~y ~w ~h =
    Printf.printf "new viewport:%f %f w:%f h:%f\n%!" x y w h;

    let ndrawings = Coordinate.make_translate handle.coords.vp_device x y in
    Coordinate.scale ndrawings w h;
    inner_make handle ndrawings


  let sub_rect vp ~x ~y ~w ~h =
    Printf.printf "sub viewport:%f %f w:%f h:%f\n%!" x y w h;
    let ndrawings = Coordinate.make_translate vp.tcs.vp_device x y in
    Coordinate.scale ndrawings w h;
    inner_make vp.handle ndrawings


  let use vp =
    (*Need to change the current viewport and coordinates of the
      context, but also flag the viewport, if it is not, and add it to
      the [used_vp] field of the context.*)
    let handle = vp.handle in
    handle.coords <- vp.tcs;
    handle.vp <- vp.data;
    if not vp.used then (
      vp.used <- true;
      Queue.push (vp.tcs, vp.data) handle.used_vp)

  (*{2 Convenience functions to create viewports}*)
  let rows handle n =
    let step = 1./.(float n) in
    let f i = make_rect handle 0. ((float i)*. step)  1. step in
    Array.init n f

  let columns handle n =
    let step = 1./.(float n) in
    let f i = make_rect handle ((float i)*. step) 0. step 1. in
    Array.init n f

  let matrix handle n m =
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

let use_initial t =
  t.coords <- t.initial_coords;
  t.vp <- t.initial_vp

(*Local settings -- in a viewport*)
let set_line_width t w =
  Sizes.set_lw t.vp.scalings
    (if w <= 0. then def_lw else w /. usr_lw)
let set_mark_size t m =
  Sizes.set_marks t.vp.scalings
    (if m <= 0. then def_marks else m /. usr_marks)
let set_font_size t s =
  Sizes.set_ts t.vp.scalings
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
let get_line_width t = (Sizes.get_lw t.vp.scalings) *. usr_lw
let get_mark_size t = (Sizes.get_marks t.vp.scalings) *. usr_marks
let get_font_size t = (Sizes.get_ts t.vp.scalings) *. usr_ts



let from_backend f t = f t.backend

(* Backend primitives (not overriden by viewport system)
 **********************************************************************)
let add_order f t =
  if t.only_immediate then f ()
  else ( Queue.add f t.vp.orders;
         if t.immediate_drawing then f ())

let get_current_pt t = match t.vp.current_pt with
    None -> raise No_current_point
  | Some (x,y) -> x,y
let set_current_pt t x y = t.vp.current_pt <- Some (x,y)


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
  set_current_pt t x y;
  update_coords t x y;
  add_order (fun () -> Backend.move_to t.backend x y) t

let line_to t ~x ~y =
  set_current_pt t x y;
  update_coords t x y;
  add_order (fun () -> Backend.line_to t.backend x y) t

let rel_move_to t ~x ~y =
  let x', y' = get_current_pt t in
  update_coords t (x +. x') (y +. y');
  set_current_pt t (x +. x') (y +. y');
  add_order (fun () -> Backend.rel_move_to t.backend x y) t

let rel_line_to t ~x ~y =
  let x', y' = get_current_pt t in
  update_coords t (x +. x') (y +. y');
  set_current_pt t (x +. x') (y +. y');
  add_order (fun () -> Backend.rel_line_to t.backend x y) t

let curve_to t ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
  update_coords t x1 y1;
  update_coords t x2 y2;
  update_coords t x3 y3;
  set_current_pt t x3 y3;
  add_order (fun () -> Backend.curve_to t.backend x1 y1 x2 y2 x3 y3) t

let rectangle t ~x ~y ~w ~h =
  update_coords t x y;
  update_coords t (x+.w) (y+.h);
  add_order (fun () -> Backend.rectangle t.backend x y w h) t

let arc t ~r ~a1 ~a2 =
  (*FIXME: better bounds for the arc can be found.*)
  let x', y' = get_current_pt t in
  update_coords t (x'+.r) (y'+.r);
  update_coords t (x'-.r) (y'-.r);
  add_order (fun () -> Backend.arc t.backend r a1 a2) t

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
    Backend.set_line_width t.backend (Sizes.get_lw t.vp.scalings);
    Backend.stroke t.backend;
    Coordinate.restore t.backend ctm
  in
  add_order f t

let stroke_preserve t =
  let f () =
    let ctm = Coordinate.use t.backend t.normalized in
    Backend.set_line_width t.backend (Sizes.get_lw t.vp.scalings);
    Backend.stroke_preserve t.backend;
    Coordinate.restore t.backend ctm
  in
  add_order f t

let fill t =
  add_order (fun () -> Backend.fill t.backend) t
let fill_preserve t =
  add_order (fun () -> Backend.fill_preserve t.backend) t

let clip_rectangle t ~x ~y ~w ~h =
  add_order (fun () -> Backend.clip_rectangle t.backend x y w h) t

let save_vp t =
  let f () =
    Backend.save t.backend;
    let sizes =
      Sizes.get_lw t.vp.scalings,
      Sizes.get_ts t.vp.scalings,
      Sizes.get_marks t.vp.scalings
    in
    Stack.push sizes t.vp.scalings_hist
  in
  add_order f t

let restore_vp t =
  let f () =
    try
      let lw, ts, marks = Stack.pop t.vp.scalings_hist in
      Sizes.set_lw t.vp.scalings lw;
      Sizes.set_ts t.vp.scalings ts;
      Sizes.set_marks t.vp.scalings marks;
      Backend.restore t.backend
    with Stack.Empty -> ()
  in
  add_order f t

let select_font_face t slant weight family =
  add_order (fun () -> Backend.select_font_face t.backend slant weight family) t

(*FIXME: when [Backend.show_text], the backend temporarily returns to
  raw coordinates -- but converts its input.*)
let show_text t ~rotate ~x ~y pos txt=
  let ts = Sizes.get_ts t.vp.scalings in
  let text_coord = Coordinate.make_scale t.normalized ts ts in
  let font_size = ts *. t.square_side in
  let f () =
    let ctm' = Coordinate.use t.backend text_coord in
    Backend.set_font_size t.backend font_size;
    Backend.show_text t.backend ~rotate ~x ~y pos txt;
    Coordinate.restore t.backend ctm'
  in
  let ctm = Coordinate.use t.backend text_coord in
  Backend.set_font_size t.backend font_size;
  let rect = Backend.text_extents t.backend txt in
  Coordinate.restore t.backend ctm;
  update_coords t rect.x rect.y;
  update_coords t
    (rect.x +. rect.w)
    (rect.y +. rect.h);
  add_order f t

let text_extents t txt =
  let ts = Sizes.get_ts t.vp.scalings in
  let text_coord = Coordinate.make_scale t.normalized ts ts in
  let font_size = ts *. t.square_side in
  let ctm = Coordinate.use t.backend text_coord in
  Backend.set_font_size t.backend font_size;
  let rect = Backend.text_extents t.backend txt in
  Coordinate.restore t.backend ctm;
  rect

let render t name =
  let marks = Sizes.get_marks t.vp.scalings in
  let f () =
    let ctm = Coordinate.use t.backend t.normalized in
    Backend.scale t.backend marks marks;
    Pointstyle.render name t.backend;
    Coordinate.restore t.backend ctm;
  in
  let extents = Pointstyle.extents name in
  update_coords t (extents.x *. marks) (extents.y *. marks);
  update_coords t ((extents.x +. extents.w) *. marks)
    ((extents.y +.extents.h) *. marks);
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


let f_line_to t (x,y) = Backend.line_to t.backend x y
let f_finish t = Backend.stroke t.backend

let f t ?color ?nsamples ?min_step
    ?(do_with = f_line_to) ?(finish = f_finish) f a b =
  let _, ranges , fct =
    Functions.samplefxy (fun t -> (t,f t)) ?nsamples ?min_step b a
  in
  let f () =
    Backend.save t.backend;
    (match color with
      Some color -> Backend.set_color t.backend color
    | None -> ());
    t.only_immediate <- true;
    fct (fun () -> do_with t) ();
    finish t;
    t.only_immediate <- false;
    Backend.restore t.backend;
  in
  update_coords t ranges.Axes.xmin ranges.Axes.ymin;
  update_coords t ranges.Axes.xmax ranges.Axes.ymax;
  add_order f t

let xy_mark mark t x y =
  move_to t x y;
  render t mark

let xy_finish t = ()

let xy t ?axes ?(mark = "X") ?(f = (xy_mark mark)) iter =
  let rec iterate do_with =
    match Iterator.next iter with
      None -> xy_finish t
    | Some(x,y) ->
        do_with t x y;
        iterate do_with
  in
  let f () =
    t.only_immediate <- true;
    iterate f;
    t.only_immediate <- false;
  in
  let extents = Iterator.extents iter in
  update_coords t extents.Axes.xmin extents.Axes.ymin;
  update_coords t extents.Axes.xmax extents.Axes.ymax;
  add_order f t

let make_xaxis = Axes.make_xaxis
let make_yaxis = Axes.make_yaxis
let make_axes = Axes.make
let print_axes t axes ?(color = Color.black)
    ?axes_print ?axes_meeting ?print_tic ranges=
  let print_axes =
    match axes_print with
      None -> Axes.print_axes
    | Some f -> fun axes ranges _ -> f axes ranges t
  and print_tic =
    match print_tic with
      None -> Axes.print_tic
    | Some f -> fun _ -> f t
  in
  let scalings = t.vp.scalings in
  let ts = Sizes.get_ts scalings in
  let font_size = ts *. t.square_side /. 3. in
  let lines = Sizes.get_lw scalings
  and marks = Sizes.get_marks scalings in
  let ctm = Coordinate.use t.backend t.coords.vp_device in
  let xmargin, ymargin =
    Axes.get_margins axes ?axes_meeting ~normalization:t.normalized
      ~lines ~marks ~font_size ranges t.backend
  in
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
  Printf.printf "Margins 1: L%f R%f B%f T%f\nMargins 2: L%f R%f B%f T%f\n%!"
    xx1 xx2 xy1 xy2 yx1 yx2 yy1 yy2;
  let left = max xx1 yx1
  and right = max xx2 yx2
  and top = max xy2 yy2
  and bottom = max xy1 yy1 in
  let margin_x = left +. right
  and margin_y = top +. bottom in
  let margins_ok = margin_x < 1. && margin_y < 1. in
  let ctm = Coordinate.use t.backend t.coords.vp_device in
  (*let m = Backend.get_matrix t.backend in
  Printf.printf "Initial coord: %f %f %f %f %f %f \n%!"
    m.xx m.xy m.yx m.yy m.x0 m.y0;
  Coordinate.restore t.backend ctm;
  Printf.printf "Margins: %f %f %f %f \n%!"
    left right bottom top;*)
  let new_vp =
    if margins_ok then
      (let vp = Viewport.make t left (1. -.right) bottom (1. -.top) in
       Viewport.use vp;
       Some vp)
    else (
      Printf.printf "Handle -- warning: too big axes labels margins.\n%!";
      None)
  in
  (*let ctm = Coordinate.use t.backend t.current_coord in
  let m = Backend.get_matrix t.backend in
  Printf.printf "Initial coord: %f %f %f %f %f %f \n%!"
    m.xx m.xy m.yx m.yy m.x0 m.y0;
  Backend.save t.backend;
  Backend.rectangle t.backend 0. 0. 0.8 0.8;
  Backend.arc t.backend 0.9 0.9 0.1 0. 7.;
  Backend.set_color t.backend (Color.make ~a:0.2 0. 0. 1.);
  Backend.fill t.backend;
  Backend.restore t.backend;*)
  Coordinate.restore t.backend ctm;
  (*else, Labels take too big margins to plot correctly => no scaling.*)
  let f () =
    Backend.set_color t.backend color;
    Axes.print axes ~normalization:t.normalized
      ~lines ~marks ~font_size ~ranges
      ~print_axes ?axes_meeting ~print_tic t.backend;
  in
  add_order f t; (*if margins_ok, axes should stay in the initial viewport.*)
  new_vp



