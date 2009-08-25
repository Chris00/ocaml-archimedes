type coordinate =
    { (*Coordinate transformations*)
      mutable linewidth: Coordinate.t;
      mutable drawings: Coordinate.t;
      mutable normalized: Coordinate.t;
      (*Bounds*)
      mutable xmin:float; mutable xmax:float;
      mutable ymin:float; mutable ymax:float}


type t =
    {backend:Backend.t;
     mutable coords:coordinate;(*Current coordinate transformations*)
     history: coordinate Stack.t; (*Saved coordinate transformations*)
     orders: (Backend.t -> unit) Queue.t}

let make ~dirs name w h =
  let backend = Backend.make ~dirs name w h in
  let init = Coordinate.make_identity () in
  let lw = Coordinate.make_scale init 0.01 0.01 in
  let coord = Coordinate.make_scale init w h in
  let norm =
    let z = min w h in
    Coordinate.make_scale init z z
  in
  let _ = Coordinate.use backend coord in
  {backend=backend;
   coords=
      {linewidth = lw; drawings = coord; normalized = norm;
       (*No drawings yet*)
      xmin = max_float; xmax = -.max_float;
      ymin = max_float; ymax = -.max_float;};
   history = Stack.create ();
   orders = Queue.create ();
  }

let close t = Backend.close t.backend

module Viewport =
struct
  type vp =
      {context: t; coord_set:coordinate}

  let make arch ~xmin ~xmax ~ymin ~ymax =
    let ndrawings = Coordinate.make_translate arch.coords.drawings xmin ymin in
    Coordinate.scale ndrawings (xmax -. xmin) (ymax -. ymin);
    { context = arch;
      coord_set =
        { linewidth = (*same as previous one, but depends on it.*)
            Coordinate.make_translate arch.coords.linewidth 0. 0.;
          drawings = ndrawings;
          normalized = (*same as previous one, but depends on it.*)
            Coordinate.make_translate arch.coords.normalized 0. 0.;
          (*No drawings yet on this viewport.*)
          xmin = max_float; xmax = -.max_float;
          ymin = max_float; ymax = -.max_float;
        }
    }

  let sub vp ~xmin ~xmax ~ymin ~ymax =
    let ndrawings = Coordinate.make_translate vp.coord_set.drawings xmin ymin in
    Coordinate.scale ndrawings (xmax -. xmin) (ymax -. ymin);
    { context = vp.context;
      coord_set =
        { linewidth = (*same as previous one, but depends on it.*)
            Coordinate.make_translate vp.coord_set.linewidth 0. 0.;
          drawings = ndrawings;
          normalized = (*same as previous one, but depends on it.*)
            Coordinate.make_translate vp.coord_set.normalized 0. 0.;
          (*No drawings yet on this viewport.*)
          xmin = max_float; xmax = -.max_float;
          ymin = max_float; ymax = -.max_float;
        }
    }

  let use vp =
    let arch = vp.context in
    arch.coords <- vp.coord_set;
    ignore (Coordinate.use arch.backend arch.coords.drawings)

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



(* Backend primitives
 **********************************************************************)
let add_order f t = Queue.add f t.orders

(*FIXME: needed?*)
let width t = Backend.width t.backend
let height t = Backend.height t.backend
let set_color t = Backend.set_color t.backend
let set_line_width t = Backend.set_line_width t.backend
let set_line_cap t = Backend.set_line_cap t.backend
let set_dash t = Backend.set_dash t.backend
let set_line_join t = Backend.set_line_join t.backend
let get_line_width t = Backend.get_line_width t.backend
let get_line_cap t = Backend.get_line_cap t.backend
let get_dash t = Backend.get_dash t.backend
let get_line_join t = Backend.get_line_join t.backend

let move_to t = Backend.move_to t.backend

let line_to t = Backend.line_to t.backend

let rel_move_to t = Backend.rel_move_to t.backend

let rel_line_to t = Backend.rel_line_to t.backend

let curve_to t = Backend.curve_to t.backend

let rectangle t ~x ~y ~w ~h = Backend.rectangle t.backend x y w h

let arc t = Backend.arc t.backend

let close_path t = Backend.close_path t.backend
let clear_path t = Backend.clear_path t.backend
let path_extents t = Backend.path_extents t.backend

(*Stroke when using current coordinates.*)
let stroke_current t = Backend.stroke t.backend
let stroke_current_preserve t = Backend.stroke_preserve t.backend

let stroke t =
  let ctm = Coordinate.use t.backend t.coords.linewidth in
  Backend.stroke t.backend;
  Coordinate.restore t.backend ctm

let stroke_preserve t =
  let ctm = Coordinate.use t.backend t.coords.linewidth in
  Backend.stroke_preserve t.backend;
  Coordinate.restore t.backend ctm

let fill t = Backend.fill t.backend
let fill_preserve t = Backend.fill_preserve t.backend
(*let clip t = Backend.clip t.backend
let clip_preserve t = Backend.clip_preserve t.backend*)
let clip_rectangle t = Backend.clip_rectangle t.backend


(*FIXME: what about saving defined coordinates and killing the new
  ones when restoring?*)
let save t =
  Backend.save t.backend;
  Stack.push t.coords t.history

let restore t =
  try
    t.coords <- Stack.pop t.history;
    Backend.restore t.backend
  with Stack.Empty -> ()

let select_font_face t = Backend.select_font_face t.backend

(*Note: font size is controlled by [normalized] coordinates.*)
(*let set_font_size t size =
  let factor = size /. 100. in*)

let adjust_font_size t factor =
  Coordinate.scale t.coords.normalized factor factor

let show_text t ~rotate ~x ~y pos txt=
  let ctm = Coordinate.use t.backend t.coords.normalized in
  let matrix = Backend.get_matrix t.backend in
  let _, size = Backend.Matrix.transform_distance matrix 0. 10. in
  Backend.set_font_size t.backend size;
  Backend.show_text t.backend ~rotate ~x ~y pos txt;
  Coordinate.restore t.backend ctm

let text_extents t = Backend.text_extents t.backend

let render t name =
  let ctm = Coordinate.use t.backend t.coords.linewidth in
  Pointstyle.render name t.backend;
  Coordinate.restore t.backend ctm


let render_extents t name =
  let ctm = Coordinate.use t.backend t.coords.linewidth in
  let rect = Pointstyle.render_extents name t.backend in
  Coordinate.restore t.backend ctm;
  (*Now express [rect] in device coords*)
  (*let x', y' = Coordinate.to_device marks rect.Backend.x rect.Backend.y in
  let w', h' =
    Coordinate.to_device_distance marks rect.Backend.w rect.Backend.h
  in
  {Backend.x = x'; y = y'; w = w'; h = h'}*)
  rect

let mark_extents t name =
  let rect = Pointstyle.extents name in
  (*Trick to get the transformation matrix*)
  let ctm = Coordinate.use t.backend t.coords.linewidth in
  let marks = Backend.get_matrix t.backend in
  Coordinate.restore t.backend ctm;
  (*Now working with [marks], which is the transformation matrix of linestyles.*)
  let x',y' =
    Backend.Matrix.transform_point marks rect.Backend.x rect.Backend.y
  in
  let wx, wy = Backend.Matrix.transform_distance marks rect.Backend.w 0.
  and hx, hy = Backend.Matrix.transform_distance marks 0. rect.Backend.h in
  assert (wx > 0. && wy = 0.);
  assert (hx = 0. && hy > 0.);
  (*i.e. assert no rotation.*)
  (*
  (*These are the general formulae*)
    let neg x = min x 0. and absf = abs_float in
    let xmin = x' +. (neg wx) +. (neg hx)
    and ymin = y' +. (neg wy) +. (neg hy) in
    let wnew = (absf wx) +. (absf hx) and hnew = (absf hy) +. (absf wy) in
    (*Extents: xmin,ymin,wnew,hnew*)
  *)
  {Backend.x = x'; y = y'; w = wx; h = hy}


let plotfx t ?axes ?nsamples ?min_step f a b =
  let _, (xmin,xmax,ymin, ymax) , fct =
    Functions.samplefxy (fun t -> (t,f t)) ?nsamples ?min_step b a
  in
  fct (fun () (x,y) -> Backend.line_to t.backend x y) ();
  match axes with
    None -> ()
  | Some axes -> Axes.print axes ~lines:t.coords.linewidth
      ~xmin ~xmax ~ymin ~ymax t.backend

let f t mark x y =
  move_to t x y;
  render t mark

let plotxy t ?axes ?(f = f) ?(mark = "X") iter =
  let xmin, xmax, ymin, ymax = Iterator.extents iter in
  let rec plot () =
    match Iterator.next iter with
      None -> ()
    | Some(x,y) ->
        f t mark x y;
        plot ()
  in plot ();
  match axes with
    None -> ()
  | Some axes -> Axes.print axes ~lines:t.coords.linewidth
      ~xmin ~xmax ~ymin ~ymax t.backend


