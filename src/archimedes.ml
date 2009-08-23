type t = {backend:Backend.t;
          mutable coord:Coordinate.t;
          mutable bounds: float * float * float* float}

let make ~dirs name w h =
  let backend = Backend.make ~dirs name w h in
  let coord = Coordinate.make_identity () in
  Coordinate.scale coord w h;
  let _ = Coordinate.use backend coord in
  {backend=backend;
   coord=coord;
   bounds = 0.,0.,1.,1.}

let close t = Backend.close t.backend

let plotfx t ?axes ?nsamples ?min_step f a b =
  let _, (xmin,xmax,ymin, ymax) , fct =
    Functions.samplefxy (fun t -> (t,f t)) ?nsamples ?min_step a b
  in
  fct (fun () (x,y) -> Backend.line_to t.backend x y) ();
  match axes with
    None -> ()
  | Some axes -> Axes.print axes ~lines:t.coord
      ~xmin ~xmax ~ymin ~ymax t.backend

let plotxy t ?axes iter = ()

module Viewport =
struct
  type vp =
      {context: t;
       coordinate:Coordinate.t;
       xmin:float; xmax:float;
       ymin:float; ymax:float}

  let make arch ~xmin ~xmax ~ymin ~ymax =
    let coord = Coordinate.make_translate arch.coord xmin ymin in
    Coordinate.scale coord (xmax -. xmin) (ymax -. ymin);
    { context = arch;
      coordinate = coord; xmin = 0.; xmax = 1.; ymin = 0.; ymax = 1.;
    }

  let sub vp ~xmin ~xmax ~ymin ~ymax =
    let coord = Coordinate.make_translate vp.coordinate xmin ymin in
    Coordinate.scale coord (xmax -. xmin) (ymax -. ymin);
    { context = vp.context;
      coordinate = coord; xmin = 0.; xmax = 1.; ymin = 0.; ymax = 1.;
    }

  let use vp =
    vp.context.coord <- vp.coordinate;
    let new_bounds = vp.xmin, vp.ymin, vp.xmax, vp.ymax in
    vp.context.bounds <- new_bounds

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
