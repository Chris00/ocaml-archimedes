type t = Backend.matrix =
    {mutable xx:float;
     mutable yx:float;
     mutable xy:float;
     mutable yy:float;
     mutable x0:float;
     mutable y0:float;}

let identity () = {xx = 1.; xy = 0.; yx = 0.; yy = 1.; x0 = 0.; y0 = 0.}

let create xx xy xt yx yy yt =
  {xx = xx; xy = xy; x0 = xt; yx = yx; yy = yy; y0 = yt}

let translate t x y =
  t.x0 <- t.x0 +. x;
  t.y0 <- t.y0 +. y

let scale t x y =
  t.xx <- t.xx *. x;
  t.xy <- t.xy *. x;
  t.x0 <- t.x0 *. x;
  t.yx <- t.yx *. y;
  t.yy <- t.yy *. y;
  t.y0 <- t.y0 *. y

let rotate t angle =
  let a = cos angle and b = sin angle in
  let rx x y = a *. x +. b *. y
  and ry x y = a *. y -. b *. x in
  t.xx <- rx t.xx t.yx;
  t.xy <- rx t.xy t.yy;
  t.x0 <- rx t.x0 t.y0;
  t.yx <- ry t.xx t.yx;
  t.yy <- ry t.xy t.yy;
  t.y0 <- ry t.x0 t.y0

let transform t x y =
  t.xx *. x +. t.xy *. y +. t.x0,
  t.yx *. x +. t.yy *. y +. t.y0

let transform_dist t x y =
  t.xx *. x +. t.xy *. y,
  t.yx *. x +. t.yy *. y

let det t =
  t.xx *. t.yy -. t.xy *. t.yx

let get_invert t =
  let d = det t in
  if d = 0. then failwith "invert"
  else
    let xx =  t.yy /. d
    and xy = (-. t.xy) /. d
    and yx = (-. t.yx) /. d
    and yy = t.xx /. d in
    let xt = t.x0 *. xx +. t.y0 *. yx
    and yt = t.x0 *. yx +. t.y0 *. yy in
    xx, xy, xt, yx ,yy , yt

let invert t =
  let xx, xy, xt, yx, yy, yt = get_invert t in
    {xx = xx; xy = xy; x0 = xt; yx = yx; yy = yy; y0 = yt}

let inv_transform t x y =
  let xx, xy, xt, yx, yy, yt = get_invert t in
  xx *. x +. xy *. y +. xt,
  yx *. x +. yy *. y +. yt

let inv_transform_dist t x y =
  let xx, xy, _, yx, yy, _ = get_invert t in
  xx *. x +. xy *. y,
  yx *. x +. yy *. y


let apply ?result ~next_t t =
  let xx = next_t.xx *. t.xx +. next_t.xy *. t.yx
  and xy = next_t.xx *. t.xy +. next_t.xy *. t.yy
  and yx = next_t.yx *. t.xx +. next_t.yy *. t.yx
  and yy = next_t.yx *. t.xy +. next_t.yy *. t.yy
  and xt = next_t.xx *. t.x0 +. next_t.xy *. t.y0 +. next_t.x0
  and yt = next_t.yx *. t.x0 +. next_t.yy *. t.y0 +. next_t.y0 in
  let r = match result with
      None -> t
    | Some r -> r
  in
  r.xx <- xx; r.xy <- xy; r.x0 <- xt;
  r.yx <- yx; r.yy <- yy; r.y0 <- yt

let copy t =
  create t.xx t.xy t.x0 t.yx t.yy t.y0

let reset_to_id t =
  t.xx <- 1.; t.xy <- 0.; t.x0 <- 0.;
  t.yx <- 0.; t.yy <- 1.; t.y0 <- 0.


let has_shear t =
  t.yx <> 0. || t.xy <> 0.
(*Local variables:*)
(*compile-command: "ocamlc -c coordinate.ml && ocamlopt -c coordinate.ml"*)
(*End:*)
