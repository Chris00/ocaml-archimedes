type t =
    {mutable xx:float;
     mutable xy:float;
     mutable yx:float;
     mutable yy:float;
     mutable xt:float;
     mutable yt:float;}

let identity () = {xx = 1.; xy = 0.; yx = 0.; yy = 1.; xt = 0.; yt = 0.}

let create xx xy xt yx yy yt =
  {xx = xx; xy = xy; xt = xt; yx = yx; yy = yy; yt = yt}

let translate t x y =
  t.xt <- t.xt +. x;
  t.yt <- t.yt +. y

let scale t x y =
  t.xx <- t.xx *. x;
  t.xy <- t.xy *. x;
  t.xt <- t.xt *. x;
  t.yx <- t.yx *. y;
  t.yy <- t.yy *. y;
  t.yt <- t.yt *. y

let rotate t angle =
  let a = cos angle and b = sin angle in
  let rx x y = a *. x +. b *. y
  and ry x y = a *. y -. b *. x in
  t.xx <- rx t.xx t.yx;
  t.xy <- rx t.xy t.yy;
  t.xt <- rx t.xt t.yt;
  t.yx <- ry t.xx t.yx;
  t.yy <- ry t.xy t.yy;
  t.yt <- ry t.xt t.yt

let transform t x y =
  t.xx *. x +. t.xy *. y +. t.xt,
  t.yx *. x +. t.yy *. y +. t.yt

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
    let xt = t.xt *. xx +. t.yt *. yx
    and yt = t.xt *. yx +. t.yt *. yy in
    xx, xy, xt, yx ,yy , yt

let invert t =
  let xx, xy, xt, yx, yy, yt = get_invert t in
    {xx = xx; xy = xy; xt = xt; yx = yx; yy = yy; yt = yt}

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
  and xt = next_t.xx *. t.xt +. next_t.xy *. t.yt +. next_t.xt
  and yt = next_t.yx *. t.xt +. next_t.yy *. t.yt +. next_t.yt in
  let r = match result with
      None -> t
    | Some r -> r
  in
  r.xx <- xx; r.xy <- xy; r.xt <- xt;
  r.yx <- yx; r.yy <- yy; r.yt <- yt

let copy t =
  create t.xx t.xy t.xt t.yx t.yy t.yt

let reset_to_id t =
  t.xx <- 1.; t.xy <- 0.; t.xt <- 0.;
  t.yx <- 0.; t.yy <- 1.; t.yt <- 0.

(*Local variables:*)
(*compile-command: "ocamlc -c coordinate.ml && ocamlopt -c coordinate.ml"*)
(*End:*)
