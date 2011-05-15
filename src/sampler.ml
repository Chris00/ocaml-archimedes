type strategy = float -> float -> float
type criterion = float -> float -> float -> float -> float -> float -> bool

let strategy_midpoint t1 t2 =
  (t1 +. t2) /. 2.

let strategy_random t1 t2 =
  Random.self_init ();
  t1 +. Random.float (t2 -. t1)

let strategy_center_random t1 t2 =
  let mid = strategy_midpoint t1 t2 in
  let interval = (t2 -. t1) /. 20. in
  strategy_random (mid -. interval) (mid +. interval)

let criterion_none _ _ _ _ _ _ = true

let criterion_angle ?(threshold=3.1) x1 y1 xm ym x2 y2 =
  let sqdist x1 y1 x2 y2 =
    (x2 -. x1) *. (x2 -. x1) +. (y2 -. y1) *. (y2 -. y1)
  in
  let side1 = sqdist x1 y1 xm ym in
  let side2 = sqdist xm ym x2 y2 in
  let side_hyp = sqdist x1 y1 x2 y2 in
  acos ((side1 +. side2 -. side_hyp)
        /. (2. *. sqrt side1 *. sqrt side2)) > threshold

let criterion_angle_log xlog ylog ?(threshold=175.) x1 y1 xm ym x2 y2 =
  let id x = x in
  let tx = if xlog then log10 else id
  and ty = if ylog then log10 else id in
  let sqdist x1 y1 x2 y2 =
    tx (x2 -. x1) *. tx (x2 -. x1) +. ty (y2 -. y1) *. ty (y2 -. y1)
  in
  let side1 = sqdist x1 y1 xm ym in
  let side2 = sqdist xm ym x2 y2 in
  let side_hyp = sqdist x1 y1 x2 y2 in
  acos ((side1 +. side2 -. side_hyp)
        /. (2. *. sqrt side1 *. sqrt side2)) > threshold

let rec refine min_step strategy criterion xylist f t1 x1 y1 t2 x2 y2 =
  if abs_float (t1 -. t2) < min_step then xylist
  else
    let m = strategy t1 t2 in
    let xm, ym = f m in
    if criterion x1 y1 xm ym x2 y2 then xylist
    else
      let r = refine min_step strategy criterion in
      let xylist = r xylist f t1 x1 y1 m xm ym in
      let xylist = (xm, ym) :: xylist in
      let xylist = r xylist f m xm ym t2 x2 y2 in
      xylist

let next tlog t1 t2 nintervals =
  if tlog then t1 *. (t2 /. t1) ** (1. /. nintervals)
  else t1 +. (t2 -. t1) /. nintervals

let samplefxy ?(tlog=false) ?(min_step=1E-9) ?(nsamples=100)
    ?(strategy=strategy_midpoint) ?(criterion=criterion_none) f t1 t2 =
  let rec aux nsamples t1 x1 y1 t2 x2 y2 xylist =
    if nsamples < 3 then List.rev ((x2, y2) :: (x1, y1) :: xylist)
    else
      let m = next tlog t1 t2 (float (nsamples - 1)) in
      let xm, ym = f m in
      let xylist = (xm, ym) :: xylist in
      let pts =
        refine min_step strategy criterion xylist f t1 x1 y1 m xm ym in
      aux (nsamples - 1) m xm ym t2 x2 y2 pts
  in
  let x1, y1 = f t1
  and x2, y2 = f t2 in
  aux nsamples t1 x1 y1 t2 x2 y2 []
