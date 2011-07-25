open Backend

let is_nan (x: float) = x <> x
let is_inf y = 1. /. y = 0.
let is_finite x = not (is_nan x) && not (is_inf x)

let update_extents e px py =
  let x, w, xupdated =
    if px < e.Matrix.x then px, e.Matrix.w +. (e.Matrix.x -. px), true
    else if px > e.Matrix.x +. e.Matrix.w then e.Matrix.x, px -. e.Matrix.x, true
    else e.Matrix.x, e.Matrix.w, false
  and y, h, yupdated =
    if py < e.Matrix.y then py, e.Matrix.h +. (e.Matrix.y -. py), true
    else if py > e.Matrix.y +. e.Matrix.h then e.Matrix.y, py -. e.Matrix.y, true
    else e.Matrix.y, e.Matrix.h, false
  in
  if xupdated or yupdated then { Matrix.x = x; y = y; w = w; h = h }
  else e

let samplefxy f ?(min_step=1E-9) ?(nsamples = 100) a b =
  let step = (b -. a) /. (float nsamples) in
  (*Can be negative; in this way, plotting is done 'in the reverse order'*)
  let x, y = f a in
  let bounds_list = [(1, a, step, nsamples)] in
  let max_length = 1. in
  let rec next_point i tmin x0 y0 bounds listxy len extents =
    match bounds with
    | [] -> (len, extents, listxy)
    | (prev_stop, prev_tmin, step, samples) :: list ->
        if i <= samples then
          (let p = tmin +. (float i) *. step in
           let x, y = f p in
           if is_nan y then
             (* ignore the point *)
             (* FIXME: this has to include "discontinuity"... what
                about preserving it and take care about it in higher
                levels? Note: if preserving it, be careful on next
                iteration (should always accept the new point). *)
             next_point (i+1) tmin x0 y0 bounds listxy (len+1) extents
           else
             let diffx = x -. x0 and diffy = y -. y0 in
             let rel_max =
               max_length *. (x0 *. x0 +. y0 *. y0 +. (b -. a) *. (b -. a)) in
             if diffx *. diffx +. diffy *. diffy < rel_max || step < min_step then
               let extents = update_extents extents x y in
               next_point (i+1) tmin x y bounds ((x,y)::listxy) (len+1) extents
             else
               (*increase precision by dividing step by 2.*)
               let ntmin = tmin +. (float (i-1)) *. step in
               (* print_string "DIV -> ";
                  print_float (step /. 2.);*)
               next_point 1 ntmin x0 y0 ((i, tmin, step/.2., 2)::bounds)
                 listxy len extents
          )
        else
          (*Plot with current step size finished; return to previous step size.*)
          next_point (prev_stop + 1) prev_tmin x0 y0 list listxy len extents
  in
  let extents = {Matrix.x = x; y = y; w = 0.; h = 0.} in
  next_point 1 a x y bounds_list [x,y] 1 extents

let samplefx ?(xlog=false) ?(ylog=false) ?(min_step=1E-9) ?(max_yrange=1E9)
    ?(nsamples = 100) f a b =
  let step = if xlog then (b /. a) ** (1. /. float nsamples)
  else (b -. a) /. (float nsamples) in
  (*Can be negative; in this way, plotting is done 'in the reverse order'*)
  let bounds_list = [(1, a, step, nsamples)] in
  let max_length = 1. in
  let rec next_point i tmin x0 y0 bounds listxy len extents =
    match bounds with
    | [] -> (len, extents, listxy)
    | (prev_stop, prev_tmin, step, samples) :: list ->
        if i > samples then
          (*Plot with current step size finished; return to previous step size.*)
          next_point (prev_stop + 1) prev_tmin x0 y0 list listxy len extents
        else
          (* FIXME: Numerical error here *)
          let x = if xlog then x0 *. step else x0 +. step in
          let y = f x in
          (* TODO: I'm not okay with that test, check it and document ! *)
          if is_nan y || (ylog && 1. /. y = 0. || not ylog && y > max_yrange) then
            next_point (i+1) tmin x y bounds listxy (len+1) extents
          else
            let diffx = if xlog then log x -. log x0 else x -. x0 in
            let diffy = if ylog then log y -. log y0 else y -. y0 in
            let y0_square = if ylog then log y0 *. log y0 else y0 *. y0 in
            let ab_square = if xlog then (log (b -. a)) ** 2. else b -. a in
            let rel_max = max_length *. (y0_square +. ab_square) in
            if diffx *. diffx +. diffy *. diffy < rel_max
              || step < min_step then
                let ymin, ymax = extents in
                let new_ext = min y ymin, max y ymax in
                let l = (x, y) :: listxy in
                next_point (i+1) tmin x y bounds l (len+1) new_ext
            else
              (*increase precision by dividing step by 2.*)
              let ntmin = if xlog then tmin *. step ** float (i - 1)
              else tmin +. (float (i-1)) *. step in
              let nstep = if xlog then step ** 0.5 else step /. 2. in
              next_point 1 ntmin x0 y0 ((i, tmin, nstep, 2) :: bounds)
                listxy len extents
  in
  let x, y = a, f a in
  next_point 1 a x y bounds_list [(x, y)] 1 (y, y)

let fxy_list f ?min_step ?nsamples a b =
  let _,_, data = samplefxy f ?min_step ?nsamples a b in
  List.fold_left (fun l a -> a::l) [] data

let fx_list f ?min_step ?nsamples a b =
  let _,_, data = samplefx f ?min_step ?nsamples a b in
  List.fold_left (fun l a -> a::l) [] data

let plotfxy t f ?(nsamples = 100) a b =
  let _, _, data = samplefxy f ~nsamples a b in
  List.fold_left (fun () (x,y) -> Backend.line_to t x y) () data

let plotfx t f = plotfxy t (fun t -> t,f t)

let stroke_plot ?(init=true) t f ?(nsamples = 100) a b =
  plotfx t f ~nsamples a b;
  if init then
    let ctm =
      Coordinate.use t (Coordinate.make_root (Matrix.make_identity ())) in
    stroke t;
    Coordinate.restore t ctm
  else stroke t

let stroke_plot_param ?(init=true) t f ?(nsamples = 100) a b =
  plotfxy t f ~nsamples a b;
  if init then
    let ctm =
      Coordinate.use t (Coordinate.make_root (Matrix.make_identity ())) in
    stroke t;
    Coordinate.restore t ctm
  else stroke t



type extend = NONE | PAD | REPEAT | REFLECT

let color_level f ?(extend=PAD) ~xmin ~xmax ~ymin ~ymax fmin cmin fmax cmax =
  let cr, cg, cb, ca = Color.get_rgba cmin
  and cr', cg', cb', ca' = Color.get_rgba cmax in
  let dr = cr' -. cr and dg = cg' -. cg
  and db = cb' -. cb and da = ca' -. ca in
  let make_color fxy =
    let make_in t =
      Color.rgba (cr +. t *. dr) (cg +. t *. dg) (cb +. t *. db) (ca +. t *. da)
    in
    let t = (fxy -. fmin) /. (fmax -. fmin) in
    if t > 0. && t < 1. then make_in t
    else match extend with
      NONE -> Color.rgba 0. 0. 0. 0.
    | PAD ->  if t <= 0. then cmin else cmax
    | REPEAT ->
        let t' = mod_float t 1. in
        let t' = if t' < 0. then t' +. 1. else t' in
        make_in t'
    | REFLECT ->
        let t' = mod_float t 2. in
        let t' = if t' < 0. then t' +. 2. else t' in
        let t' = if t' > 1. then 2. -. t' else t' in
        make_in t'
  in
  fun t u ->
    let x = xmin +. t *. (xmax -. xmin)
    and y = ymin +. u *. (ymax -. ymin) in
    make_color (f x y)
