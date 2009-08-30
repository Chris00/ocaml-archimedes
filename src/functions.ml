open Backend

let samplefxy f ?(min_step=1E-9) ?(nsamples = 100) a b =
  let step = (b -. a) /. (float nsamples) in
  (*Can be negative; in this way, plotting is done 'in the reverse order'*)
  let x,y = f a in
  let bounds_list = [1, a, step, nsamples] in
  let max_length = 1. in
  let rec next_point i tmin x0 y0 bounds listxy len extents =
    match bounds with
      [] -> (len, extents,
             (fun use_sampling init -> List.fold_left use_sampling init listxy))
    | (prev_stop, prev_tmin, step, samples) :: list ->
        if i <= samples then
          (let p = tmin +. (float i) *. step in
           let x,y = f p in
           let diffx = x -. x0 and diffy = y -. y0 in
           let rel_max =
             max_length *. (x0 *. x0 +. y0 *. y0 +. (b-.a) *. (b-.a))
           in
           if diffx *. diffx +. diffy *. diffy < rel_max
             || step < min_step then (
               Backend.Ranges.update extents x y;
               next_point (i+1) tmin x y bounds ((x,y)::listxy) (len+1) extents
             )
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
  let extents = Backend.Ranges.make () in
  Backend.Ranges.update extents x y;
  next_point 1 a x y bounds_list [x,y] 1 extents

let samplefx f ?(min_step=1E-9) ?(nsamples = 100) a b =
  let step = (b -. a) /. (float nsamples) in
  (*Can be negative; in this way, plotting is done 'in the reverse order'*)
  let y = f a in
  let bounds_list = [1, a, step, nsamples] in
  let max_length = 1. in
  let rec next_point i tmin y0 bounds listy len extents =
    match bounds with
      [] -> (len, extents,
             (fun use_sampling init -> List.fold_left use_sampling init listy))
    | (prev_stop, prev_tmin, step, samples) :: list ->
        if i > samples then
          (*Plot with current step size finished; return to previous step size.*)
          next_point (prev_stop + 1) prev_tmin y0 list listy len extents
        else
          let p = tmin +. (float i) *. step in
          let y = f p in
          let diffy = y -. y0 in
          let rel_max =
            max_length *. (y0 *. y0 +. (b-.a) *. (b-.a))
          in
          if step *. step +. diffy *. diffy < rel_max
            || step < min_step then
              let ymin, ymax = extents in
              let new_ext = min y ymin, max y ymax in
              next_point (i+1) tmin y bounds (y::listy) (len+1) new_ext
          else
            (*increase precision by dividing step by 2.*)
            let ntmin = tmin +. (float (i-1)) *. step in
            next_point 1 ntmin y0 ((i, tmin, step/.2., 2)::bounds)
              listy len extents
  in
  next_point 1 a y bounds_list [y] 1 (y,y)

let fxy_list f ?min_step ?nsamples a b =
  let _,_, f = samplefxy f ?min_step ?nsamples a b in
  f (fun l a-> a::l) []

let fx_list f ?min_step ?nsamples a b =
  let _,_, f = samplefx f ?min_step ?nsamples a b in
  f (fun l a -> a::l) []

let plotfxy t f ?(nsamples = 100) a b =
  let _,_, f = samplefxy f ~nsamples a b in
  f (fun () (x,y) -> Backend.line_to t x y) ()

let plotfx t f = plotfxy t (fun t -> t,f t)

let stroke_plot ?(init=true) t f ?(nsamples = 100) a b =
  plotfx t f ~nsamples a b;
  if init then
    let ctm = Coordinate.use t (Coordinate.make_identity ()) in
    stroke t;
    Coordinate.restore t ctm
  else stroke t

let stroke_plot_param ?(init=true) t f ?(nsamples = 100) a b =
  plotfxy t f ~nsamples a b;
  if init then
    let ctm = Coordinate.use t (Coordinate.make_identity ()) in
    stroke t;
    Coordinate.restore t ctm
  else stroke t



type extend = NONE | PAD | REPEAT | REFLECT

let color_level f ?(extend=PAD) ~xmin ~xmax ~ymin ~ymax fmin cmin fmax cmax =
  let cr, cg, cb, ca = Color.get_rgba cmin
  and cr', cg', cb', ca' = Color.get_rgba cmax in
  let conv a b t = a +. t *. (b -. a) in
  let make_color fxy =
    let make_in t = Color.make ~a:(conv ca ca' t)
      (conv cr cr' t) (conv cg cg' t) (conv cb cb' t)
    in
    let t = (fxy -. fmin) /. (fmax -. fmin) in
    if t > 0. && t < 1. then make_in t
    else match extend with
      NONE -> Color.make ~a:0. 0. 0. 0.
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
    let x = conv xmin xmax t and y = conv ymin ymax u in
    make_color (f x y)



(*Local Variables:*)
(*compile-command: "ocamlopt -c functions.ml && ocamlc -c functions.ml"*)
(*End:*)
