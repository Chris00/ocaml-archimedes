open Coord_handler

let plot_param t f ?(nsamples = 100) a b =
  let step = (b -. a) /. (float nsamples) in
  (*Can be negative; in this way, plotting is done 'in the reverse order'*)
  let x,y = f a in
  move_to t x y;
  let bounds_list = [1, a, step, nsamples] in
  let max_length = 1. in
  let rec plot_other i tmin x0 y0 bounds =
    match bounds with
      [] -> ()
    | (prev_stop, prev_tmin, step, samples) :: list ->
        if i <= samples then
          (let p = tmin +. (float i) *. step in
           let x,y = f p in
           let diffx = x -. x0 and diffy = y -. y0 in
           let rel_max =
             let ax0 = abs_float x0
             and ay0 = abs_float y0 in
             max_length *. (ax0 *. ax0 +. ay0 *. ay0 +. step *. step)
           in
           if diffx *. diffx +. diffy *. diffy > rel_max then
             (*increase precision by dividing step by 2.*)
             let ntmin = tmin +. (float (i-1)) *. step in
            (* print_string "DIV -> ";
             print_float (step /. 2.);*)
             plot_other 1 ntmin x0 y0 ((i, tmin, step/.2., 2)::bounds)
           else
             (line_to t x y;
             (* print_int i;*)
              plot_other (i+1) tmin x y bounds)
          )
        else
          (*Plot with current step size finished; return to previous step size.*)
          plot_other (prev_stop + 1) prev_tmin x0 y0 list
  in plot_other 1 a x y bounds_list

let plot t f = plot_param t (fun t -> t,f t)

let stroke_plot ?(init=true) t f ?(nsamples = 100) a b =
  plot t f ~nsamples a b;
  (if init then stroke_init else stroke) t

let stroke_plot_param ?(init=true) t f ?(nsamples = 100) a b =
  plot_param t f ~nsamples a b;
  (if init then stroke_init else stroke) t

type extend =
    NONE
  | PAD
  | REPEAT
  | REFLECT

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
