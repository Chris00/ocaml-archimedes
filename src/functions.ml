open Layer

let plot_param t f ?(nsamples = 100) a b =
  let step = (b -. a) /. (float nsamples) in
  (*Can be negative; in this way, plotting is done 'in the reverse order'*)
  let x,y = f a in
  move_to t x y;
  for i = 0 to nsamples do
    let p = a +. (float i) *. step in
    let x,y = f p in
    line_to t x y
  done

let plot t f = plot_param t (fun t -> t,f t)

let stroke_plot t f ?(nsamples = 100) a b =
  plot t f ~nsamples a b;
  stroke t

let stroke_plot_param t f ?(nsamples = 100) a b =
  plot_param t f ~nsamples a b;
  stroke t

(*Local Variables:*)
(*compile-command: "ocamlopt -c functions.ml && ocamlc -c functions.ml"*)
(*End:*)
