module F (B: Backend.T) =
struct
  let plot_param t f ?(nsamples = 100) a b =
    let step = (b -. a) /. (float nsamples) in
    (*Can be negative; in this way, plotting is done 'in the reverse order'*)
    let x,y = f a in
    B.move_to t x y;
    for i = 0 to nsamples do
      let p = a +. (float i) *. step in
      let x,y = f p in
      B.line_to t x y
    done

  let plot t f = plot_param t (fun t -> t,f t)

  let stroke_plot t f ?(nsamples = 100) a b =
    plot t f ~nsamples a b;
    B.stroke t

  let stroke_plot_param t f ?(nsamples = 100) a b =
    plot_param t f ~nsamples a b;
    B.stroke t

end
(*Local Variables:*)
(*compile-command: "ocamlopt -c functions.ml && ocamlc -c functions.ml"*)
(*End:*)
