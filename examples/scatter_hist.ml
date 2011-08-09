(* Inspired from
   http://matplotlib.sourceforge.net/examples/pylab_examples/scatter_hist.html
*)

open StdLabels
module A = Archimedes

let two_pi = 8. *. atan 1.

let xy =
  (* Box–Muller transform *)
  let pair _ =
    let u1 = Random.float 1. and u2 = Random.float two_pi in
    let r = sqrt(-2. *. log u1) in
    r *. cos u2, r *. sin u2 in
  Array.init 1000 pair

let bin a b n proj data =
  let h = (b -. a) /. float n in
  let bins = Array.init n (fun i -> a +. 0.5 *. h +. float i *. h) in
  let perc = Array.make n 0. in
  Array.iter data ~f:(fun p ->
    let d = proj p in
    let i = truncate((d -. a) /. h) in
    if 0 <= i && i < n then perc.(i) <- perc.(i) +. 1.;
  );
  let ndata = Array.length data in
  for i = 0 to n - 1 do perc.(i) <- perc.(i) /. float ndata *. 100. done;
  bins, perc

let () =
  let binx, px = bin (-4.) 4. 50 fst xy
  and biny, py = bin (-4.) 4. 50 snd xy in

  let vp = A.init(try A.backend_of_filename Sys.argv.(1) with _ -> []) in
  let vp0 = A.Viewport.make vp 0. 0.75 0. 0.75
  and vpx = A.Viewport.make vp 0. 0.75 0.75 1.
  and vpy = A.Viewport.make vp 0.75 1. 0. 0.75 in
  A.Viewport.sync_range vpx vp0 ~x:true;
  A.Viewport.sync_range vpy vp0 ~y:true;

  A.Axes.box vp0;
  A.set_color vp0 A.Color.blue;
  A.Array.xy_pairs vp0 xy;

  let fillcolor = A.Color.blue in
  A.Axes.box vpx;
  A.Array.xy vpx binx px ~style:(`Bars 0.1) ~fill:true ~fillcolor;

  A.Axes.box vpy;
  A.Array.xy vpy py biny ~style:(`HBars 0.1) ~fill:true ~fillcolor;

  A.close vp
