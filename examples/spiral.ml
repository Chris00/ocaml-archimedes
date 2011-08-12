module A = Archimedes

let () =
  let p = A.init [] ~w:600. ~h:600. in
  A.Axes.box p;
  A.set_color p A.Color.magenta;
  A.xyf p (fun t -> let r = 0.1 *. t in
                 (r *. cos t, r *. sin t)) 0. 13. ~n:200;

  A.close p
