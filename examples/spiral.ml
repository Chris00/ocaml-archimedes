open Archimedes
module P = Plot.Array

let () =
  let p = P.make "graphics hold" 600. 600. in

  P.xyf p (fun t -> let r = 0.1 *. t in
           (r *. cos t, r *. sin t)) 0. 13.;

  P.close p
