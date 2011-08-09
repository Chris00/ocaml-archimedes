(* Inspired by
   http://matplotlib.sourceforge.net/examples/pylab_examples/coords_report.html
*)

module A = Archimedes

let x = Array.init 20 (fun _ -> Random.float 1.)
let y = Array.init 20 (fun _ -> Random.float 1.e7)

let () =
  let vp = A.init(try A.backend_of_filename Sys.argv.(1) with _ -> []) in
  A.Axes.box vp;
  A.Array.xy vp x y;
  A.close vp
