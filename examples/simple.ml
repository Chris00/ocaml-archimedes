
module P = Archimedes.Plot.Array

let two_pi = 8. *. atan 1.

let () =
  let p = P.make "graphics hold" 600. 600. in
  P.f p sin 0. two_pi;
  P.close p

