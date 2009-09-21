open Archimedes
module P = Plot.Array

let () =
  let a = -6.
  and b = 6. in
  let p = P.make "graphics hold" 600. 600. in
  P.f p cos a b;
  P.set_color p Color.green;
  P.f p (fun x -> 1. -. x**2. /. 2.) a b;
  P.set_color p Color.blue;
  P.f p (fun x -> 1. -. x**2. /. 2. +. x**4. /. 24.) a b;

  P.close p
