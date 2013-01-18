module A = Archimedes

let pi = 4. *. atan 1.

let () =
  let vp = A.init(try A.backend_of_filename Sys.argv.(1) with _ -> []) in
  let n = 30 in
  A.Axes.box vp;
  for i = 0 to n do
    let i = float i /. (float (n + 1)) in
    A.set_color vp (A.Color.hue (240. *. i));
    let a = i *. 10. in
    A.fx vp (fun x -> x -. a *. sin x) 0. (3. *. pi)
  done;
  A.close vp
