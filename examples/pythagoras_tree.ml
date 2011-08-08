(* Based on http://www.ffconsultancy.com/products/fsharp_for_visualization/demo6.html *)

module A = Archimedes
module M = A.Matrix

let half_pi = 2. *. atan 1.
let ( |> ) m f = f m; m   (* left assoc *)


let branches m =
  let m1 = (M.copy m) |> M.translate ~x:0. ~y:1.
    |> M.scale ~x:(4. /. 5.) ~y:(4. /. 5.)
    |> M.rotate ~angle:(half_pi -. asin(4. /. 5.))
  and m2 = (M.copy m) |> M.translate ~x:1. ~y:1.
    |> M.scale ~x:(3. /. 5.) ~y:(3. /. 5.)
    |> M.rotate ~angle:(-. half_pi +. asin(3. /. 5.))
    |> M.translate ~x:(-1.) ~y:0. in
  [m1; m2]

let square =
  let p = A.Path.make() in A.Path.rectangle p ~x:0. ~y:0. ~w:1. ~h:1.; p

let shape vp color m =
  let p = A.Path.transform m square in
  A.Viewport.set_color vp color;
  A.Viewport.fill vp ~path:p `Data;
  A.Viewport.set_color vp A.Color.dark_green;
  A.Viewport.stroke vp ~path:p `Data

let trunks vp color ms = List.iter (shape vp color) ms

let rec tree vp n ms =
  if n=0 then trunks vp A.Color.green ms else (
    trunks vp A.Color.burlywood ms;
    tree vp (n-1) (List.concat(List.map branches ms))
  )

let () =
  let bk = A.backend_of_filename(try Sys.argv.(1) with _ -> "") in
  let vp = A.init ~w:500. ~h:500. bk in
  A.Axes.box vp;
  A.Viewport.xrange vp (-2.) 2.;
  A.Viewport.yrange vp 0. 4.;
  for n = 1 to 10 do
    tree vp n [M.make_identity()]
  done;
  A.close vp
