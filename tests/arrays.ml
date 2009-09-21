open Bigarray
open Archimedes
module P = Plot.Array

let draw backend =
  (* The directories are only added so it is not necessary to install
     the software to execute this test.  This should be seldom needed. *)
  let p = P.make backend 800. 700. ~dirs:[ "../src"; "./src"] in
  let vp = P.Viewport.rows p 2 in

  (* P.set_mark_size p 1.; *)
  P.viewport vp.(1);
  let x = [| 1.; 2.; 3.; -1.; 0. |] in
  P.x p x ~color:Color.red ~mark:"+";

  P.viewport vp.(0);
  let x = Array1.create float64 fortran_layout 20 in
  for i = 1 to Array1.dim x do
    x.{i} <- 3. *. sin(float i *. 0.1);
  done;
  Plot.Fortran.x p x;

  P.close p
;;

let () =
  let bk =
    if Array.length Sys.argv > 1 then [Sys.argv.(1)]
    else [ "tikz arrays.tex";
           "graphics hold";
           "cairo PNG arrays.png";
           "cairo PDF arrays.pdf" ]
  in
  try List.iter draw bk
  with Backend.Error e ->
    print_endline (Backend.string_of_error e);
    exit 1



(* Local Variables: *)
(* compile-command: "make -kB arrays.exe" *)
(* End: *)
