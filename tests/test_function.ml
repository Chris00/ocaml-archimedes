open Archimedes
module P = Plot.Generic

let draw backend =
  (* The directories are only added so it is not necessary to install
     the software to execute this test.  This should be seldom needed. *)
  let p = P.make backend 800. 800. ~dirs:[ "../src"; "./src"] in
  let vp = P.Viewport.grid p 2 2 in

  P.viewport vp.(0).(0);
  P.f p (fun x -> x *. x) (-3.) 3. ~color:Color.blue;

  P.viewport vp.(1).(0);
  P.set_color p Color.red;
(*  let finish handle =
    H.line_to handle 3. 10.;
    H.line_to handle (-3.) 10.;
    H.close_path handle;
    H.fill handle
  in*)
  P.f p (fun x -> x *. x) (-3.) 3.;

  P.viewport vp.(0).(1);
  P.set_color p Color.green;
(*  let finish handle =
    H.line_to handle 3. (-1.);
    H.line_to handle (-3.) (-1.);
    H.close_path handle;
    H.stroke handle
  in
*)  P.f p (fun x -> x *. (x *. x -. 3.)) (-3.) 3.;

  P.viewport vp.(1).(1);
(*  let xaxis =
    H.make_xaxis (`P "|") `Number CB (`P "tic_up") (`Linear(7,0))
  in
  let yaxis =
    H.make_yaxis (`P "-") `Number LC (`P "tic_left") (`Linear(7,2))
  in
  let axes = H.make_axes (`Rectangle(true,true)) xaxis yaxis in
  ignore
    (H.print_axes handle axes {A.x1 = -3.;x2 = 3.;y1=0.;y2 = 9.});
*)
  P.f p (fun x -> x *. x -. 2.) (-3.) 3.;
  P.close p
;;

let () =
  let bk =
    if Array.length Sys.argv > 1 then [Sys.argv.(1)]
    else [ "tikz functions.tex";
           "graphics";
           "cairo PNG functions.png";
           "cairo PDF functions.pdf" ]
  in
  try List.iter draw bk
  with Backend.Error e ->
    print_endline (Backend.string_of_error e);
    exit 1



(* Local Variables: *)
(* compile-command: "make -kB test_function.exe" *)
(* End: *)
