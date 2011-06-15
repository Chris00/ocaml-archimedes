open Archimedes
module P = Plot.Generic

let draw backend =
  (* The directories are only added so it is not necessary to install
     the software to execute this test.  This should be seldom needed. *)
  let p = P.make backend 800. 800. ~dirs:[ "../src"; "./src"] in
  let vp = P.Viewport.grid p 2 2 in

  P.set_mark_size p 3.;
  P.viewport vp.(0).(0);
  P.f p (fun x -> x *. x) (-3.) 3. ~color:Color.blue ~mark:"+" ~nsamples:20;

  P.viewport vp.(1).(0);
  P.f p (fun x -> x *. (x *. x -. 3.)) (-3.) 3. ~color:Color.red ~fill:true;

  P.viewport vp.(0).(1);
  P.set_color p (Color.rgb 0. 0.5 0.);
  P.xyf p (fun t -> (sin t, sin(2. *. t))) 0. 6.5 ~fill:true;

  (* P.viewport vp.(1).(1); *)
  (* P.f p (fun x -> sin x +. 1.) (-3.) 3.; *)
  P.close p
;;

let () =
  let bk =
    if Array.length Sys.argv > 1 then [Sys.argv.(1)]
    else [ "tikz functions.tex";
           "graphics hold";
           "cairo PNG functions.png";
           "cairo PDF functions.pdf" ]
  in
  try List.iter draw bk
  with Backend.Error e ->
    print_endline (Backend.string_of_error e);
    exit 1



(* Local Variables: *)
(* compile-command: "make -kB functions.exe" *)
(* End: *)
