open Archimedes
module P = Plot.Generic

let draw backend =
  (* The directories are only added so it is not necessary to install
     the software to execute this test.  This should be seldom needed. *)
  let p = P.make backend 800. 800. ~dirs:[ "../src"; "./src"] in
  P.f p (fun x -> x *. sin(1. /. x)) (-3.) 3. ~nsamples:500;
  P.close p


let () =
  let bk =
    if Array.length Sys.argv > 1 then [Sys.argv.(1)]
    else [ "tikz simple_plot.tex";
           "graphics hold";
           "cairo PNG simple_plot.png";
           "cairo PDF simple_plot.pdf" ]
  in
  try List.iter draw bk
  with Backend.Error e ->
    print_endline (Backend.string_of_error e);
    exit 1



(* Local Variables: *)
(* compile-command: "make -kB simple_plot.exe" *)
(* End: *)

