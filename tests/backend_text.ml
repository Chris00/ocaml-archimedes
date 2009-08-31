open Printf
module Backend = Archimedes.Backend
module A = Archimedes

let half_pi = 2. *. atan 1.
let two_pi = 8. *. half_pi

let () =
  let f s =
  try
    let dirs = [ "./src"; "../src" ] in
    let b = Backend.make s 200. 100. ~dirs in

    (* Change the cairo coordinate system.  This will not affect the text. *)
    if String.sub s 0 5 = "cairo" then
      (Backend.translate b 0. 100.;
       Backend.scale b 1. (-1.));

    Backend.select_font_face b A.Upright A.Normal "times";
    Backend.show_text b ~x:50. ~y:20. ~rotate:half_pi Backend.CC "Joy";

    let pos = [(100., 60., Backend.LT); (140., 60., Backend.CT); (180., 60., Backend.RT);
               (100., 40., Backend.LC); (140., 40., Backend.CC); (180., 40., Backend.RC);
               (100., 20., Backend.LB); (140., 20., Backend.CB); (180., 20., Backend.RB) ] in
    List.iter (fun (x,y,p) ->
                 Backend.set_color b (Archimedes.Color.make 1. 0. 0.);
                 Backend.arc b ~x ~y ~r:1. ~a1:0. ~a2:two_pi;
                 Backend.fill b;
                 Backend.set_color b (Archimedes.Color.make 0. 0. 0.5);
                 Backend.show_text b ~x ~y ~rotate:0. p "Joy";
              ) pos;

    Backend.close b
  with Backend.Error e ->
    eprintf "Backend.Error: %s\n" (Backend.string_of_error e)
  in List.iter f ["cairo PDF backend_test.pdf"; "tikz backend_text.tex"]
