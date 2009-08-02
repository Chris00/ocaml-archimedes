open Printf
module B = Archimedes.Backend

let half_pi = 2. *. atan 1.
let two_pi = 8. *. half_pi

let () =
  try
    let dirs = [ "./src"; "../src" ] in
    let b = B.make "cairo PDF backend_test.pdf" 200. 100. ~dirs in

    (* Chenge the coordinate system.  This will not affect the text. *)
    B.translate b 0. 100.;
    B.scale b 1. (-1.);

    B.select_font_face b B.Upright B.Normal "times";
    B.show_text b ~x:50. ~y:20. ~rotate:half_pi B.CC "Joy";

    let pos = [(100., 60., B.LT); (140., 60., B.CT); (180., 60., B.RT);
               (100., 40., B.LC); (140., 40., B.CC); (180., 40., B.RC);
               (100., 20., B.LB); (140., 20., B.CB); (180., 20., B.RB) ] in
    List.iter (fun (x,y,p) ->
                 B.set_color b (Archimedes.Color.make 1. 0. 0.);
                 B.arc b ~x ~y ~r:1. ~a1:0. ~a2:two_pi;
                 B.fill b;
                 B.set_color b (Archimedes.Color.make 0. 0. 0.5);
                 B.show_text b ~x ~y ~rotate:0. p "Joy";
              ) pos;

    B.close b
  with B.Error e ->
    eprintf "Backend.Error: %s\n" (B.string_of_error e)

