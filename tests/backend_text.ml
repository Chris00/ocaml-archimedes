open Testing
module Backend = Archimedes.Backend

let half_pi = pi /. 2.
let two_pi = 2. *. pi

let draw bk =
  let b = Backend.make bk 200. 100. ~dirs in
  (try Backend.select_font_face b Backend.Upright Backend.Normal "times"
   with _ -> ()); (* keep the default font if it fails *)
  Backend.show_text b ~x:50. ~y:20. ~rotate:half_pi Backend.CC "Joy";

  let pos = [(100., 60., Backend.LT);
             (140., 60., Backend.CT);
             (180., 60., Backend.RT);

             (100., 40., Backend.LC);
             (140., 40., Backend.CC);
             (180., 40., Backend.RC);

             (100., 20., Backend.LB);
             (140., 20., Backend.CB);
             (180., 20., Backend.RB) ]
  in
  List.iter (fun (x,y,p) ->
               Backend.set_color b (Archimedes.Color.rgb 1. 0. 0.);
               Backend.move_to b x y;
               Backend.arc b ~r:1. ~a1:0. ~a2:two_pi;
               Backend.fill b;
               Backend.set_color b (Archimedes.Color.rgb 0. 0. 0.5);
               Backend.show_text b ~x ~y ~rotate:0. p "Joy";
            ) pos;

  Backend.close b
