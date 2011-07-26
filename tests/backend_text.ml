include Testing
module Backend = Archimedes.Backend

let half_pi = pi /. 2.
let two_pi = 2. *. pi

let draw bk =
  let b = Backend.make bk 430. 220. ~dirs in
  Backend.set_font_size b 35.;
  (try Backend.select_font_face b Backend.Upright Backend.Normal "arial"
   with _ -> ()); (* keep the default font if it fails *)
  Backend.show_text b ~x:100. ~y:100. ~rotate:half_pi Backend.CC "Joy";
  Backend.show_text b ~x:50. ~y:150. ~rotate:0.7 Backend.CC "Joy";

  let pos = [(200., 160., Backend.LT);
             (280., 160., Backend.CT);
             (360., 160., Backend.RT);

             (200., 100., Backend.LC);
             (280., 100., Backend.CC);
             (360., 100., Backend.RC);

             (200., 40., Backend.LB);
             (280., 40., Backend.CB);
             (360., 40., Backend.RB) ]
  in
  List.iter (fun (x,y,p) ->
               Backend.set_color b (Archimedes.Color.rgb 1. 0. 0.);
               Backend.move_to b x y;
               Backend.arc b ~r:2. ~a1:0. ~a2:two_pi;
               Backend.fill b;
               Backend.set_color b (Archimedes.Color.rgb 0. 0. 0.5);
               Backend.show_text b ~x ~y ~rotate:0. p "Joy";
            ) pos;

  Backend.close b
