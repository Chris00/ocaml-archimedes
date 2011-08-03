include Tests_common

module A = Archimedes
module Backend = A.Backend
module M = A.Matrix

let description = "Text on viewport, with rotation and placement."

let half_pi = pi /. 2.
let two_pi = 2. *. pi

let draw bk =
  let vp = A.init ~w ~h ~dirs bk in
  A.Viewport.set_font_size vp 35.;
  (try A.Viewport.select_font_face vp Backend.Upright Backend.Normal "arial"
   with _ -> ()); (* keep the default font if it fails *)
  A.Viewport.show_text vp A.Viewport.Device
    ~x:0.25 ~y:0.5 ~rotate:half_pi Backend.CC "Joy";
  A.Viewport.show_text vp A.Viewport.Device
    ~x:0.1 ~y:0.8 ~rotate:0.7 Backend.CC "Joy";

  let pos = [(0.4, 0.8, Backend.LT);
             (0.6, 0.8, Backend.CT);
             (0.8, 0.8, Backend.RT);

             (0.4, 0.5, Backend.LC);
             (0.6, 0.5, Backend.CC);
             (0.8, 0.5, Backend.RC);

             (0.4, 0.2, Backend.LB);
             (0.6, 0.2, Backend.CB);
             (0.8, 0.2, Backend.RB) ]
  in
  List.iter (fun (x,y,p) ->
    A.Viewport.move_to vp x y;
    A.Viewport.arc vp ~r:0.005 ~a1:0. ~a2:two_pi;
    A.Viewport.set_color vp (A.Color.rgb 0. 0. 0.5);
    A.Viewport.show_text vp A.Viewport.Device ~x ~y ~rotate:0. p "Joy";
  ) pos;
  A.Viewport.set_color vp (A.Color.rgb 1. 0. 0.);
  A.Viewport.fill vp A.Viewport.Device;

  A.close vp
