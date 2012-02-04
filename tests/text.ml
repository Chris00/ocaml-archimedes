include Tests_common

module A = Archimedes
module Backend = A.Backend
module M = A.Matrix

let description = "Text on viewport, with rotation and placement."

let half_pi = pi /. 2.
let two_pi = 2. *. pi

let disk vp x y r =
  let c = A.Viewport.get_color vp in
  A.set_color vp A.Color.red;
  let p = A.Path.make() in
  A.Path.move_to p x y;
  A.Path.arc p ~r ~a1:0. ~a2:two_pi;
  A.Viewport.fill vp `Device p;
  A.set_color vp c

let draw bk =
  let vp = A.init ~w ~h ~dirs bk in
  A.Viewport.set_font_size vp 35.;
  (try A.Viewport.select_font_face vp Backend.Upright Backend.Normal "arial"
   with _ -> ()); (* keep the default font if it fails *)
  A.Viewport.text vp 0.25 0.5 "Joy" ~coord:`Device ~rotate:half_pi
    ~pos:Backend.RC;
  disk vp 0.25 0.5 0.005;
  A.Viewport.text vp 0.1  0.8 "Joy" ~coord:`Device ~rotate:0.7;
  disk vp 0.1 0.8 0.005;

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
  List.iter (fun (x,y,pos) ->
    disk vp x y 0.005;
    A.set_color vp (A.Color.rgb 0. 0. 0.5);
    A.Viewport.text vp x y "Joy" ~coord:`Device ~pos;
  ) pos;

  A.close vp
