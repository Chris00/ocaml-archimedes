include Testing
module Backend = Archimedes.Backend

let draw bk =
  let bk = Backend.make bk 800. 700. ~dirs in
  Backend.scale bk 1.5 1.5;

  Backend.move_to bk 500. 300.;
  Backend.arc bk 150. 0. (2. *. pi);
  Backend.stroke bk;

  (* Black disk *)
  Backend.move_to bk 400. 300.;
  Backend.arc bk 100. 0. (1.5 *. pi);
  Backend.fill bk;

  Backend.set_color bk (Archimedes.Color.rgb 0.5 0. 0.5);
  Backend.move_to bk 20. 430.;
  Backend.curve_to bk 30. 480. 100. 450. 130. 430.;
  Backend.line_to bk 50. 440.;
  Backend.stroke bk;
  Backend.move_to bk 20. 400.;
  Backend.curve_to bk 30. 450. 100. 420. 130. 400.;
  Backend.line_to bk 50. 380.;
  Backend.fill bk;
  Backend.move_to bk 20. 370.;
  Backend.line_to bk 100. 370.;
  Backend.line_to bk 50. 350.;
  Backend.fill bk;

  Backend.save bk;
  begin
    Backend.set_color bk (Archimedes.Color.rgb 0.9 0. 0.2);
    Backend.move_to bk 450. 400.;
    Backend.scale bk 3. 1.;
    Backend.arc bk 50. 0. (2. *. pi);
    Backend.stroke bk;
    Backend.set_color bk (Archimedes.Color.rgba 0.9 0. 0. 0.5);
    Backend.move_to bk 150. 400.;
    (* Backend.arc bk 20. 0. (2. *. pi); *)
    Backend.curve_to bk 150. 480.  80. 420.  50. 400.;
    Backend.fill bk;
  end;
  Backend.restore bk;

  (* Filling a self crossing path. *)
  Backend.move_to bk 50. 100.;
  Backend.curve_to bk 50. 150.  70. 150.  100. 100.;
  Backend.curve_to bk 120. 30.  150. 20.  200. 100.;
  Backend.fill bk;

  (* Path made of several subpaths *)
  Backend.set_color bk (Archimedes.Color.rgb 0. 0. 0.7);
  Backend.move_to bk 30. 300.;
  Backend.line_to bk 100. 280.;
  Backend.line_to bk 100. 320.;
  Backend.rectangle bk 30. 220. 100. 10.;
  Backend.line_to bk 120. 270.;
  Backend.line_to bk 140. 250.;
  Backend.rectangle bk 30. 200. 100. 10.; (* subpath in itself *)
  Backend.fill bk;

  (* Paths and save/restore *)
  Backend.set_color bk (Archimedes.Color.rgb 0. 0.8 0.);
  Backend.move_to bk 220. 100.;
  Backend.line_to bk 300. 150.;
  Backend.save bk;
  (* This [line_to] is taken into account even though is it in a
     save/restore group. *)
  Backend.line_to bk 350. 150.;
  Backend.restore bk;
  Backend.save bk;
  begin
    Backend.line_to bk 300. 50.;
    Backend.stroke_preserve bk;
    Backend.move_to bk 250. 100.;
  end;
  Backend.restore bk;
  Backend.line_to bk 220. 50.;
  Backend.stroke bk;

  Backend.close bk
