module A = Archimedes

let () =
  print_endline
    ("Draw fast incrementally on screen.\n"^
     "There should be no flickering, without use of double-buffering !");
  let vp = A.init [] in
  A.Axes.box vp;
  A.xrange vp 0. 1.;
  A.yrange vp 0. 1.;
  A.Viewport.show vp;
  Random.self_init ();
  let path = A.Path.make () in
  while true do
    A.Path.clear path;
    A.Path.move_to path (Random.float 1.) (Random.float 1.);
    A.Path.line_to path (Random.float 1.) (Random.float 1.);
    A.Viewport.stroke vp `Data path;
    A.Viewport.show vp
  done;
  (* Never gets here *)
  A.close vp
