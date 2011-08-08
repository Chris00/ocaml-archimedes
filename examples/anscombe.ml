(* Example inspired by
   http://matplotlib.sourceforge.net/examples/pylab_examples/anscombe.html

   This is an example from Anscombe to show 4 datasets of x and y that
   have the same mean, standard deviation, and regression line, but which
   are qualitatively different. *)

module A = Archimedes

let x  = [|10.; 8.; 13.; 9.; 11.; 14.; 6.; 4.; 12.; 7.; 5.|]
let y1 = [|8.04; 6.95; 7.58; 8.81; 8.33; 9.96; 7.24; 4.26; 10.84; 4.82; 5.68|]
let y2 = [|9.14; 8.14; 8.74; 8.77; 9.26; 8.10; 6.13; 3.10; 9.13; 7.26; 4.74|]
let y3 = [|7.46; 6.77; 12.74; 7.11; 7.81; 8.84; 6.08; 5.39; 8.15; 6.42; 5.73|]
let x4 = [|8.; 8.; 8.; 8.; 8.; 8.; 8.; 19.; 8.; 8.; 8.|]
let y4 = [|6.58;5.76;7.71;8.84;8.47;7.04;5.25;12.50;5.56;7.91;6.89|]

let fit x = 3. +. 0.5 *. x

let () =
  let vp0 = A.init(try A.backend_of_filename Sys.argv.(1) with _ -> []) in
  let vp = A.Viewport.grid vp0 2 2 in

  Array.iter (fun vpl -> Array.iter (fun v ->
    A.Axes.box v;
    A.Viewport.xrange v 0. 20.;
    A.Viewport.yrange v 2. 14.;
    A.set_color v A.Color.red;
    A.fx v fit 2. 19.;
    A.set_color v A.Color.black;
    A.Viewport.set_font_size v 20.;
  ) vpl) vp;

  A.Array.xy vp.(0).(1) x y1;
  A.Viewport.text vp.(0).(1) 3. 12. "I";

  A.Array.xy vp.(1).(1) x y2;
  A.Viewport.text vp.(1).(1) 3. 12. "II";

  A.Array.xy vp.(0).(0) x y3;
  A.Viewport.text vp.(0).(0) 3. 12. "III";

  A.Array.xy vp.(1).(0) x4 y4;
  A.Viewport.text vp.(1).(0) 3. 12. "IV";

  A.close vp0
