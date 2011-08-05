open Tests_common
module A = Archimedes
module V = A.Viewport

let description = "Show all implemented marks."

let markers = Array.of_list (Archimedes.Pointstyle.names ())

let draw backend =
  let vp = A.init ~w ~h ~dirs backend in
  V.xrange vp 0. 7.;
  V.yrange vp 0. 3.2;
  V.set_mark_size vp 20.;
  V.set_line_width vp 1.;
  V.set_font_size vp 17.;
  for i = 0 to Array.length markers - 1 do
    let x = float (i mod 8) and y = float (i / 8) in
    V.set_color vp A.Color.black;
    V.mark vp x y markers.(i);
    V.set_color vp A.Color.red;
    V.show_text vp V.Data x (y +. 0.2) A.Backend.CT markers.(i);
  done;
  A.close vp
