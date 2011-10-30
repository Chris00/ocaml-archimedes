open Tests_common
module A = Archimedes

let description = "Functions plotted on a range where they are not defined."

let draw bk =
  let vp = A.init ~w ~h ~dirs bk in
  A.Axes.box vp;
  A.Viewport.set_mark_size vp 5.;
  A.Viewport.set_line_width vp 1.;
  (* NOTE: when fill:true, with some value of nsamples, the fill has a bad
     point. This will be fixed with the new functions fill implementation
     (WIP). *)
  A.Viewport.set_color vp A.Color.royal_blue;
  A.fx vp sqrt (-10.) 10. ~style:(`Linesmarkers "S");
  A.Viewport.set_color vp A.Color.indian_red;
  A.fx vp (fun x -> 0.3 *. sqrt(x *. x -. 4.)) (-10.) 10.
  ~style:(`Linesmarkers "o") ~fill:true;
  A.close vp
