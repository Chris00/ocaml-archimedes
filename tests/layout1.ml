include Tests_common

module V = Archimedes.Viewport
module P = Archimedes.Path
module Color = Archimedes.Color

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let subvp = V.grid vp 2 2 in
  let subsubvp = V.grid subvp.(1).(1) 2 2 in
  let subsubsubvp = V.grid subsubvp.(1).(1) 2 2 in
  let colors = [| Color.red; Color.blue; Color.green; Color.chocolate |] in
  let trace i vp =
    V.set_rel_line_width vp 1.;

    let p = Archimedes.Path.make() in
    Archimedes.Path.rectangle p ~x:0. ~y:0. ~w:1. ~h:1.;
    V.set_color vp colors.(i mod 4);
    V.stroke vp `Graph p;

    V.set_line_width vp 1.;

    V.set_color vp colors.((i + 2) mod 4);
    V.stroke vp `Orthonormal p;

    V.set_color vp colors.((i + 1) mod 4);
    let p = Archimedes.Path.make () in
    Archimedes.Path.move_to p ~x:0. ~y:0.;
    Archimedes.Path.line_to p ~x:1. ~y:0.;
    Archimedes.Path.line_to p ~x:1. ~y:1.;
    Archimedes.Path.line_to p ~x:0. ~y:1.;
    Archimedes.Path.line_to p ~x:0. ~y:0.;
    V.stroke vp `Device p
  in
  let vps = List.map Array.to_list [subvp; subsubvp; subsubsubvp] in
  Array.iteri trace (Array.concat(List.concat vps));

  Archimedes.close subvp.(0).(0);

  let p = Archimedes.Path.make () in
  Archimedes.Path.move_to p 0.3 0.;
  Archimedes.Path.line_to p 0.3 1.;
  V.set_color vp Color.gold;
  V.stroke vp `Device p;

  V.set_color vp Color.red;
  V.set_rel_line_width vp 1.;
  let p = Archimedes.Path.make () in
  Archimedes.Path.rectangle p ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke vp `Graph p;

  Archimedes.close vp
