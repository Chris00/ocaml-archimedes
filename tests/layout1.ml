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

    V.set_color vp colors.(i mod 4);
    V.rectangle vp ~x:0. ~y:0. ~w:1. ~h:1.;
    V.stroke vp V.Graph;

    V.set_line_width vp 1.;

    V.set_color vp colors.((i + 2) mod 4);
    V.rectangle vp ~x:0. ~y:0. ~w:1. ~h:1.;
    V.stroke vp V.Orthonormal;

    V.set_color vp colors.((i + 1) mod 4);
    V.move_to vp ~x:0. ~y:0.;
    V.line_to vp ~x:1. ~y:0.;
    V.line_to vp ~x:1. ~y:1.;
    V.line_to vp ~x:0. ~y:1.;
    V.line_to vp ~x:0. ~y:0.;
    V.stroke vp V.Device
  in
  let vps = List.map Array.to_list [subvp; subsubvp; subsubsubvp] in
  Array.iteri trace (Array.concat(List.concat vps));

  Archimedes.close subvp.(0).(0);

  V.move_to vp 0.3 0.;
  V.line_to vp 0.3 1.;
  V.set_color vp Color.gold;
  V.stroke vp V.Device;

  V.set_color vp Color.red;
  V.set_rel_line_width vp 1.;
  V.rectangle vp ~x:0. ~y:0. ~w:1. ~h:1.;
  V.stroke vp V.Graph;

  Archimedes.close vp
