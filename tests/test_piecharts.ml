include Testing

module V = Viewport
module PC = Piechart

let draw bk =
  let vp = V.init ~w ~h ~dirs bk in
  let vps = V.layout_columns vp 2 in
  let vp1 = vps.(0) and vp2 = vps.(1) in

  let data1 = [("first", 42); ("second", 42); ("third", 126); ("crew", 180)] in
  let data2 = [("male", 42); ("female", 13)] in
  let data1 = List.map (fun (x, y) -> (x, float y)) data1 in
  let data2 = List.map (fun (x, y) -> (x, float y)) data2 in

  (* TODO add titles *)
  PC.simple vp1 data1;
  PC.simple vp2 data2;

  V.close vp
