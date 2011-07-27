include Testing

module V = Archimedes.Viewport
module PC = Archimedes.Piechart

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let vps = V.columns vp 2 in

  let data1 =
    [("first", 42.); ("second", 42.); ("third", 126.); ("crew", 180.)] in
  let data2 = [("male", 42.); ("female", 13.)] in

  (* TODO add titles *)
  PC.simple vps.(0) data1;
  PC.simple vps.(1) data2;

  Archimedes.close vp
