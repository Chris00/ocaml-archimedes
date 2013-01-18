include Tests_common

module V = Archimedes.Viewport
module PC = Archimedes.Piechart
module C = Archimedes.Color

let pi = 4. *. atan 1.

let draw bk =
  let vp = Archimedes.init ~w ~h ~dirs bk in
  let vps = V.grid vp 2 2 in

  let data1 =
    [("first", 42.); ("second", 42.); ("third", 126.); ("crew", 180.)] in
  let data2 = [("male", 42.); ("female", 13.)] in
  let data3 = [
    {PC.name = "male"; PC.value = 42.; PC.children = [
      {PC.name = "< 18"; PC.value = 14.; PC.children = []};
      {PC.name = "18 -> 65"; PC.value = 22.; PC.children = []};
      {PC.name = "> 65"; PC.value = 6.; PC.children = []}]};
    {PC.name = "female"; PC.value = 28.; PC.children = [
      {PC.name = "< 18"; PC.value = 2.; PC.children = []};
      {PC.name = "18 -> 65"; PC.value = 23.; PC.children = []};
      {PC.name = "> 65"; PC.value = 3.; PC.children = []}]}
  ] in

  (* TODO add titles *)
  let customscheme = PC.CustomColors [
    "first", C.blue;
    "second", C.red;
    "third", C.blue;
    "crew", C.orange
  ] in
  PC.simple ~style:(PC.Highlight ["third"]) ~colorscheme:customscheme
    ~keyplacement:(PC.Selective (0.5 *. pi)) vps.(0).(0) data1;
  PC.simple ~keylabels:PC.WithPercents vps.(0).(1) data2;
  PC.multilevel vps.(1).(0) data3;
  PC.simple ~keyplacement:PC.OverPie ~keylabels:PC.Label vps.(1).(1) data2;

  Archimedes.close vp
