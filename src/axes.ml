(**Styles definitions to make axes*)

type data =
    Graph of int * int

type ticstyle = (*FIXME: pointstyle??*)
    Line of float

type mode =
    Rectangle of ticstyle * ticstyle
  | Two_lines of float * float * ticstyle * ticstyle

(*
type t =
    {xmin:float;
     xmax:float;
     ymin:float;
     ymax:float;
     datax:data;
     datay:data;
     mode:mode
    }

let make ~xmin ~xmax ~ymin ~ymax ~datax ~datay ~mode =
  {xmin = xmin; xmax = xmax; ymin = ymin; ymax = ymax;
   datax = datax; datay = datay; mode = mode}
*)
(*
let print handle t =
  let diffx = t.xmax -. t.xmin and diffy = t.ymax -. t.ymin in
  let posx, posy, ticx, ticy =
    match t.mode with
      Rectangle(ticx, ticy) ->
        print_string "rect";
        D.rectangle handle ~x:t.xmin ~y:t.ymin diffx diffy;
        t.xmin, t.ymin, ticx, ticy
    | Two_lines(ofsx, ofsy, end1, end2, ticx, ticy) ->
        let posx = t.xmin +. ofsx *. diffx
        and posy = t.ymin +. ofsy *. diffy in
        D.save handle;
        D.set_endstyle handle Endstyle.Butt;
        D.line handle ~x:t.xmin ~y:posy t.xmax posy;
        (*    Endstyle.make_end handle t.xmin (t.ymin +. ofsy *. diffy)
              end1 t.xmax (t.ymin +. ofsy *. diffy);*)
        D.line handle ~x:posx ~y:t.ymin posx t.ymax;
        (*    Endstyle.make_end handle (t.xmin +. ofsx *. diffx) t.ymin
              end2 (t.xmin +. ofsx *. diffx) t.ymax*)
        D.restore handle;
        posx, posy, ticx, ticy
  in
  let make_tics b =
    let data = if b then t.datax else t.datay in
    (*     let diff = if b then diffx else diffy in *)
    match data with
      Graph(n,m) ->
        let nm = n * m in
        for i = 0 to nm do
          print_int i;
          let x,y =
            if b then (t.xmin +. (float i) *. diffx), posy
            else posx, (t.ymin +. (float i) *. diffy)
          in
          D.move handle x y;
          print_int i;
          let tic = if b then ticx else ticy in
          match tic with
            Line(r) ->
              let inv_tdist r =
                if b then D.inv_transform_dist handle 0. r
                else D.inv_transform_dist handle r 0.
              in
              let rx, ry = inv_tdist r in
              print_int i;
              D.rel_line handle (rx/.2.) (ry/.2.);
              D.rel_line handle (-.rx) (-.ry);
              let tx, ty = inv_tdist (-.11.)
                (*TODO: - (textsize + 1)*)
              in
          print_int i;

              D.text handle ~size:10. ~x:(x -. rx -. tx) ~y:(y -. ry -. ty)
                (string_of_float (if b then x else y))
        done;
  in
  make_tics true; make_tics false;
  D.move handle posx posy*)





(*Local variables:*)
(*compile-command: "ocamlopt -c axes.ml && ocamlc -c axes.ml"*)
(*End:*)
