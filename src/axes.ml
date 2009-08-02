(**Styles definitions to make axes*)

type data =
    Graph of int * int
  | Tics of float * int

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


module Print(B:Backend.T) =
struct
  let tic t major ?color_labels x y x_axis ticstyle =
    match ticstyle with
      Line(r) ->
        let r = if major then r else r /. 2. in
        B.move_to t x y;
        (*FIXME: tic have to be independent of zoom:
          using [Backend] mode for stroking*)
        let x,y =
          if x_axis then
            (B.rel_line_to t 0. (-.r/.2.);
             B.rel_line_to t 0. r;
             x, y+.r)
          else (*y_axis*)
            (B.rel_line_to t (-.r/.2.) 0.;
             B.rel_line_to t r 0.;
             x-.r,y)
        in
        if major then
          ((match color_labels with
              Some c ->
                B.save t;
                B.set_color t c
            | None -> ());
           B.show_text t ~rotate:0.(*~pos:B.Position.left*)
             ~x ~y (if x_axis then Backend.CB else Backend.LC)
             (string_of_float (if x_axis then x else y));
           match color_labels with
             Some c -> B.restore t;
           | None -> ())

  let make_axes t ?color_axes ?color_labels xmin xmax ymin ymax
      datax datay mode =
    (match color_axes with
       Some c -> B.save t; B.set_color t c
     | None -> ());
    (*Axes*)
    let ofsx, ofsy, ticx, ticy =
      match mode with
        Rectangle(tx,ty) ->
          B.rectangle t xmin ymin (xmax -. xmin) (ymax -. ymin);
          xmin, ymin, tx, ty
      | Two_lines(x,y,tx,ty) ->
          (*Need to update -before- so that mins/maxs are correctly
            initialized for making the axes lines.*)
          let xmin,ymin = min x xmin, min y ymin
          and xmax,ymax = max x xmax, max y ymax in
          B.move_to t xmin y;
          B.line_to t xmax y;
          B.move_to t x ymin;
          B.line_to t x ymax;
          x,y, tx, ty
    in
    let xmin,ymin = min ofsx xmin, min ofsy ymin
    and xmax,ymax = max ofsx xmax, max ofsy ymax in
    (*Tics or like*)
    let make_data data ticmode x_axis =
      match data with
        Graph(major,minor) ->
          let step =
            let diff = if x_axis then xmax -. xmin else ymax -. ymin in
            diff /. (float major) in
          let ministep = step /. (float minor) in
          for i = 0 to major do (*major tics in X axis*)
            let ofs = (float i) *. step in
            let x, y =
              if x_axis then xmin +. ofs, ofsy
              else ofsx, ymin +. ofs
            in
            (*Tic to put, centered in (x, y), with label 'x' or 'y' as
              given by x_axis.*)
            tic t true x y x_axis ticmode;
            if i < major then
              for j = 1 to minor - 1 do
                let x,y =
                  if x_axis then x +. (float j) *. ministep, y
                  else x, y +. (float j) *. ministep
                in
                tic t false x y x_axis ticmode
              done
          done
      | Tics(minor,num_minors) ->
          let rec make_tic x y x_axis ticmode n =
            if not ((x_axis && x > xmax) || (not x_axis && y > ymax)) then
              let xnew, ynew =
                if x_axis then
                  x+.minor, y
                else x, y+.minor
              in
              (tic t (n=num_minors) x y x_axis ticmode;
               make_tic xnew ynew x_axis ticmode ((n mod num_minors)+1))

          in make_tic xmin ymin true ticx 0;
          make_tic xmin ymin false ticy 0
    in
    (*Make data for X axis*)
    make_data datax ticx true;
    (*Make data for Y axis*)
    make_data datay ticy false;
    B.stroke t;
    (match color_axes with
       Some c -> B.restore t
     | None -> ())

end

(*Local variables:*)
(*compile-command: "ocamlopt -c axes.ml && ocamlc -c axes.ml"*)
(*End:*)
