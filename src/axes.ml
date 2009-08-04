(**Styles definitions to make axes*)

open Layer
module B = Backend

type data =
    Graph of int * int
  | Tics of float * int

type ticstyle = (*FIXME: pointstyle??*)
    Line of float

type mode =
    Rectangle of ticstyle * ticstyle
  | Two_lines of float * float * ticstyle * ticstyle

let tic t major ?color_labels x y x_axis ticstyle =
  match ticstyle with
    Line(r) ->
      let r = if major then r else r /. 2. in
      move_to t x y;
      (*FIXME: tic have to be independent of zoom:
        using [Backend] mode for stroking*)
      let x,y =
        if x_axis then
          (rel_move_to t 0. (-.r/.2.);
           rel_line_to t 0. r;
           x, y+.r)
        else (*y_axis*)
          (rel_move_to t (-.r/.2.) 0.;
           rel_line_to t r 0.;
           x-.r,y)
      in
      if major then
        (match color_labels with
            Some c ->
              save t;
              set_color t c
          | None -> ());
         show_text t ~rotate:0.(*~pos:Position.left*)
           ~x ~y (if x_axis then B.CB else B.LC)
           (string_of_float (if x_axis then x else y));
         match color_labels with
           Some c -> restore t;
         | None -> ()

  let make_axes layer ?color_axes ?color_labels ?xmin ?xmax ?ymin ?ymax
      datax datay mode =
    (*Make default values for xmin,..., ymax*)
    let rect = layer_extents layer in
    let xmin = match xmin with
        None -> rect.B.x
      | Some x -> x
    and xmax = match xmax with
        None -> rect.B.x +. rect.B.w
      | Some x -> x
    and ymin = match ymin with
        None -> rect.B.y
      | Some y -> y
    and ymax = match ymax with
        None -> rect.B.y +. rect.B.h
      | Some y -> y
    in
    (match color_axes with
       Some c -> save layer; set_color layer c
     | None -> ());
    (*Axes*)
    let ofsx, ofsy, ticx, ticy =
      match mode with
        Rectangle(tx,ty) ->
          let x, y = min xmin xmax, min ymin ymax in
          let w, h = abs_float (xmax -. xmin), abs_float (ymax -. ymin) in
          rectangle layer x y w h;
          xmin, ymin, tx, ty
      | Two_lines(x,y,tx,ty) ->
          (*Need to update -before- so that mins/maxs are correctly
            initialized for making the axes lines.*)
          let xmin,ymin = min x xmin, min y ymin
          and xmax,ymax = max x xmax, max y ymax in
          move_to layer xmin y;
          line_to layer xmax y;
          move_to layer x ymin;
          line_to layer x ymax;
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
            tic layer true x y x_axis ticmode;
            if i < major then
              for j = 1 to minor - 1 do
                let x,y =
                  if x_axis then x +. (float j) *. ministep, y
                  else x, y +. (float j) *. ministep
                in
                tic layer false x y x_axis ticmode
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
              (tic layer (n=num_minors) x y x_axis ticmode;
               make_tic xnew ynew x_axis ticmode ((n mod num_minors)+1))

          in make_tic xmin ymin true ticx 0;
          make_tic xmin ymin false ticy 0
    in
    (*Make data for X axis*)
    make_data datax ticx true;
    (*Make data for Y axis*)
    make_data datay ticy false;
    stroke layer;
    (match color_axes with
       Some c -> restore layer
     | None -> ())

(*Local variables:*)
(*compile-command: "ocamlopt -c -for-pack Archimedes axes.ml && ocamlc -c -for-pack Archimedes axes.ml"*)
(*End:*)
