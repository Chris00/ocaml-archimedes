(* General array plotting functions, to be specialized using
   macro substitution *)

(* Integer arrays
 ***********************************************************************)
(* When following a discrete structure, one must create an array of
   integer floats for the X axis.  These arrays being never modified,
   they can be shared by all functions. *)

let x_index = CREATE(1024)

let () =
  for i = FIRST to LAST(1024) do
    SET(x_index, i, float i)
  done

(* FIXME: allow offset? *)
let index_array n =
  if n < 1024 then x_index
  else (
    (* FIXME: do we want to replace x_index ?? *)
    let m = CREATE(n) in
    for i = FIRST to LAST(n) do
      SET(x_index, i, float i)
    done;
    m
  )

(* Plotting functions
 ***********************************************************************)

let lines_y vp ~fill ?base ~fillcolor (x: t) (y: t) n =
  let path = Path.make() in
  LINE_OF_ARRAY(path, x, y, FIRST, LAST(n));
  V.fit vp (Path.extents path);
  if fill then (
    let path_fill = Path.copy path in
    (match base with
    | None ->
      Path.line_to path (float(LAST(n))) 0.;
      Path.line_to path (float FIRST) 0.
    | Some b ->
      if DIM(b) <> n then invalid_arg(MOD ^ ".y: wrong length for \"base\"");
      LINE_OF_ARRAY(path_fill, x, COPY(b), LAST(n), FIRST);
      V.fit vp (Path.extents path_fill); (* update for base *)
    );
    Path.close path_fill;
    let color = V.get_color vp in
    V.set_color vp fillcolor;
    V.fill ~path:path_fill vp V.Data ~fit:false;
    V.set_color vp color;
  );
  path

let boxes vp ~fill ?base ~fillcolor (x:t) (y:t) n w =
  let path = Path.make() in
  (match base with
  | None ->
    for i = FIRST to LAST(n) do
      Path.rectangle path ~x:(GET(x,i) -. w *. 0.5) ~y:0. ~w ~h:(GET(y,i))
    done
  | Some b ->
    if DIM(b) <> n then
      invalid_arg "Archimedes.Plot.Array.y: wrong length for \"base\"";
    for i = FIRST to LAST(n) do
      Path.rectangle path
        ~x:(GET(x,i) -. w *. 0.5) ~y:(GET(b,i)) ~w ~h:(GET(y,i))
    done);
    (* For boxes, one certainly wants to see everything *)
  V.fit vp (Path.extents path);
  if fill then (
    let color = V.get_color vp in
    V.set_color vp fillcolor;
    V.fill ~path vp V.Data ~fit:false;
    V.set_color vp color;
  );
    (* Draw (for boxes, marks do not make any sense). *)
  V.stroke ~path vp V.Data

let draw_marks vp style (x: t) (y: t) n =
  match style with
  | `Lines | `Impulses | `Boxes _ -> ()
  | `Points m | `Linespoints m ->
    for i = FIRST to LAST(n) do
      V.mark vp (GET(x,i)) (GET(y,i)) m
    done

let unsafe_y vp ?base ?(fill=false) ?(fillcolor=default_fillcolor)
    ?(style=`Points "O") x y n =
  match style with
  | `Lines ->
    let path = lines_y vp ~fill ?base ~fillcolor x y n in
    V.stroke ~path vp V.Data ~fit:false
  | `Points mark ->
    ignore(lines_y vp ~fill ?base ~fillcolor x y n);
    draw_marks vp style x y n
  | `Linespoints mark ->
    let path = lines_y vp ~fill ?base ~fillcolor x y n in
    V.stroke vp ~path V.Data ~fit:false;
    draw_marks vp style x y n
  | `Boxes w ->
    boxes vp ~fill ?base ~fillcolor x y n w
  | `Impulses ->
    boxes vp ~fill ?base ~fillcolor x y n 0.

let y vp ?base ?fill ?fillcolor ?style ?(const=false) ydata =
  let y = if const then ydata else COPY(ydata) in
  let n = DIM(y) in
  let x = index_array n in
  unsafe_y vp ?base ?fill ?fillcolor ?style x y n

(* FIXME: better selection of default colors *)
let default_fillcolors =
  [| default_fillcolor; Color.thistle; Color.misty_rose; Color.old_lace;
     Color.linen; Color.plum |]

let stack vp ?colors ?(fill=true) ?(fillcolors=[| |])
    ?(style=`Boxes 0.5) yvecs =
  if Array.length yvecs > 0 then (
    let fillcolors =
      if Array.length fillcolors = 0 then default_fillcolors
      else fillcolors in
    let nc = Array.length fillcolors in
    let n = DIM(yvecs.(0)) in
    let x = index_array n in
    let y0 = COPY(yvecs.(0)) in
    unsafe_y vp ~fill ~fillcolor:fillcolors.(0) ~style x y0 n;
    let base = COPY(y0) in
    for i = 1 to Array.length yvecs - 1 do
      if DIM(yvecs.(i)) < n then
        invalid_arg(sprintf "Archimedes.Array.stack: length yvec.(%i) < %i"
                      i n);
      let yi = COPY(yvecs.(i)) in
      let fillcolor = fillcolors.(i mod nc) in
      unsafe_y vp ~base:base ~fill ~fillcolor ~style x yi n;
        (* [base] is saved in the path, it can be overwritten  *)
      for i = FIRST to LAST(n) do SET(base, i, GET(base,i) +. GET(yi,i)) done
    done
  )

let lines_xy vp ~fill ~fillcolor (x:t) (y:t) n =
  let path = Path.make() in
  LINE_OF_ARRAY(path, x, y, FIRST, LAST(n));
  V.fit vp (Path.extents path);
  if fill then (
    let path_fill = Path.copy path in
    Path.close path_fill;
    let color = V.get_color vp in
    V.set_color vp fillcolor;
    V.fill ~path:path_fill vp V.Data ~fit:false;
    V.set_color vp color;
  );
  path

let unsafe_xy vp ?(fill=false) ?(fillcolor=default_fillcolor)
    ?(style=`Points "O") (x:t) (y:t) n =
  match style with
  | `Lines ->
    let path = lines_xy vp ~fill ~fillcolor x y n in
    V.stroke ~path vp V.Data ~fit:false
  | `Points mark ->
    ignore(lines_xy vp ~fill ~fillcolor x y n);
    draw_marks vp style x y n
  | `Linespoints mark ->
    let path = lines_xy vp ~fill ~fillcolor x y n in
    V.stroke vp ~path V.Data ~fit:false;
    draw_marks vp style x y n

let xy vp ?fill ?fillcolor ?style
    ?(const_x=false) xdata ?(const_y=false) ydata =
  let n = DIM(xdata) in
  if n <> DIM(ydata) then
    invalid_arg "Archimedes.Array.xy: arrays do not have the same length";
  let x = if const_x then xdata else COPY(xdata) in
  let y = if const_y then ydata else COPY(ydata) in
  unsafe_xy vp ?fill ?fillcolor ?style x y n
