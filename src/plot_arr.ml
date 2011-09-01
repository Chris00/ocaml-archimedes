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

(* ASSUME n > 0. *)
let lines_y vp ~fill ?base ~fillcolor (x: t) (y: t) n =
  let path = Path.make() in
  LINE_OF_ARRAY(path, x, y, FIRST, LAST(n));
  V.fit vp (Path.extents path);
  if fill then (
    let path_fill = Path.copy path in
    (match base with
    | None ->
      Path.line_to path_fill (float(LAST(n))) 0.;
      Path.line_to path_fill (float FIRST) 0.
    | Some b ->
      if DIM(b) <> n then invalid_arg(MOD ^ ".y: wrong length for \"base\"");
      LINE_OF_ARRAY(path_fill, x, b, LAST(n), FIRST);
      V.fit vp (Path.extents path_fill); (* update for base *)
    );
    Path.close path_fill;
    let color = V.get_color vp in
    V.set_color vp fillcolor;
    V.fill ~path:path_fill vp `Data ~fit:false;
    V.set_color vp color;
  );
  path

let fill_and_stroke vp path ~fill ~fillcolor =
  if fill then (
    let color = V.get_color vp in
    V.set_color vp fillcolor;
    V.fill ~path vp `Data ~fit:false;
    V.set_color vp color;
  );
  (* Draw (for bars, marks do not make any sense). *)
  V.stroke ~path vp `Data ~fit:false

let bars vp ~fill ?base ~fillcolor (x:t) (y:t) n w =
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
        ~x:(GET(x,i) -. w *. 0.5) ~y:(GET(b,i)) ~w ~h:(GET(y,i) -. GET(b,i))
    done);
  (* For bars, one certainly wants to see everything.  Moreover some
     space to the left and to the right is nice to have. *)
  let e = Path.extents path in
  V.fit vp { e with Matrix.x = e.Matrix.x -. 0.2 *. w;
                    w = e.Matrix.w +. 0.4 *. w};
  fill_and_stroke vp path ~fill ~fillcolor

let horizontal_bars vp ~fill ?base ~fillcolor (x:t) (y:t) n w =
  let path = Path.make() in
  (match base with
  | None ->
    for i = FIRST to LAST(n) do
      Path.rectangle path ~x:0. ~y:(GET(y,i) -. w *. 0.5) ~w:(GET(x,i)) ~h:w
    done
  | Some b ->
    if DIM(b) <> n then
      invalid_arg "Archimedes.Plot.Array.y: wrong length for \"base\"";
    for i = FIRST to LAST(n) do
      Path.rectangle path
        ~x:(GET(b,i)) ~y:(GET(y,i) -. w *. 0.5) ~w:(GET(x,i) -. GET(b,i)) ~h:w
    done);
  (* For horizontal bars, one certainly wants to see everything.
     Moreover some space to the top and bottom is nice to have. *)
  let e = Path.extents path in
  V.fit vp { e with Matrix.y = e.Matrix.y -. 0.2 *. w;
    h = e.Matrix.h +. 0.4 *. w};
  fill_and_stroke vp path ~fill ~fillcolor

let draw_marks vp style (x: t) (y: t) n =
  match style with
  | `Lines | `Impulses | `Bars _ | `HBars _ -> ()
  | `Points m | `Linespoints m ->
    for i = FIRST to LAST(n) do
      V.mark vp (GET(x,i)) (GET(y,i)) m
    done

(* ASSUME n > 0. *)
let unsafe_y vp ?base ?(fill=false) ?(fillcolor=default_fillcolor)
    ?(style=`Points "O") x y n =
  match style with
  | `Lines ->
    let path = lines_y vp ~fill ?base ~fillcolor x y n in
    V.stroke ~path vp `Data ~fit:false
  | `Points mark ->
    ignore(lines_y vp ~fill ?base ~fillcolor x y n);
    draw_marks vp style x y n
  | `Linespoints mark ->
    let path = lines_y vp ~fill ?base ~fillcolor x y n in
    V.stroke vp ~path `Data ~fit:false;
    draw_marks vp style x y n
  | `Bars w ->
    bars vp ~fill ?base ~fillcolor x y n w
  | `Impulses ->
    bars vp ~fill ?base ~fillcolor x y n 0.
  | `HBars w ->
    horizontal_bars vp ~fill ?base ~fillcolor x y n w

let y vp ?base ?fill ?fillcolor ?style ?(const=false) ydata =
  let n = DIM(ydata) in
  if n > 0 then (
    let y = if const then ydata else COPY(ydata) in
    let x = index_array n in
    unsafe_y vp ?base ?fill ?fillcolor ?style x y n
  )

(* FIXME: better selection of default colors *)
let default_fillcolors =
  [| default_fillcolor; Color.thistle; Color.misty_rose; Color.old_lace;
     Color.linen; Color.plum |]

let stack vp ?colors ?(fill=true) ?(fillcolors=[| |])
    ?(style=`Bars 0.5) yvecs =
  if Array.length yvecs > 0 && DIM(yvecs.(0)) > 0 then (
    let fillcolors =
      if Array.length fillcolors = 0 then default_fillcolors
      else fillcolors in
    let nc = Array.length fillcolors in
    let n = DIM(yvecs.(0)) in
    let x = index_array n in
    let y0 = COPY(yvecs.(0)) in
    unsafe_y vp ~fill ~fillcolor:fillcolors.(0) ~style x y0 n;
    let base = ref y0 in
    for i = 1 to pred (Array.length yvecs) do
      if DIM(yvecs.(i)) < n then
        invalid_arg(sprintf "Archimedes.Array.stack: length yvec.(%i) < %i"
                      i n);
      let yi = CREATE(n) in
      let b = !base in
      for j = FIRST to LAST(n) do SET(yi, j, GET(b,j) +. GET(yvecs.(i),j)) done;
      let fillcolor = fillcolors.(i mod nc) in
      unsafe_y vp ~base:b ~fill ~fillcolor ~style x yi n;
      base := yi
    done
  )

(* ASSUME n > 0 *)
let lines_xy vp ~fill ~fillcolor (x:t) (y:t) n =
  let path = Path.make() in
  LINE_OF_ARRAY(path, x, y, FIRST, LAST(n));
  V.fit vp (Path.extents path);
  if fill then (
    let path_fill = Path.copy path in
    Path.close path_fill;
    let color = V.get_color vp in
    V.set_color vp fillcolor;
    V.fill ~path:path_fill vp `Data ~fit:false;
    V.set_color vp color;
  );
  path

(* ASSUME n > 0 *)
let unsafe_xy vp ?(fill=false) ?(fillcolor=default_fillcolor)
    ?(style=`Points "O") (x:t) (y:t) n =
  match style with
  | `Lines ->
    let path = lines_xy vp ~fill ~fillcolor x y n in
    V.stroke ~path vp `Data ~fit:false
  | `Points mark ->
    ignore(lines_xy vp ~fill ~fillcolor x y n);
    draw_marks vp style x y n
  | `Linespoints mark ->
    let path = lines_xy vp ~fill ~fillcolor x y n in
    V.stroke vp ~path `Data ~fit:false;
    draw_marks vp style x y n
  | `Bars w ->
    bars vp ~fill ?base:None ~fillcolor x y n w
  | `Impulses ->
    bars vp ~fill ?base:None ~fillcolor x y n 0.
  | `HBars w ->
    horizontal_bars vp ~fill ?base:None ~fillcolor x y n w

let xy vp ?fill ?fillcolor ?style
    ?(const_x=false) xdata ?(const_y=false) ydata =
  let n = DIM(xdata) in
  if n <> DIM(ydata) then
    invalid_arg "Archimedes.Array.xy: arrays do not have the same length";
  if n > 0 then (
    let x = if const_x then xdata else COPY(xdata) in
    let y = if const_y then ydata else COPY(ydata) in
    unsafe_xy vp ?fill ?fillcolor ?style x y n
  )
