(**Styles definitions to make axes*)

module L = Layer

(* What an Axes must provide:*)
module type Axes =
sig
  val name: string (*A name for registering*)
  type t (*Type storing the information to make axes*)
  val make: string list -> t
    (*Given a string list whose interpretation is implementation-dependent, makes a [t].*)
  val print_axes: t -> ?xmin:float -> ?xmax:float -> ?ymin:float -> ?ymax:float ->
    ?color_axes:Color.t -> ?color_labels:Color.t -> L.t -> unit
    (*How to make the axes in a Layer.*)
end

type t =
    {print_axes: ?xmin:float -> ?xmax:float -> ?ymin:float -> ?ymax:float ->
     ?color_axes:Color.t -> ?color_labels:Color.t -> L.t -> unit}
      (*Type which will be registered*)

(*This is the module we provide by default.*)
module Default =
struct
  open Layer
  let name = "default"

  type data =
      Graph of int * int
    | Tics of float * int

  type ticstyle = (*FIXME: pointstyle??*)
      Line of float

  type mode =
      Rectangle
    | Two_lines of float * float

  type t =
      {mutable mode: mode;
       mutable datax:data; mutable ticx:ticstyle;
       mutable datay:data; mutable ticy:ticstyle}

  let make_options_list mode datax ticx datay ticy =
    let smode = match mode with
        Rectangle -> "rectangle"
      | Two_lines(x,y) -> Printf.sprintf "two_lines %f %f" x y
    in
    let get_sdata data = match data with
        Graph(n,m) -> Printf.sprintf "graph %i %i" n m
      | Tics(x,n) -> Printf.sprintf "tics %f %i" x n
    in
    let get_stic tic = match tic with
        Line(r) -> Printf.sprintf "line %f" r
    in
    [smode; get_sdata datax; get_stic ticx; get_sdata datay; get_stic ticy]

  let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

  (* Return the index of the first space in s.[i0 .. i1-1] or [i1] if
     none was found.  s.[i0 .. i1-1] is assumed to be a valid substring
     of s.  *)
  let rec index_of_space s i0 i1 =
    if i0 >= i1 then i1
    else if is_space s.[i0] then i0
    else index_of_space s (i0 + 1) i1

  let rec index_of_non_space s i0 i1 =
    if i0 >= i1 then i1
    else if is_space s.[i0] then index_of_non_space s (i0 + 1) i1
    else i0

  (*See also Backend.split_on_spaces*)
  let rec split_on_spaces s i0 i1 =
    let i0 = index_of_non_space s i0 i1 in (* skip spaces *)
    if i0 >= i1 then []
    else (
      let i01 = index_of_space s i0 i1 in
      String.sub s i0 (i01 - i0) :: split_on_spaces s (i01 + 1) i1
    )


  (*Possible errors when decoding*)
  (*FIXME: Enumerative type for errors? What about a declared
    exception in the interface?*)
  let error reason = invalid_arg ("Axes.Default.make -- "^reason)
  let nonvalid =
    Printf.sprintf "%s argument in the list is not valid (found %s)"
  and not_understood = Printf.sprintf "%s is not understood as %s type"
  and unparseable = Printf.sprintf "%s cannot be parsed as %s arguments for %s"


  (*Decoding string options*)
  let get_mode mode =
    match mode with
      "rectangle"  -> Rectangle
    | _ ->
        let mode_opts = split_on_spaces mode 0 (String.length mode) in
        if List.length mode_opts <> 3 then
          error(nonvalid "first" mode)
        else
          let opt::x::y::[] = mode_opts in
          match opt with
            "two_lines" ->
              (try
                 Two_lines(float_of_string x, float_of_string y)
               with Failure "float_of_string" ->
                 error (unparseable (x^" and/or "^y) "float" "mode"))
          | _ -> error (not_understood opt "mode")

  let get_data which data =
    let data_opts = split_on_spaces data 0 (String.length data) in
    if List.length data_opts <> 3 then
      error(nonvalid which data)
    else
      let opt::x::y::[] = data_opts in
      match opt with
        "graph" ->
          (try Graph(int_of_string x, int_of_string y)
           with Failure "int_of_string" ->
             error (unparseable (x^" and/or "^y) "int" "data"))
      | "tics" ->
          (try Tics(float_of_string x, int_of_string y)
           with Failure "int_of_string" ->
             error (unparseable y "int" "data")
           | Failure "float_of_string" ->
               error (unparseable x "float" "data"))
      | _ -> error (not_understood opt "data")

  let get_tic which tic =
    let tic_opts = split_on_spaces tic 0 (String.length tic) in
    if List.length tic_opts <> 2 then
      error(nonvalid which tic)
    else
      let opt::x::[] = tic_opts in
      match opt with
        "line" ->
          (try Line(float_of_string x)
           with Failure "float_of_string" ->
             error (unparseable x "float" "tic"))
      | _ -> error (not_understood opt "tic")




      (*Function make to be exported*)
  let make list =
    if List.length list <> 5 then
      error (Printf.sprintf "options list is not of length 5 (found %i)"
               (List.length list))
    else
      let mode::datax::ticx::datay::ticy::[] = list in

      {mode = get_mode mode;
       datax = get_data "second" datax; ticx = get_tic "third" ticx;
       datay = get_data "fourth" datay; ticy = get_tic "fifth" ticy}



  let tic t major ?color_labels x y x_axis ticstyle =
    match ticstyle with
      Line(r) ->
        let r = if major then r else r /. 2. in
        move_to t x y;
        (*FIXME: tic have to be independent of zoom:
          using [Backend] mode for stroking*)
        let x,y =
          if x_axis then
            (rel_move_to t 0. (r/.2.);
             rel_line_to t 0. (-.r);
             x, y+.r)
          else (*y_axis*)
            (rel_move_to t (-.r/.2.) 0.;
             rel_line_to t r 0.;
             x-.r,y)
        in
        if major then
          ((match color_labels with
              Some c ->
                save t;
                set_color t c
            | None -> ());
           show_text t ~rotate:0.
             ~x ~y (if x_axis then Backend.CB else Backend.LC)
             (string_of_float (if x_axis then x else y));
           match color_labels with
             Some _ -> restore t;
           | None -> ())

  let print_axes t ?xmin ?xmax ?ymin ?ymax ?color_axes ?color_labels layer =
    (*Make default values for xmin,..., ymax*)
    let rect = layer_extents layer in
    let xmin = match xmin with
        None -> rect.Backend.x
      | Some x -> x
    and xmax = match xmax with
        None -> rect.Backend.x +. rect.Backend.w
      | Some x -> x
    and ymin = match ymin with
        None -> rect.Backend.y
      | Some y -> y
    and ymax = match ymax with
        None -> rect.Backend.y +. rect.Backend.h
      | Some y -> y
    in
    (match color_axes with
       Some c -> save layer; set_color layer c
     | None -> ());
    (*Axes*)
    let ofsx, ofsy =
      match t.mode with
        Rectangle ->
          let x, y = min xmin xmax, min ymin ymax in
          let w, h = abs_float (xmax -. xmin), abs_float (ymax -. ymin) in
          rectangle layer x y w h;
          xmin, ymin
      | Two_lines(x,y) ->
          (*Need to update -before- so that mins/maxs are correctly
            initialized for making the axes lines.*)
          let xmin,ymin = min x xmin, min y ymin
          and xmax,ymax = max x xmax, max y ymax in
          move_to layer xmin y;
          line_to layer xmax y;
          move_to layer x ymin;
          line_to layer x ymax;
          x,y
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
            tic layer true ?color_labels x y x_axis ticmode;
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
          let rec make_tic x y n =
            if not ((x_axis && x > xmax) || (not x_axis && y > ymax)) then
              let xnew, ynew =
                if x_axis then
                  x+.minor, y
                else x, y+.minor
              in
              (tic layer (n=num_minors) ?color_labels x y x_axis ticmode;
               make_tic xnew ynew ((n mod num_minors)+1))

          in make_tic xmin ymin num_minors
               (*First tic will be a major tic.*)
    in
    (*Make data for X axis*)
    make_data t.datax t.ticx true;
    (*Make data for Y axis*)
    make_data t.datay t.ticy false;
    stroke layer;
    (match color_axes with
       Some _ -> restore layer
     | None -> ())
end

(*Registering axes representations.*)
module M = Map.Make(String)

let registry = ref M.empty

module Register(A:Axes) =
struct
  if not(M.mem A.name !registry) then
    let axes_maker options =
      let t = A.make options in
      {print_axes = A.print_axes t}
    in
    registry := M.add A.name axes_maker !registry;
end

(*Default is always available*)
let () =
  let module D = Register(Default) in ()

(*Working with axes*)
exception Axes_error of string

let make name options =
  try
    let f = M.find name !registry in
    f options
  with Not_found ->
    raise (Axes_error name)

let make_default ?(mode= Default.Rectangle) ?(ticx=(Default.Line 0.2)) datax
    ?(ticy=(Default.Line 0.2)) datay =
  let list = Default.make_options_list mode datax ticx datay ticy
  in make "default" list

let print_axes t = t.print_axes
(*Local variables:*)
(*compile-command: "ocamlopt -c -for-pack Archimedes axes.ml && ocamlc -c -for-pack Archimedes axes.ml"*)
(*End:*)
