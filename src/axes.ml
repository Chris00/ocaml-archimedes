(**Styles definitions to make axes*)

module L = Layer

module P = Pointstyle.Default

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

type t = ?xmin:float -> ?xmax:float -> ?ymin:float -> ?ymax:float ->
     ?color_axes:Color.t -> ?color_labels:Color.t -> L.t -> unit
      (*Type which will be registered*)

(*This is the module we provide by default.*)
module Default =
struct
  open String_utils
  let name = "default"

  type data =
      Graph of int * int
    | Tics of float * int

  type ticstyle = P.t

  type mode =
      Rectangle
    | Two_lines of float * float

  type t =
      {mutable mode: mode;
       mutable datax:data; mutable ticx:ticstyle; mutable minticx:ticstyle;
       mutable datay:data; mutable ticy:ticstyle; mutable minticy:ticstyle;}

  let make_options_list mode datax ticx minticx datay ticy minticy =
    let smode = match mode with
        Rectangle -> "rectangle"
      | Two_lines(x,y) -> Printf.sprintf "two_lines %f %f" x y
    in
    let get_sdata data = match data with
        Graph(n,m) -> Printf.sprintf "graph %i %i" n m
      | Tics(x,n) -> Printf.sprintf "tics %f %i" x n
    in
    [smode;
     get_sdata datax; P.string_of_t ticx; P.string_of_t minticx;
     get_sdata datay; P.string_of_t ticy; P.string_of_t minticy ]

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
    try P.make tic
    with (*Pointstyle.PSError(s) ->
      error (Printf.sprintf
               "%s argument: '%s' cannot be found as point style (tic: %s)"
               which s tic)*)
    | Invalid_argument reason ->
        error (Printf.sprintf
                 "%s argument: cannot find a point style (tic: %s, error: %s)"
               which tic reason)


  (*Function make to be exported*)
  let make list =
    if List.length list <> 7 then
      error (Printf.sprintf "options list is not of length 7 (found %i)"
               (List.length list))
    else
      (*Printf.printf "Axes.Default.make...%!";*)
      let mode::datax::ticx::minticx::datay::ticy::minticy::[] = list in
      {mode = get_mode mode;
       datax = get_data "second" datax; ticx = get_tic "third" ticx;
       minticx =  get_tic "fourth" minticx;
       datay = get_data "fifth" datay; ticy = get_tic "sixth" ticy;
       minticy = get_tic "seventh" minticy;}



  let make_tic layer ?color_labels ?text x y =
    L.point layer x y;
    match text with
      None -> ()
    | Some (pos, txt) ->
        ((match color_labels with
           Some c ->
             L.save layer;
             L.set_color layer c
         | None -> ());
        L.show_text layer ~rotate:0. ~x ~y pos txt;
        match color_labels with
          Some _ -> L.restore layer;
        | None -> ())

  let print_axes t ?xmin ?xmax ?ymin ?ymax ?color_axes ?color_labels layer =
    (*Make default values for xmin,..., ymax*)
    let rect = L.layer_extents layer in
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
       Some c -> L.save layer; L.set_color layer c
     | None -> ());

    (*Axes*)
    let ofsx, ofsy =
      match t.mode with
        Rectangle ->
          let x, y = min xmin xmax, min ymin ymax in
          let w, h = abs_float (xmax -. xmin), abs_float (ymax -. ymin) in
          L.rectangle layer x y w h;
          xmin, ymin
      | Two_lines(x,y) ->
          (*Need to update -before- so that mins/maxs are correctly
            initialized for making the axes lines.*)
          let xmin,ymin = min x xmin, min y ymin
          and xmax,ymax = max x xmax, max y ymax in
          L.move_to layer xmin y;
          L.line_to layer xmax y;
          L.move_to layer x ymin;
          L.line_to layer x ymax;
          x,y
    in
    let xmin,ymin = min ofsx xmin, min ofsy ymin
    and xmax,ymax = max ofsx xmax, max ofsy ymax in

    (*Tics or like*)
    let make_data x_axis =
      match (if x_axis then t.datax else t.datay) with
        Graph(major,minor) ->
          let step =
            let diff = if x_axis then xmax -. xmin else ymax -. ymin in
            diff /. (float major) in
          let ministep = step /. (float minor) in
          let tic, minitic =
            if x_axis then
              (Pointstyle.make_default t.ticx),
              (Pointstyle.make_default t.minticx)
            else
              (Pointstyle.make_default t.ticy),
              (Pointstyle.make_default t.minticy)
          in
          L.set_point_style layer tic;
          for i = 0 to major do (*major tics in X axis*)
            let ofs = (float i) *. step in
            let x, y =
              if x_axis then xmin +. ofs, ofsy
              else ofsx, ymin +. ofs
            in
            (*Tic to put, centered in (x, y), with label 'x' or 'y' as
              given by x_axis.*)
            let text =
              if x_axis then Backend.CB, string_of_float x
              else Backend.LC, string_of_float y
            in
            make_tic layer ?color_labels ~text x y;
            if i < major then
              (L.save layer;
               L.set_point_style layer minitic;
               for j = 1 to minor - 1 do
                 let x,y =
                   if x_axis then x +. (float j) *. ministep, y
                   else x, y +. (float j) *. ministep
                 in
                 make_tic layer x y;
               done;
               L.restore layer);
          done
      | Tics(minor,num_minors) ->
          let rec make_tics x y n =
            if not ((x_axis && x > xmax) || (not x_axis && y > ymax)) then
              let xnew, ynew =
                if x_axis then
                  x+.minor, y
                else x, y+.minor
              in
              let text =
                if n = num_minors then
                  Some (if x_axis then
                          Backend.CB, string_of_float x
                        else
                          Backend.LC, string_of_float y)
                else None
              in
              (make_tic layer ?color_labels ?text x y;
               make_tics xnew ynew ((n mod num_minors)+1))

          in make_tics xmin ymin num_minors
               (*First tic will be a major tic.*)
    in
    (*Make data for X axis*)
    make_data true;
    (*Make data for Y axis*)
    make_data false;
    L.stroke layer;
    (match color_axes with
       Some _ -> L.restore layer
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
      A.print_axes t
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

let make_default ?(mode= Default.Rectangle)
    ?(ticx=(P.TIC_UP 4.)) ?(minticx=(P.TIC_UP 2.)) datax
    ?(ticy=(P.TIC_LEFT 4.)) ?(minticy=(P.TIC_LEFT 2.)) datay =
  let list = Default.make_options_list mode datax ticx minticx datay ticy minticy
  in make "default" list

let print_axes t = t

(*Local variables:*)
(*compile-command: "ocamlopt -c -for-pack Archimedes axes.ml && ocamlc -c -for-pack Archimedes axes.ml"*)
(*End:*)
