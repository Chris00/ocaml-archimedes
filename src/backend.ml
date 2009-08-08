(* File: backend.ml

   Copyright (C) 2009

     Bertrand Desmons <Bertrand.Desmons@umons.ac.be>
     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Printf

(* The following modules are needed by some backends and are not
   loadable dynamically so must be referenced here so that pluging
   linking succeeds. *)
module ForLinking_1__ = Callback

(* Without the following, the native version reports "undefined
   symbol: caml_hash_variant" when loading Cairo. *)
external for_linking_1__ : unit -> unit = "caml_hash_variant"


type line_cap =
  | BUTT
  | ROUND
  | SQUARE

type line_join =
  | JOIN_MITER
  | JOIN_ROUND
  | JOIN_BEVEL

type rectangle = {x:float; y:float; w:float; h:float}

type matrix = { mutable xx: float; mutable yx: float;
                mutable xy: float; mutable yy: float;
                mutable x0: float; mutable y0: float; }

type slant = Upright | Italic

type weight = Normal | Bold

type text_position =
  | CC
  | LC
  | RC
  | CT
  | CB
  | LT
  | LB
  | RT
  | RB


module type T =
sig
  type t

  (* val set_pointstyle : t -> pointstyle -> unit *)
  (* val set_pattern : t -> pattern -> unit *)
  val set_color : t -> Color.t -> unit
  val set_line_width : t -> float -> unit
  val set_line_cap : t -> line_cap -> unit
  val set_dash : t -> float -> float array -> unit
  val set_line_join : t -> line_join -> unit
  (* val set_miter_limit : t -> float -> unit *)

  (* val get_pointstyle: t -> pointstyle *)
  (* val get_pattern: t -> pattern *)
  val get_line_width: t -> float
  val get_line_cap: t -> line_cap
  val get_dash: t -> float array * float
  val get_line_join: t -> line_join

  (* val string_of_error : t -> backend_error -> string *)
    (*val point : t -> float -> float -> unit*)
  val move_to : t -> x:float -> y:float -> unit
  val line_to : t -> x:float -> y:float -> unit
  val rel_move_to : t -> x:float -> y:float -> unit
  val rel_line_to : t -> x:float -> y:float -> unit

  val curve_to : t ->
    x1:float -> y1:float -> x2:float -> y2:float -> x3:float -> y3:float -> unit

  val rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit

  val arc : t -> x:float -> y:float -> r:float -> a1:float -> a2:float -> unit
    (* Do we need arc_negative (path orientation)? *)

  (* (* Is this really needed? *)
  val ellipse_arc : t ->
    x:float -> y:float -> a:float -> b:float -> a1:float -> a2:float -> unit
  *)
  val close_path : t -> unit
  val clear_path : t -> unit
  val path_extents : t -> rectangle

  val stroke : t -> unit
  val stroke_preserve : t -> unit
  val fill : t -> unit
  val fill_preserve : t -> unit

  val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit

  val save : t -> unit
  val restore : t -> unit

  val translate : t -> x:float -> y:float -> unit
  val scale : t -> x:float -> y:float -> unit
  val rotate : t -> angle:float -> unit
  val set_matrix : t -> matrix -> unit
  val get_matrix : t -> matrix

  val select_font_face : t -> slant -> weight -> string -> unit
  val set_font_size : t -> float -> unit
  val text_extents : t -> string -> rectangle
  val show_text : t -> rotate:float -> x:float -> y:float ->
    text_position -> string -> unit

(*  val put_image :
    t -> x:float -> y:float -> ?scale:float -> string -> unit*)
end


(* Internals of a backend handle
 ***********************************************************************)
type t = {
  width: float;  (* width of the backend canvas in its original units *)
  height: float; (* height of the backend canvas in its original units *)
  close: unit -> unit;

  (* set_pointstyle: 'a -> pointstyle -> unit; *)
  (* set_pattern: 'a -> pattern -> unit; *)
  set_color : Color.t -> unit;
  set_line_width : float -> unit;
  set_line_cap : line_cap -> unit;
  set_dash : float -> float array -> unit;
  set_line_join : line_join -> unit;

  get_line_width: unit -> float;
  get_line_cap: unit -> line_cap;
  get_dash: unit -> float array * float;
  get_line_join: unit -> line_join;

  move_to : x:float -> y:float -> unit;
  line_to : x:float -> y:float -> unit;
  rel_move_to : x:float -> y:float -> unit;
  rel_line_to : x:float -> y:float -> unit;

  curve_to: x1:float -> y1:float -> x2:float -> y2:float ->
                                            x3:float -> y3:float -> unit;
  rectangle : x:float -> y:float -> w:float -> h:float -> unit;
  arc : x:float -> y:float -> r:float -> a1:float -> a2:float -> unit;
  close_path : unit -> unit;
  clear_path : unit -> unit;
  path_extents : unit -> rectangle;

  stroke : unit -> unit;
  stroke_preserve : unit -> unit;
  fill : unit -> unit;
  fill_preserve : unit -> unit;
  clip_rectangle : x:float -> y:float -> w:float -> h:float -> unit;

  save: unit -> unit;
  restore: unit -> unit;

  translate : x:float -> y:float -> unit;
  scale : x:float -> y:float -> unit;
  rotate : angle:float -> unit;
  set_matrix : matrix -> unit;
  get_matrix : unit -> matrix;

  select_font_face: slant -> weight -> string -> unit;
  set_font_size: float -> unit;
  text_extents: string -> rectangle;
  show_text: rotate:float -> x:float -> y:float ->
                                text_position -> string -> unit
  (* put_image: 'a -> x:float -> y:float -> ?scale:float -> string -> unit; *)
}

(* Registering
 ***********************************************************************)

module type Capabilities =
sig
  include T
  val name: string
  val make : options:string list -> float -> float -> t
  val close : options:string list -> t -> unit
end

module M = Map.Make(String)

let registry = ref M.empty
  (* Global registry holding the modules registered. *)

module Register(B: Capabilities) =
struct
  (* Register, only if not previously registered. *)
  if not(M.mem B.name !registry) then
    let make options w h =
      let handle = B.make options w h in
      { width = w;  height = h;
        close = (fun () -> B.close ~options handle);
        set_color = B.set_color handle;
        set_line_width = B.set_line_width handle;
        set_line_cap = B.set_line_cap handle;
        set_dash = B.set_dash handle;
        set_line_join = B.set_line_join handle;
        get_line_width = (fun () -> B.get_line_width handle);
        get_line_cap = (fun () -> B.get_line_cap handle);
        get_dash = (fun () -> B.get_dash handle);
        get_line_join = (fun () -> B.get_line_join handle);

        move_to = B.move_to handle;
        line_to = B.line_to handle;
        rel_move_to = B.rel_move_to handle;
        rel_line_to = B.rel_line_to handle;
        curve_to = B.curve_to handle;
        rectangle = B.rectangle handle;
        arc = B.arc handle;
        close_path = (fun () -> B.close_path handle);
        clear_path = (fun () -> B.clear_path handle);
        path_extents = (fun () -> B.path_extents handle);
        stroke = (fun () -> B.stroke handle);
        stroke_preserve = (fun () -> B.stroke_preserve handle);
        fill = (fun () -> B.fill handle);
        fill_preserve = (fun () -> B.fill_preserve handle);
        clip_rectangle = B.clip_rectangle handle;

        save = (fun () -> B.save handle);
        restore = (fun () -> B.restore handle);

        translate = B.translate handle;
        scale = B.scale handle;
        rotate = B.rotate handle;
        set_matrix = B.set_matrix handle;
        get_matrix = (fun () -> B.get_matrix handle);

        select_font_face = B.select_font_face handle;
        set_font_size = B.set_font_size handle;
        text_extents = B.text_extents handle;
        show_text = B.show_text handle;
      }
    in
    registry := M.add B.name make !registry
  ;;
end

(* Dynamic loading
 ***********************************************************************)

(* Allow partial evaluation on [t] to recover the function (allows to
   avoid indirections) *)
let width t = t.width
let height t = t.height
let close t = t.close()
let set_color t = t.set_color
let set_line_width t = t.set_line_width
let set_line_cap t = t.set_line_cap
let set_dash t = t.set_dash
let set_line_join t = t.set_line_join
let get_line_width t = t.get_line_width()
let get_line_cap t = t.get_line_cap()
let get_dash t = t.get_dash()
let get_line_join t = t.get_line_join()
let move_to t = t.move_to
let line_to t = t.line_to
let rel_move_to t = t.rel_move_to
let rel_line_to t = t.rel_line_to
let curve_to t = t.curve_to
let rectangle t = t.rectangle
let arc t = t.arc
let close_path t = t.close_path()
let clear_path t = t.clear_path()
let path_extents t = t.path_extents()
let stroke t = t.stroke()
let stroke_preserve t = t.stroke_preserve()
let fill t = t.fill()
let fill_preserve t = t.fill_preserve()
let clip_rectangle t = t.clip_rectangle
let save t = t.save()
let restore t = t.restore()
let translate t = t.translate
let scale t = t.scale
let rotate t = t.rotate
let set_matrix t m = t.set_matrix m
let get_matrix t = t.get_matrix()
let select_font_face t = t.select_font_face
let set_font_size t = t.set_font_size
let text_extents t = t.text_extents
let show_text t = t.show_text

type error =
  | Corrupted_dependency of string
  | Non_loadable_dependency of string
  | Nonexistent of string
  | Not_loadable of string * Dynlink.error
  | Not_registering of string

let string_of_error = function
  | Corrupted_dependency fname ->
      sprintf "The dependency file %s is corrupted." fname
  | Non_loadable_dependency fname ->
      sprintf "The libray %s (occurring as a plugin dependency) cannot \
        be loaded" fname
  | Nonexistent bk -> sprintf "The backend %S is not found" bk
  | Not_loadable(bk, e) ->
      sprintf "The backend %S is not loadable because:\n%s"
        bk (Dynlink.error_message e)
  | Not_registering bk ->
      sprintf "The backend %S does not register itself properly" bk

exception Error of error

let registered () = M.fold (fun name _ l -> name :: l) !registry []

open String_utils

(* Split the backend from its option list. Backend name is put in
   lowercase letters.*)
let backend_options b =
  let s,l = first_and_list b in
  String.lowercase s, l

(* Return a fully qualified path to the [fname] or raise [Not_found]. *)
let rec find_file dirs fname =
  match dirs with
  | [] -> raise Not_found (* no more paths to explore *)
  | d :: tl ->
      let path_file = Filename.concat d fname in
      if Sys.file_exists path_file then path_file else find_file tl fname

(* Get the list of dependencies for a given backend.  Beware that
   order is important. *)
let get_dependencies dirs basename =
  try
    let dep = find_file dirs (basename ^ ".dep") in
    try
      let fh = open_in dep in
      let lines = ref [] in
      try
        while true do lines := input_line fh :: !lines done;
        assert false
      with End_of_file -> List.rev !lines
    with Sys_error _ -> raise(Error(Corrupted_dependency dep))
  with Not_found -> [] (* assuming it implies no deps *)


let loaded_dependencies = ref []

let make ?(dirs=[]) b width height =
  (* FIXME: must always include several system dirs detected at compile time *)
  let backend, options = backend_options b in
  let make =
    try M.find backend !registry (* backend already loaded *)
    with Not_found ->
      (* Load the backend and all its dependencies -- that usually
         means modules with stubs. *)
      Dynlink.allow_unsafe_modules true;
      let base = "archimedes_" ^ backend in
      (* Load dependencies *)
      List.iter begin fun dep ->
        if not(List.mem dep !loaded_dependencies) then (
          let fdep = Dynlink.adapt_filename dep in
          try
            Dynlink.loadfile fdep;
            loaded_dependencies := dep :: !loaded_dependencies
          with Dynlink.Error e -> raise(Error(Not_loadable(fdep, e)))
        )
      end (get_dependencies dirs base);
      (* Load the main module *)
      let dyn = (try find_file dirs (Dynlink.adapt_filename(base ^ ".cmo"))
                 with Not_found -> raise(Error(Nonexistent backend))) in
      (try Dynlink.loadfile dyn
       with Dynlink.Error e -> raise(Error(Not_loadable(backend, e))));
      (* Check that the backend correctly updated the registry *)
      try M.find backend !registry
      with Not_found -> raise(Error(Not_registering backend))
  in
  make options width height

let available ~dirs =
  let ext = if Dynlink.is_native then ".cmxs" else ".cmo" in
  List.fold_left begin fun bk d ->
    try
      let files = Sys.readdir d in
      Array.fold_left begin fun bk fname ->
        if start_with fname "archimedes_"
          && Filename.check_suffix fname ext then
            let fname = Filename.chop_suffix fname ext in
            String.sub fname 11 (String.length fname - 11) :: bk
        else bk
      end bk files
    with Sys_error _ -> bk (* e.g. if [d] not a dir, not readable,... *)
  end [] dirs


module Matrix =
struct
  type t = matrix

  exception Not_invertible

  let make_identity () = { xx=1.; yx=0.; xy=0.; yy=1.; x0=0.; y0=0. }

  let make_translate ~x ~y =
    { xx=1.; yx=0.; xy=0.; yy=1.;  x0=x;  y0=y }

  let make_scale ~x ~y =
    { xx=x; yx=0.; xy=0.; yy=y;  x0=0.;  y0=0. }

  let make_rotate ~angle =
    { xx=cos(angle);    yx=sin(angle);
      xy= -. sin(angle); yy=cos(angle);  x0=0.;  y0=0. }

  let set_to_identity m =
    m.xx <- 1.; m.xy <- 0.; m.x0 <- 0.;
    m.yx <- 0.; m.yy <- 1.; m.y0 <- 0.

  let translate m ~x ~y =
    m.x0 <- m.x0 +. m.xx *. x +. m.xy *. y;
    m.y0 <- m.y0 +. m.yx *. x +. m.yy *. y

  let scale m ~x ~y =
    m.xx <- m.xx *. x;
    m.yx <- m.yx *. x;
    m.xy <- m.xy *. y;
    m.yy <- m.yy *. y

  let rotate m ~angle =
    let cosa = cos angle and sina = sin angle in
    let xx = m.xx in
    m.xx <- xx *. cosa +. m.xy *. sina;
    m.xy <- m.xy *. cosa -. xx *. sina;
    let yx = m.yx in
    m.yx <- yx *. cosa +. m.yy *. sina;
    m.yy <- m.yy *. cosa -. yx *. sina

  let invert m =
    (* Optimize for scaling|translation matrices which are the ones
       most used. *)
    if m.xy = 0. && m.yx = 0. then (
      m.x0 <- -. m.x0;
      m.y0 <- -. m.y0;
      if m.xx <> 1. then (
        if m.xx = 0. then raise Not_invertible;
        m.xx <- 1. /. m.xx;
        m.x0 <- m.x0 *. m.xx;
      );
      if m.yy <> 1. then (
        if m.yy = 0. then raise Not_invertible;
        m.yy <- 1. /. m.yy;
        m.y0 <- m.y0 *. m.yy;
      );
    )
    else
      let det = m.xx *. m.yy -. m.yx *. m.xy in
      if det = 0. || 1. /. det = 0. (* infinite det *) then
        raise Not_invertible;
      let yy = m.xx /. det in
      m.xx <- m.yy /. det;
      m.xy <- -. m.xy /. det;
      m.yx <- -. m.yx /. det;
      m.yy <- yy;
      let y0 = -. m.yx *. m.x0 -. yy *. m.y0 in
      m.x0 <- -. m.xx *. m.x0 -. m.xy *. m.y0;
      m.y0 <- y0


  let det m = m.xx *. m.yy -. m.xy *. m.yx

  let transform_distance m ~dx ~dy =
    (m.xx *. dx +. m.xy *. dy,  m.yx *. dx +. m.yy *. dy)

  let transform_point m ~x ~y =
    (m.xx *. x +. m.xy *. y +. m.x0,  m.yx *. x +. m.yy *. y +. m.y0)

  let copy t = { t with xx = t.xx }

  let inv_transform_point m =
    let m_inv = copy m in
    invert m_inv;
    transform_point m_inv

  let inv_transform_distance m =
    let m_inv = copy m in
    invert m_inv;
    transform_distance m_inv

  let mul b a =
    { xx = b.xx *. a.xx +. b.xy *. a.yx;
      xy = b.xx *. a.xy +. b.xy *. a.yy;
      yx = b.yx *. a.xx +. b.yy *. a.yx;
      yy = b.yx *. a.xy +. b.yy *. a.yy;
      x0 = b.xx *. a.x0 +. b.xy *. a.y0 +. b.x0;
      y0 = b.yx *. a.x0 +. b.yy *. a.y0 +. b.y0; }

  (* allow [c] to be [a] or [b]. *)
  let mul_in c b a =
    let c_xx = b.xx *. a.xx +. b.xy *. a.yx in
    let c_xy = b.xx *. a.xy +. b.xy *. a.yy in
    let c_yx = b.yx *. a.xx +. b.yy *. a.yx in
    let c_yy = b.yx *. a.xy +. b.yy *. a.yy in
    let c_c0 = b.xx *. a.x0 +. b.xy *. a.y0 +. b.x0 in
    c.y0 <- b.yx *. a.x0 +. b.yy *. a.y0 +. b.y0;
    c.xx <- c_xx;
    c.xy <- c_xy;
    c.yx <- c_yx;
    c.yy <- c_yy;
    c.x0 <- c_x0

  let has_shear t =
    t.yx <> 0. || t.xy <> 0.
end
