(* File: backend.ml

   Copyright (C) 2009

     Bertrand Desmons <Bertrand.Desmons@student.umons.ac.be>
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

type line_cap =
  | BUTT
  | ROUND
  | SQUARE

type line_join =
  | JOIN_MITER
  | JOIN_ROUND
  | JOIN_BEVEL

type rectangle = {x:float; y:float; w:float; h:float}

type slant = Upright | Italic

type weight = Normal | Bold

type text_position =
  | CC  (** centrer horizontally and vertically *)
  | LC  (** align left horizontally and center vertically *)
  | RC  (** align right horizontally and center vertically *)
  | CT  (** center horizontally and align top vertically *)
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

  val rectangle : t -> x:float -> y:float -> width:float -> height:float -> unit

  val arc : t -> x:float -> y:float -> r:float -> a1:float -> a2:float -> unit
    (* Do we need arc_negative (path orientation)? *)

  (* (* Is this really needed? *)
  val ellipse_arc : t ->
    x:float -> y:float -> a:float -> b:float -> a1:float -> a2:float -> unit
  *)
  val close_path : t -> unit

  val path_extents : t -> rectangle

  val stroke : t -> unit
  val stroke_preserve : t -> unit
  val fill : t -> unit
  val fill_preserve : t -> unit
  val clip : t -> unit
  val clip_preserve : t -> unit

  val save : t -> unit
  val restore : t -> unit

  val translate : t -> x:float -> y:float -> unit
  val scale : t -> x:float -> y:float -> unit
  val rotate : t -> angle:float -> unit
(*  val transform : t -> float -> float -> float * float
  val transform_dist : t -> float -> float -> float * float
  val invert : t -> coord
  val inv_transform : t -> float -> float -> float * float
  val inv_transform_dist : t -> float -> float -> float * float
  val apply : next:coord -> t -> unit
  val get_coord : t -> coord
  val reset_to_id : t -> unit
*)
  val text : t -> size:float -> x:float -> y:float -> string -> unit
(*  val put_image :
    t -> x:float -> y:float -> ?scale:float -> string -> unit*)


(* FIXME: Are [save] and [restore] needed?  Is [clip] needed ?  How to
   implement it for graphics ?  Do we want to rotate text ?
*)
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
  rectangle : x:float -> y:float -> width:float -> height:float -> unit;
  arc : x:float -> y:float -> r:float -> a1:float -> a2:float -> unit;
  close_path : unit -> unit;
  path_extents : unit -> rectangle;

  stroke : unit -> unit;
  stroke_preserve : unit -> unit;
  fill : unit -> unit;
  fill_preserve : unit -> unit;
  clip : unit -> unit;
  clip_preserve : unit -> unit;

  save: unit -> unit;
  restore: unit -> unit;

  translate : x:float -> y:float -> unit;
  scale : x:float -> y:float -> unit;
  rotate : angle:float -> unit;
(*  transform : 'a -> float -> float -> float * float;
  transform_dist : 'a -> float -> float -> float * float;
  invert : 'a -> coord;
  inv_transform : 'a -> float -> float -> float * float;
  inv_transform_dist : 'a -> float -> float -> float * float;
  apply : next:coord -> 'a -> unit;
  get_coord : 'a -> coord;
  reset_to_id : 'a -> unit;
*)
  text: size:float -> x:float -> y:float -> string -> unit;
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
        path_extents = (fun () -> B.path_extents handle);
        stroke = (fun () -> B.stroke handle);
        stroke_preserve = (fun () -> B.stroke_preserve handle);
        fill = (fun () -> B.fill handle);
        fill_preserve = (fun () -> B.fill_preserve handle);
        clip = (fun () -> B.clip handle);
        clip_preserve = (fun () -> B.clip_preserve handle);

        save = (fun () -> B.save handle);
        restore = (fun () -> B.restore handle);

        translate = B.translate handle;
        scale = B.scale handle;
        rotate = B.rotate handle;

        text = B.text handle;
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
let path_extents t = t.path_extents()
let stroke t = t.stroke()
let stroke_preserve t = t.stroke_preserve()
let fill t = t.fill()
let fill_preserve t = t.fill_preserve()
let clip t = t.clip()
let clip_preserve t = t.clip_preserve()
let save t = t.save()
let restore t = t.restore()
let translate t = t.translate
let scale t = t.scale
let rotate t = t.rotate
let text t = t.text

type error =
  | Corrupted_dependency of string
  | Non_loadable_dependency of string
  | Nonexistent of string
  | Corrupted of string
  | Not_registering of string

exception Error of error

let registered () = M.fold (fun name _ l -> name :: l) !registry []

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

(* Return a list of substrings of s.[i0 .. i1-1] which are separated
   by one or several spaces. *)
let rec split_on_spaces s i0 i1 =
  let i0 = index_of_non_space s i0 i1 in (* skip spaces *)
  if i0 >= i1 then []
  else (
    let i01 = index_of_space s i0 i1 in
    String.sub s i0 (i01 - i0) :: split_on_spaces s (i01 + 1) i1
  )

(* Split the backend from its option list *)
let backend_options b =
  let len = String.length b in
  let i = index_of_space b 0 len in
  if i = len then (String.lowercase b, []) (* no options *)
  else (String.lowercase(String.sub b 0 i), split_on_spaces b (i+1) len)

(* Return a fully qualified path to the [fname] or raise [Not_found]. *)
let rec find_file dirs fname =
  match dirs with
  | [] -> raise Not_found (* no more paths to explore *)
  | d :: tl ->
      let fname = Filename.concat d fname in
      if Sys.file_exists fname then fname else find_file tl fname

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
      let dyn = (try find_file dirs (Dynlink.adapt_filename(base ^ ".cmo"))
                 with Not_found -> raise(Error(Nonexistent backend))) in
      (* Load dependencies *)
      List.iter begin fun dep ->
        if not(List.mem dep !loaded_dependencies) then (
          try
            let fdep = Dynlink.adapt_filename(dep ^ ".cmo") in
            let fdep = find_file dirs fdep in
            Dynlink.loadfile fdep;
            loaded_dependencies := dep :: !loaded_dependencies
          with _ -> raise(Error(Non_loadable_dependency dep))
        )
      end (get_dependencies dirs base);
      (* Load the main module *)
      (try Dynlink.loadfile dyn
       with _ -> raise(Error(Corrupted backend)));
      (* Check that the backend correctly updated the registry *)
      try M.find backend !registry
      with Not_found -> raise(Error(Not_registering backend))
  in
  make options width height



(* [s.[i]] and [p.[i]] are identical for all [i] s.t. [i0 <= i < i1]. *)
let rec identical s p i0 i1 =
  i0 >= i1 || (s.[i0] = p.[i0] && identical s p (i0 + 1) i1)

let start_with s p =
  let len_p = String.length p in
  String.length s >= len_p && identical s p 0 len_p

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
