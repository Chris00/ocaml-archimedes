(* File: plot.ml

   Copyright (C) 2009

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)
(*
module Sizes:
sig
  type t
  val get_lw: t -> float
  val get_ts: t -> float
  val get_marks: t -> float
  val set_rel_lw: t -> float -> unit
  val set_rel_ts: t -> float -> unit
  val set_rel_marks: t -> float -> unit
  val set_abs_lw: t -> float -> unit
  val set_abs_ts: t -> float -> unit
  val set_abs_marks: t -> float -> unit
end
  =
struct
  type size =
    | Absolute of float
    | Rel_not_updated of float
    | Rel_updated of float * float

  type t =
      { mutable lw: size; (* Line width *)
        mutable ts: size; (* Text size *)
        mutable ms: size; (* Marks size *) }

  (* A node is 'up to date' (concerning a style) if "style".global is of
     the form [Some _].

     INVARIANT(S):

     - if a node is up to date, then so is its parent; conversely, if a
     node is not up to date, then so are all its children.

     - if a node is up_to_date, then "style" is ABSOLUTE(x) or
     REL_UPDATED(h, x); in this latter case, we have that:

     the parent of the current node is also ABSOLUTE(y) or
     REL_UPDATED(_, y); and x = h * y. *)

  let make_root init lines text marks =
    let rec real_root =
      { parent = real_root;
        lw = Absolute init;
        ts = Absolute init;
        ms = Absolute init;
        (* Note: to make a mark we place ourselves in normalized
           coords. There is thus no need to premultiply.*)
        children = [] }
    in
    (*real_root is not accessible to the user, so there's no risk to
      change its variables. All the following code just mustn't modify
      parent's data of a given node.*)
    let root =
      { parent = real_root;
        lw = REL_UPDATED (lines, init *. lines);
        ts = REL_UPDATED (text, init *. text);
        marks = REL_UPDATED (marks, marks);
        children = [] }
    in
    real_root.children <- [root];
    root

  let add_child parent child =
    parent.children <- child :: parent.children

  let prod x = function
      ABSOLUTE(y)
    | REL_UPDATED(_,y) -> REL_UPDATED (x, x*.y)
    | REL_NOT_UPDATED _ -> REL_NOT_UPDATED x

  let child parent lines text marks =
    let child =
      {parent = parent;
       lw = prod lines parent.lw;
       ts = prod text parent.ts;
       marks = prod marks parent.marks;
       children = []}
    in
    add_child parent child;
    child

  let rec update_lw node =
    match node.lw with
      ABSOLUTE(_)
    | REL_UPDATED(_,_) -> ()
    | REL_NOT_UPDATED w ->
        let parent = node.parent in
        update_lw parent;(*ensures invariant 1 ok.*)
        node.lw <- prod w parent.lw
          (*ensures invariant 2*)

  let rec update_ts node =
    match node.ts with
      ABSOLUTE(_)
    | REL_UPDATED(_,_) -> ()
    | REL_NOT_UPDATED s ->
        let parent = node.parent in
        update_ts parent;(*ensures invariant 1 ok.*)
        node.ts <- prod s parent.ts
          (*ensures invariant 2*)

  let rec update_marks node =
    match node.marks with
      ABSOLUTE(_)
    | REL_UPDATED(_,_) -> ()
    | REL_NOT_UPDATED m ->
        let parent = node.parent in
        update_marks parent;(*ensures invariant 1 ok.*)
        node.marks <- prod m parent.marks
          (*ensures invariant 2*)

  let rec set_none_lw node =
    let need_iter =
      match node.lw with
        REL_UPDATED(this,_) ->
          node.lw <- REL_NOT_UPDATED this;
          true
      | ABSOLUTE _ -> true
      | REL_NOT_UPDATED _ -> false
    in
    (*Now un_update all children if needed => invariant 1.*)
    if need_iter then List.iter set_none_lw node.children

  let rec set_none_ts node =
    let need_iter =
      match node.ts with
        REL_UPDATED(this,_) ->
          node.ts <- REL_NOT_UPDATED this;
          true
      | ABSOLUTE _ -> true
      | REL_NOT_UPDATED _ -> false
    in
    (*Now un_update all children if needed => invariant 1.*)
    if need_iter then List.iter set_none_ts node.children

  let rec set_none_marks node =
    let need_iter =
      match node.marks with
        REL_UPDATED(this,_) ->
          node.marks <- REL_NOT_UPDATED this;
          true
      | ABSOLUTE _ -> true
      | REL_NOT_UPDATED _ -> false
    in
    (*Now un_update all children if needed => invariant 1.*)
    if need_iter then List.iter set_none_marks node.children

  let get_lw node =
    update_lw node;
    match node.lw with
      REL_NOT_UPDATED _ -> failwith "get_lw: internal error"
    | ABSOLUTE x
    | REL_UPDATED(_,x) -> x

  let get_ts node =
    update_ts node;
    match node.ts with
      REL_NOT_UPDATED _ -> failwith "get_ts: internal error"
    | ABSOLUTE x
    | REL_UPDATED(_,x) -> x

  let get_marks node =
    update_marks node;
    match node.marks with
      REL_NOT_UPDATED _ -> failwith "get_marks: internal error"
    | ABSOLUTE x
    | REL_UPDATED(_,x) -> x

  (*Note: the Failures have a priori no way to happen, due to
    updates. If that is the case, then there is a bug.*)

  let set_rel_lw node size =
    node.lw <- REL_NOT_UPDATED size;
    set_none_lw node

  let set_rel_ts node size =
    node.ts <- REL_NOT_UPDATED size;
    set_none_ts node

  let set_rel_marks node size =
    node.marks <- REL_NOT_UPDATED size;
    set_none_marks node

  let set_abs_lw node size =
    node.lw <- ABSOLUTE size;
    set_none_lw node

  let set_abs_ts node size =
    node.ts <- ABSOLUTE size;
    set_none_ts node

  let set_abs_marks node size =
    node.marks <- ABSOLUTE size;
    set_none_marks node

end

type t = ...    +axes_set
*)
