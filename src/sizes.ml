(* File: size.ml

   Copyright (C) 2009-2015

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     Pierre Hauweele <antegallya@gmail.com>
     Noemie Meunier <noemie_6462@hotmail.com>
     Fabian Pijcke <fabian.pijcke@gmail.com>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(* TODO Check that the sizes are correctly handled: get_lw must return the
   size to be given to the backend. Also check the updates. *)

type size =
  | Absolute of float
  | Rel_not_updated of float
  | Rel_updated of float * float

type t = {
  parent: t;
  mutable children: t list;

  mutable line_width: size;
  mutable text_size: size;
  mutable mark_size: size
}

let make_root init lw ts ms =
  let rec real_root =
    { parent = real_root;
      children = [];
      line_width = Absolute init;
      text_size = Absolute init;
      mark_size = Absolute 1. }
  in
  (* real_root is not accessible to the user, so there's no risk to change
     its variables. All the following code just musn't modify parent's data
     of a given node. *)
  let root =
    { parent = real_root;
      children = [];
      line_width = Rel_updated (lw, lw *. init);
      text_size = Rel_updated (ts, ts *. init);
      mark_size = Rel_updated (ms, ms *. init) }
  in
  real_root.children <- [root];
  root

let make parent lw ts ms =
  let sizes =
    { parent = parent;
      children = [];
      line_width = lw;
      text_size = ts;
      mark_size = ms } in
  parent.children <- sizes :: parent.children;
  sizes

let make_rel parent lw ts ms =
  make parent (Rel_not_updated lw) (Rel_not_updated ts) (Rel_not_updated ms)

let make_abs parent lw ts ms =
  make parent (Absolute lw) (Absolute ts) (Absolute ms)

let prod x = function
  | Absolute y -> Rel_updated (x, x *. y)
  | Rel_updated (_, y) -> Rel_updated (x, x *. y)
  | Rel_not_updated _ -> Rel_not_updated x

let rec update_lw node =
  match node.line_width with
  | Absolute _ | Rel_updated (_, _) -> ()
  | Rel_not_updated lw ->
      let parent = node.parent in
      update_lw parent;
      node.line_width <- prod lw parent.line_width

let rec update_ts node =
  match node.text_size with
  | Absolute _ | Rel_updated (_, _) -> ()
  | Rel_not_updated ts ->
      let parent = node.parent in
      update_ts parent;
      node.text_size <- prod ts parent.text_size

let rec update_ms node =
  match node.mark_size with
  | Absolute _ | Rel_updated (_, _) -> ()
  | Rel_not_updated ms ->
      let parent = node.parent in
      update_ms parent;
      node.mark_size <- prod ms parent.mark_size

let get_lw node =
  update_lw node;
  match node.line_width with
  | Absolute lw -> lw
  | Rel_updated (_, lw) -> lw
  | Rel_not_updated _ -> assert false (* check *)

let get_ts node =
  update_ts node;
  match node.text_size with
  | Absolute ts -> ts
  | Rel_updated (_, ts) -> ts
  | Rel_not_updated _ -> assert false (* check *)

let get_marks node =
  update_ms node;
  match node.mark_size with
  | Absolute ms -> ms
  | Rel_updated (_, ms) -> ms
  | Rel_not_updated _ -> assert false (* check *)

let rec outdate_lw node =
  let need_iter =
    match node.line_width with
    | Absolute _ -> true
    | Rel_updated (lw, _) ->
        node.line_width <- Rel_not_updated lw;
        true
    | Rel_not_updated _ -> false
  in
  if need_iter then List.iter outdate_lw node.children

let rec outdate_ts node =
  let need_iter =
    match node.text_size with
    | Absolute _ -> true
    | Rel_updated (ts, _) ->
        node.line_width <- Rel_not_updated ts;
        true
    | Rel_not_updated _ -> false
  in
  if need_iter then List.iter outdate_ts node.children

let rec outdate_ms node =
  let need_iter =
    match node.mark_size with
    | Absolute _ -> true
    | Rel_updated (ms, _) ->
        node.line_width <- Rel_not_updated ms;
        true
    | Rel_not_updated _ -> false
  in
  if need_iter then List.iter outdate_ms node.children

let set_rel_lw node size =
  node.line_width <- Rel_not_updated size;
  outdate_lw node

let set_rel_ts node size =
  node.text_size <- Rel_not_updated size;
  outdate_ts node

let set_rel_ms node size =
  node.mark_size <- Rel_not_updated size;
  outdate_ms node

let set_abs_lw node size =
  node.line_width <- Rel_not_updated size;
  outdate_lw node

let set_abs_ts node size =
  node.text_size <- Rel_not_updated size;
  outdate_ts node

let set_abs_ms node size =
  node.mark_size <- Rel_not_updated size;
  outdate_ms node

