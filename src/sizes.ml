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

type size =
  | Absolute of float
  | Rel_not_updated of float
  | Rel_updated of float * float

type t = {
  parent: t;
  children: t list;

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
      mark_size = Abolute 1. }
  in
  (* real_root is not accessible to the user, so there's no risk to change
     its variables. All the following code just musn't modify parent's data
     of a given node. *)
  let root =
    { parent = real_root;
      children = [];
      line_width = Rel_update (lw, lw *. init);
      text_size = Rel_update (ts, ts *. init);
      mark_size = Rel_update (ms, ms *. init) }
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
  | Absolute y | Rel_update _, y -> Rel_update (x, x *. y)
  | Rel_not_update _ -> Rel_not_update x

let rec update_lw node =
  match node.line_width with
  | Absolute _ | Rel_update _, _ -> ()
  | Rel_not_update lw ->
      let parent = node.parent in
      update_line_width parent;
      node.line_width <- prod lw parent.line_width

let rec update_ts node =
  match node.text_size with
  | Absolute _ | Rel_update _, _ -> ()
  | Rel_not_update ts ->
      let parent = node.parent in
      update_ts parent;
      node.text_size <- prod ts parent.text_size

let rec update_ms node =
  match node.mark_size with
  | Absolute _ | Rel_update _, _ -> ()
  | Rel_not_update ms ->
      let parent = node.parent in
      update_ms parent;
      node.mark_size <- prod ms parent.mark_size

let get_lw node =
  update_lw node;
  match node.line_width with
  | Absolute lw | Rel_update _, lw -> lw
  | Rel_not_update _ -> assert false (* check *)

let get_ts node =
  update_ts node;
  match node.text_size with
  | Absolute ts | Rel_update _, ts -> ts
  | Rel_not_update _ -> assert false (* check *)

let get_marks node =
  update_ms node;
  match node.mark_size with
  | Absolute ms | Rel_update _, ms -> ms
  | Rel_not_update _ -> assert false (* check *)

let outdate_lw node =
  let need_iter =
    match node.line_width with
    | Absolute _ -> true
    | Rel_update lw, _ ->
        node.line_width <- Rel_not_update lw;
        true
    | Rel_not_update _ -> false

let outdate_ts node =
  let need_iter =
    match node.text_size with
    | Absolute _ -> true
    | Rel_update ts, _ ->
        node.line_width <- Rel_not_update ts;
        true
    | Rel_not_update _ -> false

let outdate_ms node =
  let need_iter =
    match node.mark_size with
    | Absolute _ -> true
    | Rel_update ms, _ ->
        node.line_width <- Rel_not_update ms;
        true
    | Rel_not_update _ -> false

let set_rel_lw node size =
  node.line_width <- Rel_not_update size;
  outdate_lw node

let set_rel_ts node size =
  node.text_size <- Rel_not_update size;
  outdate_ts node

let set_rel_ms node size =
  node.mark_size <- Rel_not_update size;
  outdate_ms node

let set_abs_lw node size =
  node.line_width <- Rel_not_update size;
  outdate_lw node

let set_abs_ts node size =
  node.text_size <- Rel_not_update size;
  outdate_ts node

let set_abs_ms node size =
  node.mark_size <- Rel_not_update size;
  outdate_ms node

