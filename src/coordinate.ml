(* File: coordinate.ml

   Copyright (C) 2009

     Bertrand Desmons <Bertrand.Desmons@umons.ac.be>
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

module Matrix = Backend.Matrix

type ctm = Matrix.t

(** Simple sets build on weak arrays. *)
module W : sig
  type 'a t
  val make : unit -> 'a t
  val add : 'a t -> 'a -> unit
  val iter : 'a t -> ('a -> unit) -> unit
    (** [iter w f] iterates [f] on all "full" entries of [w]. *)
end =
struct
  type 'a t = {
    mutable ptr: 'a Weak.t;
    mutable used: int;  (* number of used entries *)
  }

  let make () = { prt = Weak.create 10;  used = 0 }

  let iter w f =
    let prt = w.ptr in
    for i = 0 to w.used - 1 do
      match Weak.get ptr i with
      | None -> ()
      | Some a -> f a
    done

  (* Move all full entries at the beginning. *)
  let compact w =
    let prt = w.ptr in
    let j = ref 0 in
    for i = 0 to used - 1 do
      match Weak.get prt i with
      | None -> ()
      | (Some _) as a -> Weak.set prt !j a;  incr j
    done;
    w.used <- !j

  let make_space w =
    (* If there is still some space, we do nothing *)
    if w.used >= Weak.length prt then (
      compact w;
      (* FIXME: should care about shrinking the array if used << length. *)
      if w.used >= Weak.length prt then (
        (* Still no space after compaction, increase the size. *)
        let ptr2 = Weak.create (Weak.length w.ptr) in
        Weak.blit w.ptr 0 ptr2 0 (Weak.length prt);
        w.ptr <- ptr2;
      )
    )

  let add w x =
    make_space w;
    Weak.set w.ptr w.used (Some x);
    w.used <- w.used + 1
end

(* Coordinate systems will stack on top of each other :

   tm1 -> tm2 -> ... tmN

   which correspond the tranforming a point [x] in the device
   coordinates [tmN * ... * tm2 * tm1 * x].  We could recompute the
   composition of affine transformations at each use but, as an
   optimization, we store [tmN * ... * tm2 * tm1] into [ctm].  Now, of
   course, if [tmi] is modified, [ctm] must be updated.  Thus
   coordinate systems form a tree through their dependencies.  When a
   coordinate system is updated, all [ctm] of the children must be
   recomputed.

   The [children] are kept in a weak hashtable so that link does not
   prevent them from being garbage collected.
*)
type t = {
  depends_on : t; (* The other coordinate system this one depends
                     upon.  All coordinate systems, except the device
                     one, depends on another one. *)
  tm : Matrix.t; (* transformation matrix that transform these
                    coordinates into the coordinates it depends upon.  *)
  ctm : Matrix.t;
  (* Transformation to device coordinates.  This is the composition of
     [tm] with the [ctm] of the coordinate system this one depends on.
     This is an optimization that needs to be updated if the
     underlying coordinate system is modified.  *)
  mutable up_to_date : bool;
  (* Whether the [ctm] is up to date.  If not it needs to be
     recomputed.  INVARIANT: when a coordinate system is not up to
     date, all its children must not be either. *)
  mutable children : t W.t;
  (* List of coordinate systems that depend on this one. *)
}

(* If necessary, resynchronize [coord.ctm] and possibly others along
   the way (to the tree root). *)
let rec update coord =
  if not coord.up_to_date then (
    let parent = coord.depends_on in
    (* First make sure its parent is up to date.  This recursion stops
       because the "device coordinates" are always up to date. *)
    update parent;
    Matrix.mul_in coord.ctm  parent.ctm coord.tm;
    coord.up_to_date <- true;
  )

(* Just replace the current CTM with the one of the transformation *)
let use b coord =
  update coord;
  let curr_ctm = Backend.get_matrix b in
  Backend.set_matrix b coord.ctm;
  curr_ctm

let restore b ctm = Backend.set_matrix b ctm

(* Transforming coordinates
 ***********************************************************************)

let to_device coord ~x ~y =
  update coord;
  Matrix.transform_point coord.ctm ~x ~y

let to_device_distance coord ~dx ~dy =
  update coord;
  Matrix.transform_distance coord.ctm ~dx ~dy

(* Creating new coordinate systems
 ***********************************************************************)

let make_identity() =
  let rec dev =
    { depends_on = dev; (* fake parent, never accessed *)
      tm = Matrix.make_identity();
      ctm = Matrix.make_identity();
      up_to_date = true; (* always must be *)
      children = [];
    } in
  dev

let copy coord =
  { coord with
      (* One must copy the matrices to avoid that modifying [coord]
         influences its copy. *)
      tm = Matrix.copy coord.tm;
      ctm = Matrix.copy coord.ctm;
      children = []; (* this is a new coordinate system, no other one
                        depends on it. *)
  }

(* Create a new coordinate system that consists into first applying
   the transformation [tm] before the one of [coord]. *)
let make_with_matrix coord tm =
  let coord' = { depends_on = coord;
                 tm = tm;
                 ctm = Matrix.mul coord.ctm coord.tm;
                 up_to_date = coord.up_to_date; (* iff [coord] is up to date *)
                 children = []  } in
  coord.children <- coord' :: coord.children;
  coord'

let make_translate coord ~x ~y =
  make_with_matrix coord (Matrix.make_translate ~x ~y)

let make_scale coord ~x ~y =
  make_with_matrix coord (Matrix.make_scale ~x ~y)

let make_rotate coord ~angle =
  make_with_matrix coord (Matrix.make_rotate ~angle)


(* Changing this coordinate system
 ***********************************************************************)

let rec put_children_not_up_to_date coord =
  (* If the current coordinate system is not up to date (which implies
     its children are not either -- because of the invariant), there
     is no need to update the CTM until its parents are brought up to
     date. *)
  if coord.up_to_date then begin
    Matrix.mul_in coord.ctm  coord.depends_on.ctm coord.tm;
    List.iter (fun c ->
                 (* If [c] is already not up to date, so are all its
                    children.  There is no need to recurse down. *)
                 if c.up_to_date then (
                   c.up_to_date <- false;
                   put_children_not_up_to_date c; (* => invariant *)
                 )
              ) coord.children
  end

let translate coord ~x ~y =
  Matrix.translate coord.tm ~x ~y;
  put_children_not_up_to_date coord

let scale coord ~x ~y =
  Matrix.scale coord.tm ~x ~y;
  put_children_not_up_to_date coord

let rotate coord ~angle =
  Matrix.rotate coord.tm ~angle;
  put_children_not_up_to_date coord
