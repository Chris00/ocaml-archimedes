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

type ctm = Matrix.t

(* Monitors are used to keep track of wether a given coordinate system
   has been updated.  This can be used to redraw elements depending to
   the coordinate system. *)
module Monitor =
struct
  type t = int ref
  (* The low bit (2**0) is used as a boolean, the other bits as id. *)

  let equal m1 m2 = m1 == m2 (* physical equality *)
  let hash m = !m lsr 1

  let id = ref 0
  let make () = incr id; ref(!id lsl 1)
  let set m = m := !m lor 1
  let reset_mask = lnot 1
  let reset m = m := !m land reset_mask
  let is_set m = !m land 1 <> 0
end

module WM = Weak.Make(Monitor)

(* Coordinate systems will stack on top of each other :

   tm1 -> tm2 -> ... -> tmN

   which corresponds to tranforming a point [x] into the device
   coordinates [tmN * ... * tm2 * tm1 * x].  We could recompute the
   composition of affine transformations at each use but, as an
   optimization, we store [tmN * ... * tm2 * tm1] into [ctm].  Now, of
   course, if [tmi] is modified, [ctm] must be updated.  Thus
   coordinate systems form a tree through their dependencies.  When a
   coordinate system is updated, all [ctm] of the children must be
   recomputed.

   The [children] are kept in a weak hashtable so that link does not
   prevent them from being garbage collected.  Since the weak hash
   tables are instantiated through a functor, one must use the
   recursive module "trick".
*)
module rec W : (Weak.S with type data = Coord.t) = Weak.Make(Coord)
and Coord : sig
  type t =  { depends_on : t;
              tm : Matrix.t;
              ctm : Matrix.t;
              mutable up_to_date : bool;
              mutable children : W.t;
              mutable monitors : WM.t;
              id : int; }
  val equal : t -> t -> bool
  val hash : t -> int
  val new_id : unit -> int
end =
struct
  type t = {
    depends_on : t; (* The other coordinate system this one depends
                       on.  All coordinate systems, except the first
                       one (root), depend on another one. *)
    tm : Matrix.t; (* transformation matrix that transform these
                      coordinates into the coordinates it depends on.  *)
    ctm : Matrix.t;
    (* Transformation to "root" coordinates.  This is the composition of
       [tm] with the [ctm] of the coordinate system this one depends on.
       This is an optimization that needs to be updated if the
       underlying coordinate system is modified.  *)
    mutable up_to_date : bool;
    (* Whether the [ctm] is up to date.  If not it needs to be
       recomputed.  INVARIANT: when a coordinate system is not up to
       date, all its children must not be either. *)
    mutable children : W.t;
    (* List of coordinate systems that depend on this one. *)
    mutable monitors : WM.t;
    (* Handles used to monitor updates to this coordinate system. *)
    id : int;
    (* id of the object (for hash) *)
  }

  let equal c1 c2 = c1 == c2
  let hash c = c.id

  (* global id for distinguishing coordinate systems *)
  let next_id = ref 0
  let new_id () = incr next_id; !next_id
end

include Coord


(* If necessary, resynchronize [coord.ctm] and possibly others along
   the way (to the tree root). *)
let rec update coord =
  if not coord.up_to_date then (
    let parent = coord.depends_on in
    (* First make sure its parent is up to date.  This recursion stops
       because the "device coordinates" are always up to date. *)
    update parent;
    Matrix.mul_in coord.ctm parent.ctm coord.tm;
    coord.up_to_date <- true;
  )



(* Replace the CTM with the one of the transformation and return it for
   future restoration. *)
let use b coord =
  update coord;
  let saved_ctm = Backend.get_matrix b in
  Backend.set_matrix b coord.ctm;
  saved_ctm

let restore b ctm = Backend.set_matrix b ctm

(* Transforming coordinates
 ***********************************************************************)

let to_parent coord ~x ~y =
  update coord;
  Matrix.transform_point coord.tm ~x ~y

let from_parent coord ~x ~y =
  update coord;
  Matrix.inv_transform_point coord.tm ~x ~y

let to_device coord ~x ~y =
  update coord;
  Matrix.transform_point coord.ctm ~x ~y

let to_device_distance coord ~dx ~dy =
  update coord;
  Matrix.transform_distance coord.ctm ~dx ~dy

let to_coord coord ~x ~y =
  update coord;
  Matrix.inv_transform_point coord.ctm ~x ~y

let to_coord_distance coord ~dx ~dy =
  update coord;
  Matrix.inv_transform_distance coord.ctm ~dx ~dy

(* Creating new coordinate systems
 ***********************************************************************)

let make_root m =
  let m = Matrix.Homothety.to_matrix m in
  let rec dev =
    { depends_on = dev; (* fake parent, physically equal *)
      tm = m;
      ctm = m;
      up_to_date = true; (* always must be *)
      children = W.create 5;
      monitors = WM.create 0;
      id = new_id();
    } in
  dev


let copy coord =
  { coord with
      (* One must copy the matrices to avoid that modifying [coord]
         influences its copy. *)
      tm = Matrix.copy coord.tm;
      ctm = Matrix.copy coord.ctm;
      children = W.create 5; (* this is a new coordinate system, no other one
                                depends on it. *)
      monitors = WM.create 2;
      id = new_id();
  }

(* Create a new coordinate system that consists into first applying
   the transformation [tm] before the one of [coord].  [ctm] must be
   [coord.ctm * tm] but is is passed to allow an optimization for
   [make_identity]. *)
let make_from_transform_with_ctm coord tm ctm =
  let coord' = { depends_on = coord;
                 tm = tm;
                 ctm = ctm;
                 up_to_date = coord.up_to_date; (* iff [coord] is up to date *)
                 children = W.create 5;
                 monitors = WM.create 2;
                 id = new_id() } in
  W.add coord.children coord';
  coord'


let make_identity coord =
  let id = Matrix.make_identity() in
  make_from_transform_with_ctm coord id (Matrix.copy coord.ctm)

let make_from_transform coord tm =
  let tm = Matrix.unsafe_of_homothety tm in
  make_from_transform_with_ctm coord tm (Matrix.mul coord.ctm tm)

let make_translate coord ~x ~y =
  make_from_transform coord (Matrix.Homothety.make_translate ~x ~y)

let make_scale coord ~x ~y =
  make_from_transform coord (Matrix.Homothety.make_scale ~x ~y)


(* Changing this coordinate system
 ***********************************************************************)

let rec put_children_not_up_to_date coord =
  (*Cannot put any root 'not up to date'*)
  if coord == coord.depends_on then (* that must be the root. *)
    failwith "Coordinate: Trying to modify root coordinate";
  (* If the current coordinate system is not up to date (which implies
     its children are not either -- because of the invariant), there
     is nothing to do (no need to update the CTM until its parents are
     brought up to date). Otherwise, put this coordinate not up to
     date and transmit this information to children. *)
  if coord.up_to_date then begin
    coord.up_to_date <- false;
    W.iter put_children_not_up_to_date coord.children; (* => invariant *)
    WM.iter Monitor.set coord.monitors;
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

let transform coord tm =
  Matrix.blit tm coord.tm;
  put_children_not_up_to_date coord


(* Monitoring
 ***********************************************************************)

type monitor = Monitor.t

let monitor coord =
  let m = Monitor.make() in
  WM.add coord.monitors m;
  m

let reset = Monitor.reset
let changed = Monitor.is_set
