(* File: axes.mli

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

include Axes_public.T

type fixed_ranges =
    private
      {
        mutable xmin:float;
        mutable ymin:float;
        mutable xmax:float;
        mutable ymax:float
      }

module FixedRanges: sig
  val make : float -> float -> fixed_ranges

  val update : fixed_ranges -> float -> float -> bool
    (**Returns [true] if the point really updated the fixed_ranges.*)

  val copy: fixed_ranges -> fixed_ranges
  val to_rect: fixed_ranges -> Matrix.rectangle
  val of_rect: Matrix.rectangle -> fixed_ranges
  val of_ranges: ranges -> fixed_ranges
  val to_ranges: ?xswitch:bool -> ?yswitch:bool -> fixed_ranges -> ranges
end

exception Not_available
  (**Raised when some polymorphic variant has no definition of the
     ways to work with (this is especially raised when you define a
     new variant without providing how to manage with it). *)



val print_axes :
  [> axes] -> ranges -> Backend.t -> unit
  (**Given the axes mode, the bounds and a backend to draw on,
     prints the corresponding axes on the backend.*)

val axes_meeting :
  [> axes] -> ranges -> float * float
    (**Returns the point where the axes meet.*)


val print_tic : Backend.t -> [> tic] -> unit
  (**Given a backend and a tic, prints the tic.*)

val tic_extents : [> tic] -> Matrix.rectangle
    (**Returns the extents for the given tic. (This is needed to place
       the labels correctly.)*)

type label
  (**The type which manages with labels on axes.*)

(*  val tic_label_extents : rectangle -> label option ->
    float -> float -> Backend.text_position -> Backend.t -> rectangle
    (**[tic_label_extents tic_extents label x y pos b] returns the global
       extents of a (major or minor) tic, with its (eventual) label,
       as if all is done at the point [(x,y)]. The backend [b] is used
       only to determine the extents ([box]) of the label.*)*)

type label_collection
  (**Storing several labels*)



exception Too_few_labels

val get_labels: bool -> [> data] -> label_collection
    (**Converts a [data] into a [label_collection], which will be used
       by [get_position] (see below).*)

type tic_position = ranges -> bool ->  Backend.t ->
  (float * float * label option) list
  (**Shortcut. The output [list] contains tuples of the form
     [(x,y,labelopt)], with [(x,y)] a point where we want a tic and
     [labelopt] indicates the (optional) label wanted (it is [None]
     for the minor tics).*)


(**Convenient ways to specify where we want tics.*)

val get_position : [>loc_tics] -> label_collection -> tic_position
    (**[get_position loc labels] transforms the [loc] to obtain a
       [position]. When a label is required, it is picked in the [labels]
       argument; if there's too few labels, no label is provided (so the
       last major tics can be without label in this case).
    *)

(*
  val make_axis : bool ->
  ([> tic] as 'a) -> ([>data] as 'b) -> Backend.text_position -> 'a ->
  ?get_labels:(bool -> 'b -> label_collection) ->
  ?get_position:(([>loc_tics] as 'c) -> label_collection -> tic_position) ->
  'c -> 'a axis*)

val make_xaxis :
  ([> tic] as 'a) -> ([>data] as 'b) -> Backend.text_position -> 'a ->
  ?get_labels:(bool -> 'b -> label_collection) ->
  ?get_position:(([>loc_tics] as 'c) -> label_collection -> tic_position) ->
  ?tic_extents:('a -> Matrix.rectangle) ->
  'c -> 'a axis
val make_yaxis :
  ([> tic] as 'a) -> ([>data] as 'b) -> Backend.text_position -> 'a ->
  ?get_labels:(bool -> 'b -> label_collection) ->
  ?get_position:(([>loc_tics] as 'c) -> label_collection -> tic_position) ->
  ?tic_extents:('a -> Matrix.rectangle) ->
  'c -> 'a axis
  (**[make_*axis major data pos minor loc] makes an axis whose major
     tics will be [major], minor tics [minor], positioned using [loc]
     and labels will be positioned using [pos]. The optional arguments are used
     to define the positionment of tics (see the corresponding default functions).*)

val make : ([>axes] as 'a) -> 'b axis -> 'b axis -> ('a, 'b) t
  (**Given two axes and a way to render them, makes a [t].*)


type margins =
    {left: float; (**Margin to reserve on the left*)
     right: float; (**Margin to reserve on the right*)
     top: float; (**Margin to reserve on the top*)
     bottom: float (**Margin to reserve on the bottom*)
    }
      (**The return type for margins. *)

val get_margins :
  ([> axes ] as 'a, [> tic] as 'b) t ->
  ?axes_meeting:('a -> ranges -> float * float) ->
  normalization:Coordinate.t ->
  lines:float ->
  marks:float ->
  font_size:float ->
  ranges -> Backend.t -> margins * margins
  (**Returns the margins needed to print the axes. Returns a pair of
     [margins], the first one gives the margins needed for the X
     axis, and the second one the margins needed for the Y axis.*)

val print :
  ([> axes] as 'a, [> tic] as 'b) t ->
  normalization:Coordinate.t ->
  lines:float ->
  marks:float ->
  font_size:float ->
  ranges:ranges ->
  ?print_axes:('a -> ranges -> Backend.t -> unit) ->
  ?axes_meeting:('a -> ranges -> float * float) ->
  ?print_tic:(Backend.t -> 'b -> unit) -> Backend.t -> unit
    (**Prints axes, following the parameters stored in [t] and the
       optional arguments, if given.*)
