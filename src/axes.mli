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


type ranges =
    private {mutable xmin:float;
             mutable ymin:float;
             mutable xmax:float;
             mutable ymax:float}

module Ranges: sig
  val make : float -> float -> ranges

  val update : ranges -> float -> float -> unit

  val to_rect: ranges -> Backend.rectangle
  val of_rect: Backend.rectangle -> ranges
end

exception Not_available
  (**Raised when some polymorphic variant has no definition of the
     ways to work with (this is especially raised when you define a
     new variant without providing how to manage with it). *)

type axes =
    [ `None of bool * bool
    | `Rectangle of bool * bool
    | `Two_lines of float * float
    | `Two_lines_rel of float * float
    ]

(**Different axes modes. They all specify which point has to
   be taken into account for the intersection of the
   axes. This point determine the position of tics.*)

val print_axes :
  [> axes] -> ranges -> Backend.t -> unit
  (**Given the axes mode, the bounds and a backend to draw on,
     prints the corresponding axes on the backend.*)

val axes_meeting :
  [> axes] -> ranges -> float * float
    (**Returns the point where the axes meet.*)


type tic = [ `P of Pointstyle.name ]
    (**Type for tics.*)

val print_tic : Backend.t -> [> tic] -> unit
  (**Given a backend and a tic, prints the tic.*)

val tic_extents : [> tic] -> Backend.rectangle
    (**Returns the extents for the given tic. (This is needed to place
       the labels correctly.)*)

type label
  (**The type which manages with labels on axes.*)

(*  val tic_label_extents : Backend.rectangle -> label option ->
    float -> float -> Backend.text_position -> Backend.t -> Backend.rectangle
    (**[tic_label_extents tic_extents label x y pos b] returns the global
       extents of a (major or minor) tic, with its (eventual) label,
       as if all is done at the point [(x,y)]. The backend [b] is used
       only to determine the extents ([box]) of the label.*)*)

type label_collection
  (**Storing several labels*)


type data =
    [ (*`Label of label array
        (**Labels already known*)*)
    | `Text_label of string array * float
        (**Labels will be text labels, rotated by the second argument*)
    | `Number
        (**Use abscissas or ordinates as labels*)
    | `Expnumber
        (**Labels of the form [10^x] with [x] abscissa or ordinate*)
    ]
      (**This type informs on which type of data we want as labels.*)

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

type loc_tics =
    [ `Fixed_rel of (float * bool) list
        (**List of pairs [(x, major)] with [x] a number between 0 and
           1, specifying the relative position of the tic and [major]
           indicating whether the tic is major.*)
    | `Fixed_abs of ((float * bool) list)
    | `Linear_variable of int array
        (**The [i]th element of the array specifies the number of
           minor tics between the [i]th major tic and the [i+1]th
           one (starting count at 0). All tics are placed linearly;
           that is, if the length of the axis is [len], then the
           [i]th tic (among all tics, starting to count at 0) is
           placed at distance [i /. len].*)
    | `Linear of int * int
        (**[`Linear(majors, minors)]: Fixed number of major tics,
           and number of minor tics between two consecutive major
           tics. They are all placed linearly.*)
    | `Logarithmic of int * int
        (**Same as [`Linear] except that the minor tics are placed
           in a logarithmic scale.*)
    | `Auto_linear
    ]


(**Convenient ways to specify where we want tics.*)

val get_position : [>loc_tics] -> label_collection -> tic_position
    (**[get_position loc labels] transforms the [loc] to obtain a
       [position]. When a label is required, it is picked in the [labels]
       argument; if there's too few labels, no label is provided (so the
       last major tics can be without label in this case).
    *)

type 'a axis
type ('a, 'b) t

(*
val make_axis : bool ->
  ([> tic] as 'a) -> ([>data] as 'b) -> Backend.text_position -> 'a ->
  ?get_labels:(bool -> 'b -> label_collection) ->
  ?get_position:(([>loc_tics] as 'c) -> label_collection -> tic_position) ->
  'c -> 'a axis*)

val make_xaxis :
  ([> tic] as 'a) -> ([>data] as 'b) -> text_position -> 'a ->
  ?get_labels:(bool -> 'b -> label_collection) ->
  ?get_position:(([>loc_tics] as 'c) -> label_collection -> tic_position) ->
  'c -> 'a axis
val make_yaxis :
  ([> tic] as 'a) -> ([>data] as 'b) -> text_position -> 'a ->
  ?get_labels:(bool -> 'b -> label_collection) ->
  ?get_position:(([>loc_tics] as 'c) -> label_collection -> tic_position) ->
  'c -> 'a axis
(**[make_*axis major data pos minor loc] makes an axis whose major
   tics will be [major], minor tics [minor], positioned using [loc]
   and labels will be positioned using [pos]. The optional arguments are used
   to define the positionment of tics (see the corresponding default functions).*)

val make : ([>axes] as 'a) -> 'b axis -> 'b axis -> ('a, 'b) t
  (**Given two axes and a way to render them, makes a [t].*)


val get_margins :
  ([> axes ] as 'a, [> tic] as 'b) t ->
  ?axes_meeting:('a -> ranges -> float * float) ->
  ?tic_extents:('b -> Backend.rectangle) ->
  ranges -> Backend.t -> Backend.rectangle * Backend.rectangle
  (**Returns the margins needed to print the axes. Returns a pair of
     [Backend.rectangle], the first one represents the extents for the X
     axis, and the second one the extents for the Y axis.*)

val print :
  ([> axes] as 'a, [> tic] as 'b) t ->
  lines:Coordinate.t ->
  ranges:ranges ->
  ?print_axes:('a -> ranges -> Backend.t -> unit) ->
  ?axes_meeting:('a -> ranges -> float * float) ->
  ?print_tic:(Backend.t -> 'b -> unit) -> Backend.t -> unit
    (**Prints axes, following the parameters stored in [t] and the
       optional arguments, if given.*)
