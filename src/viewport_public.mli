(** Axes maker and convenient ways to create axes. *)
module type T = sig

  type viewport;

  module Axes : sig
    type labels =
      | Text of string array * float
      | Number
      | Expnumber of float
      | Expnumber_named of float * string
      | Custom of (float -> string)

    type tics =
      | Fixed of float list
      | Fixed_norm of float list
      | Equidistants of int * int
      | Auto

    type sign = Positive | Negative;

    type offset =
      | Relative of float
      | Absolute of float

    type axis = {
      tics: tics;
      offset: float;
      tics_position: sign;
    }

    type dim = {
      mutable x0: float;
      mutable xend: float;
      log: bool;
      orientation: sign;
      (* axes: axis list; *)
}

    type axes_system = {
      dimx: dim;
      dimy: dim;
      mutable viewports: viewport list;
    }

    val make: ?logx:bool -> ?logy:bool -> ?orientationx:sign -> ?orientationy:sign
      -> ?x0:float -> ?xend:float -> ?y0:float -> ?yend:float -> unit -> axes_system


...

  (** A data structure holding ranges.*)
  type ranges = { x1:float; x2:float; y1:float; y2:float}

  type 'a axis
    (** This type stores all information about an axis: major, minor
        tics, their positioning and how to position labels relative to
        tics. *)

  type ('a, 'b) t
    (** This type stores information about a pair of axes. *)

  (** Different axes modes. They all specify which point has to be
      taken into account for the intersection of the axes. This point
      determine the position of tics. *)
  type axes =
      [ `None of bool * bool
          (**No axis will be printed. The bools have to be interpreted
             this way:

             -for the first one, [true] means: minimal abscissa;
             [false]: maximal abscissa.

             -for the second one, same meaning, but on ordinates.*)
      | `Rectangle of bool * bool
          (**A rectangle, whose corners are taken so that it fits the
             zone. The bools have the same meaning as for [`None].*)
      | `Two_lines of float * float
          (**Abscissas axes are represented in a horizontal line,
             ordinates in a vertical line. The pair of floats is
             precisely the intersection of these two lines.*)
      | `Two_lines_rel of float * float
          (**Same as [Two_lines] except that the two floats are
             relative; that is, if [(t,u)] is an argument pair, and
             [xmin, ..., ymax] are the bounds, then the intersection
             is computed as [(xmin +. t *.(xmax -. xmin), ymin +. u
             *. (ymax -. ymin) )].*)
      ]

  (** Type of data to put as labels on major tics. *)
  type data = [
  | `Text_label of string array * float
      (** Labels will be text labels, rotated by the second argument. *)
  | `Number
      (** Use abscissas or ordinates as labels. *)
  | `Expnumber
      (** Labels of the form [10^x] with [x] abscissa or ordinate. *)
  ]

  type tic = [ `P of string ]
      (**Type for tics.*)

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


end
