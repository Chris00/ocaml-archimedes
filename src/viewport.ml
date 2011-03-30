
type viewport = {


}

module Axes =
struct
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
    axes: axis list
  }

  type axes_system = {
    dimx: dim;
    dimy: dim;
    mutable viewports: viewport list;
  }
 end
