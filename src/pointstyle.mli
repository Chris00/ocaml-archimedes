module type Style =
  sig
    val name : string
      (**Name for the point style, which will be used in registering.*)
    type t
      (**Point styles*)
    val make : string -> t
      (**Makes a point style, using the string as options. For
         example, in the [Default] module, an empty string gives
         [Default.NONE], vhereas the string "| 2." will raise to a
         [Default.VERT 2.] *)
    val point : t -> float -> float -> Backend.t -> unit
      (**Given a point style and a backend, makes the point style at
         the point specified by the floats; it is expressed in backend
         coordinates.*)
  end

module Default :
sig
  type style =
      (**All distance and length are expressed in {i device} coordinates. *)
    | NONE (**Don't make any style. String option to get it:""*)
    | X of float (**X centered at the point, the float argument is the
                    width of the square containing this X. String
                    option to get it:"X float".*)
    | HORIZ of float (**Horizontal line centered at the point, the
                        argument is the length of this line. String
                        option to get it: "- float".*)
    | VERT of float (**Vertical line centered at the point, the
                       argument is the length of this line. String
                       option to get it: "| float".*)
    | TIC_UP of float (**Vertical line whose bottom is the point.  The
                         float argument is the length of this line. String option: "TU
                         float".*)
    | TIC_DOWN of float(**Vertical line whose top is the point.  The
                          float argument is the length of this line. String option: "TD
                          float".*)
    | TIC_LEFT of float(**Horizontal line whose right end is the point.  The
                          float argument is the length of this line. String option: "TU
                          float".*)
    | TIC_RIGHT of float(**Horizontal line whose left end is the point.  The
                          float argument is the length of this line. String option: "TU
                          float".*)


  include Style with type t = style
end

type t

exception PSError of string
val make : string -> t
  (**Makes a point style according to the string specified. This
     string must be of the following form:

     [name^" "^options]

     where [name] is the name of the registered point style you want
     to use, and [options] are the options associated.

     For instance, [make "default X 3."] will make the default point
     style [X(3.)] *)

val point : t -> float -> float -> Backend.t -> unit
  (**Given a point style and a backend, makes the point style at the
         point specified by the floats; it is expressed in backend
         coordinates.*)

val make_point : string -> float -> float -> Backend.t -> unit
  (**[make_point pointstyle x y handle] is equivalent to
     [let t = make pointstyle in point t x y handle].*)
val points: t -> (float * float) list -> Backend.t -> unit
  (**Makes the point style at all points in the list.*)
