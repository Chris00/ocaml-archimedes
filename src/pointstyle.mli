module type Style =
  sig
    val name : string
      (**Name for the point style, which will be used in registering.*)
    type t
      (**Point styles*)
    val make : string -> t
      (**Makes a point style, using the string as options.*)
    val point : t -> float -> float -> Backend.t -> unit
      (**Given a point style and a backend, makes the point style at
         the point specified by the floats; it is expressed in backend
         coordinates.*)
  end

module Default :
sig
  type style =
    | NONE (**Don't make any style*)
    | X of float (**X centered at the point, the float argument is the
                    width of the square containing this X, expressed
                    in device coordinates.*)
    | HORIZ of float (**Horizontal line centered at the point, the
                        argument is the length of this line, expressed
                        in device coordinates.*)
    | VERT of float (**Vertical line centered at the point, the
                        argument is the length of this line, expressed
                        in device coordinates.*)

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
