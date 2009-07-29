type t
(**The type for colors*)

val color : ?a:float -> float -> float -> float -> t
(**[color ~a r g b] creates the color with transparency [~a], red
component [r], green component [g] and blue component [b]. All values
must be between [0.] and [1.]; raises [Invalid_argument] otherwise.*)

val red : t -> float
(**Returns the red component of a color.*)

val green : t -> float
(**Returns the green component of a color.*)

val blue : t -> float
(**Returns the blue component of a color.*)

val alpha : t -> float
(**Returns the transparency (alpha) component of a color.*)

type operator =
    OVER
  | SOURCE
  | CLEAR
  | IN
  | OUT
  | ATOP
  | DEST
  | DEST_OVER
  | DEST_IN
  | DEST_OUT
  | DEST_ATOP
  | XOR
  | ADD
  | SATURATE

val add : ?op:operator -> t -> t -> t

(*Local variables:*)
(*compile-command: "make -k byte native"*)
(*End:*)
