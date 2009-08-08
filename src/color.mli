(**Colors representation.*)

type t
(**The type for colors*)

val make : ?a:float -> float -> float -> float -> t
(**[color ~a r g b] creates the color with transparency [~a], red
component [r], green component [g] and blue component [b]. All values
must be between [0.] and [1.]; raises [Invalid_argument] otherwise.*)

val r : t -> float
(**Returns the red component of a color.*)

val g : t -> float
(**Returns the green component of a color.*)

val b : t -> float
(**Returns the blue component of a color.*)

val a : t -> float
(**Returns the transparency (alpha) component of a color.*)

val get_rgb : t -> float * float * float
(**Equivalent to ([r t],[g t],[b t]).*)

val get_rgba : t -> float * float * float * float
(**Equivalent to ([r t],[g t],[b t], [a t]).*)

val black : t
val red : t
val green : t
val blue : t
val yellow : t
val purple : t
val cyan : t
val white : t
(**Predefined colors.*)

(**{2 Merging colors}*)
type operator =
    OVER(**Transparency and color components are mixed in such a way
           that it corresponds to putting the second color over the first*)
  | SOURCE(**First color completely ignored.*)
  | CLEAR(**Inhibits all colors*)
  | IN(**RGB components as the second color, A component product of
         the two A components. So, a transparent color result if the
         first one was transparent.*)
  | OUT (**RGB components as the second color, A component product of
           the second A component with (1 - A) first component. So, a
           transparent color result if the first one was opaque.*)
  | ATOP (**Transparency of the first color is the final transparency;
            mixes RGB components.*)
  | DEST(**Second color completely ignored. (<-> SOURCE)*)
  | DEST_OVER(**Transparency and color components are mixed in such a
                way that it corresponds to putting the first color over the
                second. (<-> OVER)*)
  | DEST_IN(**RGB components as the first color, A component product of
              the two A components. So, a transparent color result if the
              second one was transparent. (<-> IN)*)
  | DEST_OUT(**RGB components as the first color, A component product of
               the first A component with (1 - A) second component. So, a
               transparent color result if the second one was opaque. (<-> OUT)*)
  | DEST_ATOP(**Transparency of the second color is the final transparency;
                mixes RGB components. (<-> ATOP)*)
  | XOR (**Same mix of color than OVER, but transparency will be more important.*)
  | ADD (**RGB components: ponderated sum of RGB components, with
           transparency. Resulting A is the sum of transparencies (bounded to
           1. if necessary).*)
  | SATURATE (**Same as ADD, but the sum for RGB components shrinks
                the ponderation the first color components (coeff: min (first A, 1 -
                second A)) *)
      (**Different ways of merging colors. See
         http://cairographics.org/operators/ for more explanations.*)


val add : ?op:operator -> t -> t -> t
  (**Adds the first color to the second color, according to the operator
     [op] (default : [OVER]).*)
