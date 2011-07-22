(** Abstract representation of colors (suitable for RGBA). *)

type t
(** The type for colors*)

val rgb : float -> float -> float -> t
(** [rgb r g b] creates the color with transparency [~a], red
    component [r], green component [g] and blue component [b]. All
    values must be between [0.] and [1.]; raises [Invalid_argument]
    otherwise. *)

val rgba : float -> float -> float -> float -> t
(** [rgba r g b a] creates the color with transparency [~a], red
    component [r], green component [g] and blue component [b]. All values
    must be between [0.] and [1.]; raises [Invalid_argument] otherwise.*)

val r : t -> float
(** Returns the red component of a color.*)

val g : t -> float
(** Returns the green component of a color.*)

val b : t -> float
(** Returns the blue component of a color.*)

val a : t -> float
(** Returns the transparency (alpha) component of a color.*)

val get_rgb : t -> float * float * float
(** Equivalent to ([r t],[g t],[b t]).*)

val get_rgba : t -> float * float * float * float
(** Equivalent to ([r t],[g t],[b t], [a t]).*)

val black : t
val red : t
val green : t
val blue : t
val yellow : t
val magenta : t
val cyan : t
val white : t
(** Predefined colors.*)

(** {2 Merging colors} *)

(** Different ways of merging colors.  See
    http://cairographics.org/operators/ for more explanations.*)
type operator =
| Over (** Transparency and color components are mixed in such a way
           that it corresponds to putting the second color over the first*)
| Source (** First color completely ignored. *)
| Clear (** Inhibits all colors *)
| In (** RGB components as the second color, A component product of
         the two A components. So, a transparent color result if the
         first one was transparent.*)
| Out (** RGB components as the second color, A component product of
          the second A component with (1 - A) first component. So, a
          transparent color result if the first one was opaque.*)
| Atop (** Transparency of the first color is the final transparency;
           mixes RGB components.*)
| Dest (** Second color completely ignored. (<-> SOURCE)*)
| Dest_Over (** Transparency and color components are mixed in such a
                way that it corresponds to putting the first color over the
                second. (<-> OVER)*)
| Dest_In (** RGB components as the first color, A component product of
              the two A components. So, a transparent color result if the
              second one was transparent. (<-> IN)*)
| Dest_Out (** RGB components as the first color, A component product
               of the first A component with (1 - A) second
               component. So, a transparent color result if the
               second one was opaque. (<-> OUT)*)
| Dest_Atop (** Transparency of the second color is the final transparency;
                mixes RGB components. (<-> ATOP)*)
| Xor (** Same mix of color than OVER, but transparency will be more
          important.*)
| Add (** RGB components: ponderated sum of RGB components, with
          transparency. Resulting A is the sum of transparencies
          (bounded to 1. if necessary).*)
| Saturate (** Same as ADD, but the sum for RGB components shrinks
               the ponderation the first color components (coeff:
               min (first A, 1 - second A)) *)


val add : ?op:operator -> t -> t -> t
(** Adds the first color to the second color, according to the
    operator [op] (default : [Over]).*)

(* TODO add lighten function *)


