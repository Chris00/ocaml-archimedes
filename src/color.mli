(** Abstract representation of colors (suitable for RGBA). *)

type t
(** Represent a color (immutable). *)

val rgb : float -> float -> float -> t
(** [rgb r g b] creates the color with transparency [~a], red
    component [r], green component [g] and blue component [b]. All
    values must be between [0.] and [1.]; raises [Invalid_argument]
    otherwise. *)

val rgba : float -> float -> float -> float -> t
(** [rgba r g b a] creates the color with transparency [~a], red
    component [r], green component [g] and blue component [b]. All values
    must be between [0.] and [1.]; raises [Invalid_argument] otherwise.*)

val int : int -> t
(** [int c] returns a color from its specification as an integer whose
    value is [0xRRGGBB] where [R], [G] and [B] are hexadecimal
    digits giving the red, green, and blue components of that color.

    It is the form used by [Graphics]. *)

val hue : float -> t
(** [hue h] returns a color of given hue [h] in the interval \[0 .. 360.\[
    ([h] is reduced modulo 360.) and of maximal luminance. *)

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

val luminance : t -> float
(** @return the luminance of the color.  See
    e.g. {{:http://en.wikipedia.org/wiki/Luminance_%28relative%29}Wikipedia}.
*)

(** {3 Predefined colors} *)

val black : t
val red : t
val green : t
val blue : t
val yellow : t
val magenta : t
val cyan : t
val white : t
val dark_slate_grey : t

val colors : t list
(** The list of all predefined colors. *)

(** {4 Shades of Blue} *)

val deep_sky_blue : t
val dodger_blue : t
val aquamarine : t
val light_blue : t
val medium_blue : t
val navy_blue : t
val royal_blue : t

(** {4 Shades of Brown} *)

val burlywood : t
val chocolate : t
val tan : t

(** {4 Shades of Green} *)

val dark_green : t
val dark_olive_green : t
val forest_green : t
val green_yellow : t
val sea_green : t

(** {4 Shades of Orange} *)

val dark_orange : t
val peach_puff : t
val coral : t
val orange : t

(** {4 Shades of Red} *)

val hot_pink : t
val indian_red : t
val light_pink : t
val misty_rose : t
val orange_red : t
val firebrick : t

(** {4 Shades of Violet} *)

val dark_orchid : t
val lavender_blush : t
val plum : t
val orchid : t
val purple : t
val thistle : t

(** {4 Shades of White} *)

val antique_white : t
val old_lace : t
val ivory : t
val linen : t
val wheat : t
val white_smoke : t

(** {4 Shades of Yellow} *)

val lemon_chiffon : t
val light_goldenrod : t
val cornsilk : t
val gold : t

(** {4 Shades of black} *)

val light_gray : t
val gainsboro : t
val silver : t
val trolley_grey : t

(** {3 Merging colors} *)

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

(** {3 Variations of a given color} *)

val lighten : t -> float -> t
(** [lighten c v] Lighten the color [c] of [v] percent. 0 corresponds to
    the same color, 1 corresponds to the white color. *)

val darken : t -> float -> t
(** [darken c v] Darken the color [c] of [v] percent. 0 correspond to the
    same color, 1 corresponds to the black color. *)

val highest_contrast_bw : t -> t
(** returns black or white depending on which of the two is better to
    write on the given color. *)
