type size
type t

val make_rel : t -> float -> float -> float -> t
val make_abs : t -> float -> float -> float -> t
val get_lw : t -> float
val get_ts : t -> float
val get_marks : t -> float
val set_rel_lw : t -> float -> unit
val set_rel_ts : t -> float -> unit
val set_rel_ms : t -> float -> unit
val set_abs_lw : t -> float -> unit
val set_abs_ts : t -> float -> unit
val set_abs_ms : t -> float -> unit
