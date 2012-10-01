(** Windows Bitmap format. *)

type color = int
(** A color type compatible with the [Graphics] module. *)

val write : string -> color array array -> unit
(** [write fname img] write the image [img] in the file [fname] in
    Bitmap format. *)

;;
