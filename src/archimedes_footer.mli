(** {3 Initializing Archimedes} *)

val init : ?lines:float -> ?text:float -> ?marks:float ->
  ?bg:Color.t ->
  ?w:float -> ?h:float -> ?dirs:string list -> string list -> Viewport.t
(** [init backend] initializes Archimedes and returns the main viewport
    using the backend specified.  The first element of [backend] is
    the name (case insensitive) of the underlying engine.  It may be
    followed by one or several options.  For example, ["Graphics"] for
    the graphics backend or ["Cairo"; "PNG"; filename] for the Cairo
    backend, using a PNG surface to be saved to [filename].  The empty
    list selects ["Graphics"; "hold"].

    @param w the width of the main viewport (in backend's unit).

    @param h the height of the main viewport (in backend's unit).

    @param bg the color of the background.  Default: {!Color.white}.

    @param lines the width of the lines.  Default: [1.] which
    corresponds to a line width on the backend of [min w h /. 500.].

    @param text the size of the text.  Default: [12.] which
    corresponds to puting about 42 lines of text in [min w h] height.

    @param marks the size of the marks.  Default: [7.] which
    corresponds packing about 100 marks in [min w h].

    @param dirs a list of directories where Archimedes looks for
    libraries (cma or cmxs) for dynamically loaded backends.  The
    default is the directory where the backends that come with
    Archimedes were installed.
*)

val backend_of_filename : string -> string list
(** Selects a backend according to the filename suffix.  If the suffix
    is not matched (this in particular for [""]), the graphics backend
    is selected. *)

val close : Viewport.t -> unit

val set_color : Viewport.t -> Color.t -> unit
(** Alias for {!Viewport.set_color}. *)

val set_line_width : Viewport.t -> float -> unit
(** Alias for {!Viewport.set_line_width}. *)
