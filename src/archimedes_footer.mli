(** A 2D plotting library with various backends. *)


val init : ?lines:float -> ?text:float -> ?marks:float ->
  ?w:float -> ?h:float -> ?dirs:string list -> string list -> Viewport.t
(** [init backend] initializes Archimedes and returns a main viewport
    using the backend specified.  The first element of [backend] is
    the name (case insensitive) of the underlying engine.  It may be
    followed by one or several options.  For example, ["Graphics"] for
    the graphics backend or ["Cairo"; "PNG"; filename] for the Cairo
    backend, using a PNG surface to be saved to [filename].  The empty
    list selects the graphics backend.

    @param lines the width of the lines (default: 1. corresponds to
    filling a biggest square of the viewport with 500 lines)

    @param text the size of the text in (default: 12. corresponds to
    filling a biggest square of the viewport with about 42 lines of
    text)

    @param marks the size of the marks in pixels (default:
    1. corresponds to filling a biggest square the viewport with 100
    "lines of marks")

    @param w the width of the main viewport (in backend's unit)

    @param h the height of the main viewport (in backend's unit)

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


val fx : ?tlog:bool ->
  ?strategy:Sampler.strategy -> ?criterion:Sampler.criterion ->
  ?min_step:float -> ?nsamples:int ->
  ?fill:bool -> ?fill_base:(float -> float) -> ?fillcolor:Color.t ->
  ?pathstyle:Plot.pathstyle -> Viewport.t ->
  (float -> float) -> float -> float -> unit
