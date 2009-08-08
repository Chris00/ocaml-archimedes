(**Axes management and dynamical loading of other axes management*)

(**{2 Axes interface}*)
module type Axes =
  sig
    val name : string
      (**Get a name to be registered. You cannot use "default" because
         this is already used.*)
    type t
      (**The principal type to make axes.*)
    val make : string list -> t
      (**Given a list of option, makes a [t].*)
    val print_axes :
      t ->
      ?xmin:float ->
      ?xmax:float ->
      ?ymin:float ->
      ?ymax:float ->
      ?color_axes:Color.t -> ?color_labels:Color.t -> Layer.t -> unit
      (**Function to print axes on a layer.*)
  end

type t

(**{2 Default implementation}*)
(**This module provides a default implementation to make axes.*)
module Default :
sig
  include Axes
  type data =
      Graph of int * int
    | Tics of float * int

  type ticstyle = Pointstyle.Default.t

  type mode =
      Rectangle
    | Two_lines of float * float
end

module Register : functor (A : Axes) -> sig  end
  (**This module registers the functions in [A] under the name
     [A.name]. This operation is done as a {i side effect}.

     In the same way as registering backends, the registration can be
     performed by invoking [let module U = Register(A) in ()]. *)

exception Axes_error of string
  (**Exception raised by [make].*)

val make : string -> string list -> t
  (**[make name options] creates a [t] whose background is the type
     defined in the module registered as [name], and set with the list
     [options]. Raises [Axes_error(name)] if there's no Axes module
     registered into [name].*)

val make_default :
  ?mode:Default.mode ->
  ?ticx:Default.ticstyle -> ?minticx:Default.ticstyle -> Default.data ->
  ?ticy:Default.ticstyle -> ?minticy:Default.ticstyle -> Default.data -> t
  (**To make a [t] whose background is a [Default.t], you can use this shortcut.*)

val print_axes :
  t ->
  ?xmin:float ->
  ?xmax:float ->
  ?ymin:float ->
  ?ymax:float -> ?color_axes:Color.t -> ?color_labels:Color.t -> Layer.t -> unit
(**Print axes according to the parameters stored in [t] into the
layer. If some option is not given, then the layer provides the
default. For example, if [xmin] is not provided, then it is set to the
layer minimal abscissa value.*)
