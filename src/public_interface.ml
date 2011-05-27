(** Create a file with the public interface for Archimedes.

    A single file with the whole documentation is convenient to search
    through.  Moreover, pack will use this interface to hide some
    submodules. *)

open Printf

let include_module name =
  let fh = open_in (String.lowercase name ^ "_public.mli") in
  try
    while true do
      let l = input_line fh in
      if l = "module type T = sig" then
        printf "module %s :\nsig\n" name
      else printf "%s\n" l
    done
  with End_of_file ->
    printf "\n";
    close_in fh

let section msg =
  printf "(**************************************************************\
    **********)\n(** {2 %s} *)\n\n" msg

let () =
  printf "(** A 2D plotting library with various backends. *)\n\n";
  include_module "Color";

  section "Affine transformations";
  include_module "Matrix";

  section "Registering backends and extending the library";
  include_module "Backend";
  include_module "Coordinate";
  include_module "Pointstyle";
  include_module "Path";
  include_module "Tics";
  include_module "Viewport";
  include_module "Arrows";
  include_module "Axes";
  include_module "Sampler";
  include_module "Iterator";
  include_module "Plot"
