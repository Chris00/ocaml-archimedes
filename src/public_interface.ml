(** Create a file with the public interface for Archimedes.

    A single file with the whole documentation is convenient to search
    through.  Moreover, pack will use this interface to hide some
    submodules. *)

open Printf

let start_with s p =
  let lenp = String.length p in
  String.length s >= lenp && String.sub s 0 lenp = p

let end_with s p =
  let lens = String.length s and lenp = String.length p in
  lens >= lenp && String.sub s (lens - lenp) lenp = p

let rec skip_simple_comment fh l =
  if not(end_with l "*)") then skip_simple_comment fh (input_line fh)

let rec copy_comment fh l =
  printf "%s\n" l;
  if not(end_with l "*)") then copy_comment fh (input_line fh)

let include_file fname =
  let fh = open_in (Filename.concat "src" fname) in
  let buf = String.create 4096 in
  let len = ref (-1) in
  while !len <> 0 do
    len := input fh buf 0 4096;
    output stdout buf 0 !len;
  done;
  close_in fh

let include_module name =
  let fh = open_in ("src/" ^ (String.lowercase name) ^ ".mli") in
  try
    let module_comment = ref true in
    while true do
      let l = input_line fh in
      if l = "(**/**)" then raise End_of_file;
      if start_with l "(* " then skip_simple_comment fh l
      else if !module_comment && start_with l "(**" then (
        printf "\n"; (* separate comment from previous module *)
        copy_comment fh l;
        printf "module %s :\nsig\n" name;
        module_comment := false;
      )
      else if l = "" then printf "\n"
      else (
        (* If no module comment: *)
        if !module_comment then (
          printf "module %s :\nsig\n" name;
          module_comment := false;
        );
        printf "  %s\n" l
      )
    done
  with End_of_file ->
    printf "end\n";
    close_in fh

let section msg =
  printf "(*-----------------------------------------------------------\
    -----------*)\n(** {2 %s} *)\n\n" msg

let () =
  include_file "archimedes_header.mli";
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
  include_module "Plot";
  include_module "Piechart";
  include_file "archimedes_footer.mli";
