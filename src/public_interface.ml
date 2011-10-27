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

let include_file ?(as_module=true) ?(header="") name =
  let fh = open_in ("src/" ^ (String.lowercase name) ^ ".mli") in
  try
    let copy_line = ref true in
    let module_comment = ref true in (* first doc comment *)
    let prefix =
      if as_module then "  "
      else (printf "%s\n" header; "") in
    while true do
      let l = input_line fh in
      if l = "(**/**)" then
        copy_line := not !copy_line
      else if !copy_line then (
        if start_with l "(* " then skip_simple_comment fh l
        else if as_module && !module_comment && start_with l "(**" then (
          printf "\n"; (* separate comment from previous module *)
          copy_comment fh l;
          printf "module %s :\nsig\n%s" name header;
          module_comment := false;
        )
        else if l = "" then printf "\n"
        else (
          (* If no module comment: *)
          if as_module && !module_comment then (
            printf "module %s :\nsig\n%s" name header;
            module_comment := false;
          );
          printf "%s%s\n" prefix l
        )
      )
    done
  with End_of_file ->
    if as_module then printf "end\n";
    close_in fh

let section msg =
  printf "(*-----------------------------------------------------------\
    -----------*)\n(** {2 %s} *)\n\n" msg

let () =
  include_file "archimedes_header" ~as_module:false;

  section "Affine transformations";
  include_file "Matrix";

  section "Base elements of a plot";
  include_file "Color";
  include_file "Path"
    ~header:"  type t = Archimedes_internals.Path.t\n  \
               (** Abstract mutable path. *)";

  section "Registering backends";
  include_file "Backend";

  section "Managing viewports";
  include_file "Coordinate";
  include_file "Viewport";

  section "Sampling functions";
  include_file "Sampler";

  section "High level functions";
  include_file "Marker";
  include_file "Arrows";
  include_file "Tics";
  include_file "Axes";
  include_file "archimedes_footer" ~as_module:false;
  include_file "Plot" ~as_module:false;
  section "Pie-charts";
  include_file "Piechart";
