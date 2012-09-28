(* OASIS_START *)
(* DO NOT EDIT (digest: 7f47a529f70709161149c201ccd90f0b) *)
#use "topfind";;
#require "oasis.dynrun";;
open OASISDynRun;;
(* OASIS_STOP *)

open Printf

let skip = [
  (* Common configuration *)
  "tests.ml";
  "tests_common.ml";
]

let is_test fname =
  Filename.check_suffix fname ".ml" && not(List.mem fname skip)

let tests_list () =
  let files = Array.to_list (Sys.readdir "tests") in
  let files = List.filter is_test files in
  let files = List.sort String.compare files in
  let to_string f =
    let name = String.capitalize(Filename.chop_suffix f ".ml") in
    sprintf "(%S, %s.description, %s.draw)" name name name in
  String.concat ";\n  " (List.map to_string files)

let _ = BaseEnv.var_define "tests_list" tests_list

(** Determine the installation directory.  It is needed to dynamically
    find the backend modules. *)
let get_destdir() =
  (* If OCAMLFIND_DESTDIR is set, use it *)
  try
    Sys.getenv "OCAMLFIND_DESTDIR"
  with Not_found ->
    let inst = OASISExec.run_read_output
                 ~ctxt:!BaseContext.default
                 (BaseCheck.ocamlfind ()) ["install"; "-help"] in
    match inst with
    | _ :: destdir :: _ ->
      (try
          let path_beg = String.index destdir '/' in
          let cl_brace = String.index destdir ')' in
          String.sub destdir path_beg (cl_brace - path_beg)
        with _ ->
          printf "ERROR: The line %S does not allow to determine the \
            installation directory.\n" destdir;
          exit 1)
    | _ ->
      printf "ERROR: 'ocamlfind install -help' output is incompatible.\n";
      exit 1

let _ = BaseEnv.var_define "ocamlfind_destdir" get_destdir

(* pkg_cairo2 is not defined when --disable-cairo is used.  However,
   it is required by "src/archimedes_cairo.dep.ab".  Use a dummy
   value, if configure does not set one. *)
let () =
  let args = Array.to_list Sys.argv in
  let disable_cairo = List.mem "--disable-cairo" args in
  if disable_cairo then
    let _ = BaseEnv.var_define "pkg_cairo2" (fun () -> "disabled") in
    ()

let () = setup ();;
