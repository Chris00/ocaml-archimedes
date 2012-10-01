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
                 (BaseCheck.ocamlfind ()) ["printconf"; "destdir"] in
    match inst with
    | destdir :: _ ->
       (* This value will be substituted into a string; it must be
          properly escaped. *)
       String.escaped destdir
    | _ -> printf "ERROR: 'ocamlfind printconf destdir' output is empty!\n";
          exit 1

let _ = BaseEnv.var_define "ocamlfind_destdir_escaped" get_destdir

(* Variables that will be substituted in strings in conf.ml.ab and so
   must be properly escaped (especially for windows). *)
let _ = BaseEnv.var_define "libdir_escaped"
                           (fun _ -> String.escaped(BaseStandardVar.libdir()))
let _ = BaseEnv.var_define "datarootdir_escaped"
                           (fun _ -> String.escaped(BaseStandardVar.datarootdir()))

(* pkg_cairo2 is not defined when --disable-cairo is used.  However,
   it is required by "src/archimedes_cairo.dep.ab".  Use a dummy
   value, if configure does not set one. *)
let () =
  let args = Array.to_list Sys.argv in
  if List.mem "--disable-graphics" args then (
    let _ = BaseEnv.var_define "pkg_graphics" (fun () -> "disabled") in
    ());
  if List.mem "--disable-cairo" args then (
    let _ = BaseEnv.var_define "pkg_cairo2" (fun () -> "disabled") in
    ())

let () = setup ();;
