(* File: archimedes_toplevel.ml

   Copyright (C) 2009

   Christophe Troestler <Christophe.Troestler@umons.ac.be>
   WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(** When working in the toplevel, it is convenient to have a
    simplified interface so one can plot without having to declare any
    handle.  The usual Archimedes functions will remain available in
    case several concurrent plots are needed. *)

(* One must pass the usual Backend registration mechanism of Backend
   in the toplevel because, for example, loading graphics in this way
   raises: Assert_failure ("bytecomp/dll.ml", 109, 4).

   We use the toplevel mechanism to load the modules. *)

open Printf

let modules = [ "graphics.cma";
                (* "dynlink.cma";*) (* already loaded by META *)
                "archimedes.cma";
                "archimedes_graphics.cma";
              ]

let eval_string
    ?(print_outcome = false) ?(err_formatter = Format.err_formatter) str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome err_formatter phrase

let rec load_modules = function
  | [] -> true
  | cma :: tl ->
      eval_string(sprintf "#load %S;;" cma) && load_modules tl

let () =
  if load_modules modules then (
    if eval_string "module A = Archimedes;;" then
      Format.eprintf "Module Archimedes loaded and aliased as A.@."
    else
      Format.eprintf "Problem aliasing the Archimedes module!@."
  )
  else
    Format.eprintf "Problem loading Archimedes module@."
