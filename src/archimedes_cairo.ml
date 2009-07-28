(* File: archimedes_cairo.ml

   Copyright (C) 2009

     Bertrand Desmons <Bertrand.Desmons@student.umons.ac.be>
     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(** Cairo Archimedes plugin *)

open Cairo

module B : Backend.Capabilities =
struct
  include Cairo
  let name = "cairo"

  type t = Cairo.context
  let path_extents = Cairo.Path.extents
  let close_path = Cairo.Path.close

  (* Same type (same internal representation), just in different modules *)
  let set_line_cap cr c = set_line_cap cr (Obj.magic c : Cairo.line_cap)
  let get_line_cap cr = (Obj.magic(get_line_cap cr) : Backend.line_cap)

  let set_line_join cr j = set_line_join cr (Obj.magic j : Cairo.line_join)
  let get_line_join cr = (Obj.magic(get_line_join cr) : Backend.line_join)

  let path_extents cr = (Obj.magic (path_extents cr) : Backend.rectangle)

  let set_dash cr ofs arr = set_dash cr ~ofs arr

  let set_color cr color = ()

  let text cr ~size ~x ~y txt =
    ()

  (* FIXME: better error message for options *)
  let make ~options width height =
    let surface =match options with
      | ["PDF"; fname] -> PDF.create fname width height
      | ["PNG"; _] -> (* We need to modify the close function *)
          Image.create Image.ARGB32 (truncate width) (truncate height)
      | [""] -> (* interactive display. FIXME: when ready *)
          Image.create Image.ARGB32 (truncate width) (truncate height)
      | _ -> failwith "Archimedes_cairo.make" in
    Cairo.create surface

  let close ~options cr =
    let surface = Cairo.get_target cr in
    (match options with
     | ["PNG"; fname] -> PNG.write surface fname;
     | _ -> ());
    Surface.finish surface
end

let () =
  let module U = Backend.Register(B)  in ()



(* Local Variables: *)
(* compile-command: "make -k archimedes_cairo.cmo" *)
(* End: *)
