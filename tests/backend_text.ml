open Printf
open Archimedes

let half_pi = atan 1.

let () =
  try
    let dirs = [ "./src"; "../src" ] in
    let b = Backend.make "cairo PDF backend_test.pdf" 200. 100. ~dirs in

    Backend.show_text b 100. 50. half_pi Backend.CC "Joy";

    Backend.close b
  with Backend.Error e ->
    eprintf "Backend.Error: %s\n" (Backend.string_of_error e)
