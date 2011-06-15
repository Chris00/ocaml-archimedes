open Archimedes
module B = Backend
module T = Coord_handler
(*module A = Axes.Print(T)*)
let () =
  let f s =
    Printf.printf "Backend:%!";
    (try
       Printf.printf "open %!";
       let t = B.make ~dirs:[ "./src";"../src"] s 150. 150. in
       Printf.printf "Stop 1s... %!";
       (*Unix.sleep 1;*)
       Printf.printf "close %!";
       B.close t
     with
     | B.Error e ->
         Printf.printf "%s" (B.string_of_error e);
         exit 1);
    Printf.printf "ok. \nCH:%!";
    (try
      Printf.printf "open %!";
      let cr = B.make ~dirs:[ "./src";"../src"] s 150. 150. in
      let t = T.use cr in
      Printf.printf "Stop 1s... %!";
      (*Unix.sleep 1;*)
      Printf.printf "close %!";
      B.close cr
    with
      T.Error e ->
        Printf.printf "%s" (T.string_of_error e);
        exit 0
    | B.Error e ->
        Printf.printf "%s" (B.string_of_error e);
        exit 0);
    Printf.printf "ok. \nCHdir:%!";
    (try
      Printf.printf "open %!";
      let t = T.make ~dirs:[ "./src";"../src"] s 150. 150. in
      Printf.printf "Stop 1s... %!";
      (*Unix.sleep 1;*)
      Printf.printf "close %!";
      T.close t
    with
      T.Error e ->
        Printf.printf "%s" (T.string_of_error e);
        exit 0
    | B.Error e ->
        Printf.printf "%s" (B.string_of_error e);
        exit 0);
    Printf.printf ">>Finished.\n%!";
  in List.iter f ["graphics";"cairo PDF _.pdf";(*"cairo PNG _.png"*)]

(*Local Variables:*)
(*compile-command: "ocamlopt -o test_handle.com -I ../src unix.cmxa dynlink.cmxa bigarray.cmxa archimedes.cmxa test_handle.ml && ocamlc -g -o test_handle.exe unix.cma dynlink.cma bigarray.cma -I ../src archimedes.cma test_handle.ml"*)
(*End:*)
