open Archimedes
open Printf

type t1 = {cr:Backend.t}

let make1 ~dirs s w h =
  let cr = Backend.make ~dirs s w h in
  {cr = cr}

let set_matrix1 t = Backend.set_matrix t.cr

let close1 t =
  Unix.sleep 1;
  Backend.close t.cr

type t2 = {cr:t1}

let make2 ~dirs s w h =
  let cr = make1 ~dirs s w h in
  {cr = cr}

let set_matrix2 t = set_matrix1 t.cr

let close2 t = close1 t.cr

let () =
  let m = {Backend.xx=1.;xy=2.;yx=3.;yy=4.;x0=5.;y0=6.} in
  let f s =
    try
      printf "Direct backend:%!";
      let cr = Backend.make ~dirs:[ "../src"; "./src"] s 100. 100. in
      Backend.set_matrix cr m;
      printf "closing...%!";
      Backend.close cr;
      printf "OK.\n%!";
      printf "Type 1:%!";
      let t1 = make1 ~dirs:[ "../src"; "./src"] s 100. 100. in
      set_matrix1 t1 m;
      printf "closing...%!";
      close1 t1;
      printf "OK.\n%!";
      printf "Type 2:%!";
      let t2 = make2 ~dirs:[ "../src"; "./src"] s 100. 100. in
      set_matrix2 t2 m;
      printf "closing...%!";
      close2 t2;
      printf "OK.\n%!";
    with Backend.Error e ->
        print_string (Backend.string_of_error e);
        exit 1
  in List.iter f
       ["graphics";"tikz __.tex";"cairo PDF __.pdf";"cairo PNG __.png"]


(*Local Variables:*)
(*compile-command: "ocamlopt -o cairo_backend.com -I ../src dynlink.cmxa bigarray.cmxa archimedes.cmxa cairo_backend.ml && ocamlc -g -o cairo_backend.exe dynlink.cma bigarray.cma -I ../src archimedes.cma cairo_backend.ml"*)
(*End:*)
