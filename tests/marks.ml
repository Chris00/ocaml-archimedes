open Archimedes
module P = Plot.Generic

(* The names of all implemented markers. *)
let markers =
  [| "X";   "-";   "|";   "o";
     "O";   "+";   "s";   "S";
     "d";   "D";   "v";   "^";
     ">";   "<";   "-v";   "^-";
     "|>";  "<|";  "--v"; "^--";
     "||>"; "<||"; "*";   "p";
     "P";   "h";   "H";   "tic_up";
     "tic_down"; "tic_left"; "tic_right"; "" |]

let draw backend =
  let p = P.make backend 250. 150. ~dirs:[ "../src"; "./src"] in
  P.set_mark_size p 10.;
  P.set_line_width p 10.;
  (* P.rectangle p (-1.) (-1.) 10. 6.; *)
  (* P.stroke p; *)
  for i = 0 to Array.length markers - 1 do
    let i1 = i / 8 in
    let i2 = i - (i1 * 8) in
    P.move_to handle (float i2) (float i1);
    P.render handle markers.(i);
  done;
  P.close handle


let () =
  let bk =
    if Array.length Sys.argv > 1 then [Sys.argv.(1)]
    else [ "tikz functions.tex";
           "graphics hold";
           "cairo PNG functions.png";
           "cairo PDF functions.pdf" ]
  in
  try List.iter draw bk
  with Backend.Error e ->
    print_endline (Backend.string_of_error e);
    exit 1
