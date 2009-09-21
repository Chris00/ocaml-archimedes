open Archimedes
module P = Plot.List

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
  for i = 0 to Array.length markers - 1 do
    P.xy p [float(i mod 8)] [float(i / 8)] ~mark:markers.(i)
  done;
  P.close p


let () =
  let bk =
    if Array.length Sys.argv > 1 then [Sys.argv.(1)]
    else [ "tikz marks.tex";
           "graphics hold";
           "cairo PNG marks.png";
           "cairo PDF marks.pdf" ]
  in
  try List.iter draw bk
  with Backend.Error e ->
    print_endline (Backend.string_of_error e);
    exit 1
