open Archimedes
module H = Handle

let () =
  let markers =
    [| "X";   "-";   "|";   "o";
       "O";   "+";   "s";   "S";
       "d";   "D";   "v";   "^";
       ">";   "<";"  -v";   "^-";
       "|>";  "<|";  "--v"; "^--";
       "||>"; "<||"; "*";   "p";
       "P";   "h";   "H";   "tic_up";
       "tic_down"; "tic_left"; "tic_right"; "" |] in
  let f s =
    Printf.printf "%s\n***************************\n%!" s;
    try
      let handle = H.make ~dirs:[ "../src"; "./src"] s 250. 150. in
      H.set_mark_size handle 10.;
      H.set_line_width handle 10.;
      H.rectangle handle (-1.) (-1.) 10. 6.;
      H.stroke handle;
      for i = 0 to Array.length markers - 1 do
        let i1 = i / 8 in
        let i2 = i - (i1 * 8) in
        H.move_to handle (float i2) (float i1);
        H.render handle markers.(i);
      done;
      H.close handle
    with
      Backend.Error e ->
        print_string (Backend.string_of_error e);
        exit 1
  in List.iter f
       ["tikz marks.tex";
        "cairo PNG marks.png";
        "cairo PDF marks.pdf";
        "graphics"  ]
