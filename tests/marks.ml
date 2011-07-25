include Testing
(*module P = Plot.List*)
module V = Archimedes.Viewport

(* The names of all implemented markers. *)
(*let markers =
  [| "X";   "-";   "|";   "o";
     "O";   "+";   "s";   "S";
     "d";   "D";   "v";   "^";
     ">";   "<";   "-v";   "^-";
     "|>";  "<|";  "--v"; "^--";
     "||>"; "<||"; "*";   "p";
     "P";   "h";   "H";   "tic_up";
     "tic_down"; "tic_left"; "tic_right"; "" |]*)

let markers = Array.of_list (Archimedes.Pointstyle.names ())

let draw backend =
  let vp = Archimedes.init ~w ~h ~dirs backend in
  V.set_mark_size vp 20.;
  V.set_line_width vp 1.;
  for i = 0 to Array.length markers - 1 do
    let x = float (i mod 8) and y = float (i / 8) in
    V.mark vp x y markers.(i)
  done;
  (*V.set_line_width vp 1.;
  V.f vp (fun _ -> -1.) (-1.) 8.;
  V.f vp (fun _ -> 8.) (-1.) 8.;*)
  Archimedes.close vp
