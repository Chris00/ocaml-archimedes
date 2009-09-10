module B = Backend

let pi = 4.*. atan 1.

(*Registering point styles.*)
module M = Map.Make(String)

type name = string

exception Error of string

let registry = ref M.empty

let add ~name f g =
  (*FIXME: what to do if name already used?*)
  registry := M.add name (f,g) !registry

let () =
  add "" (fun _ -> ()) {x=0.; y=0.; w=0.;h=0.};
  add "X" (fun handle ->
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 1. 1.;
             B.rel_move_to handle (-1.) 0.;
             B.rel_line_to handle 1. (-1.);
             B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "-" (fun handle ->
             B.rel_move_to handle (-0.5) 0.;
             B.rel_line_to handle 1. 0.;
             B.stroke handle)
    {x=(-0.5); y=0.; w=1.;h=0.};
  add  "|" (fun handle ->
              B.rel_move_to handle 0. (-0.5);
              B.rel_line_to handle 0. 1.;
              B.stroke handle)
    {x=0.; y=(-0.5); w=0.;h=1.};
  add "o" (fun handle ->
             B.arc handle 1. 0. (2. *. pi);
             B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "O" (fun handle ->
             B.arc handle 1. 0. (2. *. pi);
             B.fill handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "+" (fun handle ->
             B.rel_move_to handle (-0.5) 0.;
             B.rel_line_to handle 1. 0.;
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 0. 1.;
             B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "s" (fun handle ->
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 1. 0.;
             B.rel_line_to handle 0. 1.;
             B.rel_line_to handle (-1.) 0.;
             B.rel_line_to handle 0. (-1.);
             B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "S" (fun handle ->
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 1. 0.;
             B.rel_line_to handle 0. 1.;
             B.rel_line_to handle (-1.) 0.;
             B.rel_line_to handle 0. (-1.);
             B.fill handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "d" (fun handle ->
             B.rel_move_to handle (-1.) 0.;
             B.rel_line_to handle 0.5 0.5;
             B.rel_line_to handle 0.5 (-0.5);
             B.rel_line_to handle (-0.5) (-0.5);
             B.rel_line_to handle (-0.5) 0.5;
             B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "D" (fun handle ->
             B.rel_move_to handle (-1.) 0.;
             B.rel_line_to handle 0.5 0.5;
             B.rel_line_to handle 0.5 (-0.5);
             B.rel_line_to handle (-0.5) (-0.5);
             B.rel_line_to handle (-0.5) 0.5;
             B.fill handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "v" (fun handle ->
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 0.5 1.;
             B.rel_line_to handle 0.5 (-1.);
             B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "^" (fun handle ->
             B.rel_move_to handle (-0.5) 0.5;
             B.rel_line_to handle 0.5 (-1.);
             B.rel_line_to handle 0.5 1.;
             B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add ">" (fun handle ->
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 1. 0.5;
             B.rel_line_to handle (-1.) 0.5;
             B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "<" (fun handle ->
             B.rel_move_to handle 0.5 (-0.5);
             B.rel_line_to handle (-1.) 0.5;
             B.rel_line_to handle 1. 0.5;
             B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "-v" (fun handle ->
              B.rel_move_to handle (-0.5) (-0.5);
              B.rel_line_to handle 0.5 1.;
              B.rel_line_to handle 0.5 (-1.);
              B.rel_line_to handle (-1.) 0.;
              B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "^-" (fun handle ->
              B.rel_move_to handle (-0.5) 0.5;
              B.rel_line_to handle 0.5 (-1.);
              B.rel_line_to handle 0.5 1.;
              B.rel_line_to handle (-1.) 0.;
              B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "|>" (fun handle ->
              B.rel_move_to handle (-0.5) (-0.5);
              B.rel_line_to handle 1. 0.5;
              B.rel_line_to handle (-1.) 0.5;
              B.rel_line_to handle 0. (-1.);
              B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "<|" (fun handle ->
              B.rel_move_to handle 0.5 (-0.5);
              B.rel_line_to handle (-1.) 0.5;
              B.rel_line_to handle 1. 0.5;
              B.rel_line_to handle 0. (-1.);
              B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "-V" (fun handle ->
              B.rel_move_to handle (-0.5) (-0.5);
              B.rel_line_to handle 0.5 1.;
              B.rel_line_to handle 0.5 (-1.);
              B.rel_line_to handle (-1.) 0.;
              B.fill handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "--v" (fun handle ->
               B.rel_move_to handle (-0.5) (-0.5);
               B.rel_line_to handle 0.5 1.;
               B.rel_line_to handle 0.5 (-1.);
               B.rel_line_to handle (-1.) 0.;
               B.fill handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "^--" (fun handle ->
               B.rel_move_to handle (-0.5) 0.5;
               B.rel_line_to handle 0.5 (-1.);
               B.rel_line_to handle 0.5 1.;
               B.rel_line_to handle (-1.) 0.;
               B.fill handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "||>" (fun handle ->
               B.rel_move_to handle (-0.5) (-0.5);
               B.rel_line_to handle 1. 0.5;
               B.rel_line_to handle (-1.) 0.5;
               B.rel_line_to handle 0. (-1.);
            B.fill handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "<||" (fun handle ->
               B.rel_move_to handle 0.5 (-0.5);
               B.rel_line_to handle (-1.) 0.5;
               B.rel_line_to handle 1. 0.5;
               B.rel_line_to handle 0. (-1.);
               B.fill handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "*" (fun handle ->
             for i = 0 to 4 do
               let angle = (float (2*i)) *. pi /. 5. in
               let cosa = cos angle
               and sina = sin angle in
               (*We want (0,1) and other points uniformly. It is
                 equivalent to get the axes rotated.*)
               B.rel_line_to handle (-.sina) cosa;
               B.rel_move_to handle sina (-.cosa)
             done;
             B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "p" (fun handle ->
             B.rel_move_to handle 0. 1.;
             for i = 1 to 5 do
               let angle = (float (2*i)) *. pi /. 5. in
               let cosa = cos angle
               and sina = sin angle in
               (*We want (0,1) and other points uniformly. It is
                 equivalent to get the axes rotated.*)
               B.rel_line_to handle (-.sina) cosa;
             done;
             B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "P" (fun handle ->
             B.rel_move_to handle 0. 1.;
             for i = 1 to 5 do
               let angle = (float (2*i)) *. pi /. 5. in
               let cosa = cos angle
               and sina = sin angle in
               (*We want (0,1) and other points uniformly. It is
                 equivalent to get the axes rotated.*)
               B.rel_line_to handle (-.sina) cosa;
             done;
             B.fill handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "h" (fun handle ->
             B.rel_move_to handle 0. 1.;
             for i = 1 to 6 do
               let angle = (float (2*i)) *. pi /. 6. in
               let cosa = cos angle
               and sina = sin angle in
               (*We want (0,1) and other points uniformly. It is
                 equivalent to get the axes rotated.*)
               B.rel_line_to handle (-.sina) cosa;
             done;
             B.stroke handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "H" (fun handle ->
             B.rel_move_to handle 0. 1.;
             for i = 1 to 6 do
               let angle = (float (2*i)) *. pi /. 6. in
               let cosa = cos angle
               and sina = sin angle in
               (*We want (0,1) and other points uniformly. It is
                 equivalent to get the axes rotated.*)
               B.rel_line_to handle (-.sina) cosa;
             done;
             B.fill handle)
    {x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "tic_up" (fun handle ->
                  B.rel_move_to handle 0. (-0.5);
                  B.rel_line_to handle 0. 0.5;
                  B.stroke handle)
    {x=0.; y=(-0.5); w=0.;h=0.5};
  add "tic_down" (fun handle ->
                    B.rel_move_to handle 0. 0.5;
                    B.rel_line_to handle 0. (-0.5);
                    B.stroke handle)
    {x=0.; y=0.; w=0.;h=0.5};
  add "tic_left" (fun handle ->
                    B.rel_move_to handle (-0.5) 0.;
                    B.rel_line_to handle 0.5 0.;
                    B.stroke handle)
    {x=(-0.5); y=0.; w=0.5;h=0.};
  add "tic_right" (fun handle ->
                     B.rel_move_to handle 0.5 0.;
                     B.rel_line_to handle (-0.5) 0.;
                     B.stroke handle)
    {x=0.; y=0.; w=0.5;h=0.}


let render name =
  try
    let f, _ = M.find name !registry in
    f
  with Not_found -> raise (Error name)

let extents name =
  try
    let _, rect = M.find name !registry in
    rect
  with Not_found -> raise (Error name)

let render_extents name =
  try
    let f, rect = M.find name !registry in
    fun b -> (f b; rect)
  with Not_found -> raise (Error name)
