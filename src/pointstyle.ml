module B = Backend

let pi = 4.*. atan 1.

(*Registering point styles.*)
module M = Map.Make(String)

let registry =
  let rec create registry = function
      [] -> registry
    | (name,f)::l -> create (M.add name f registry) l
  in
  let pointstyles = [
    "",(fun _ -> ());
    "X", (fun handle ->
            B.rel_move_to handle (-0.5) (-0.5);
            B.rel_line_to handle 1. 1.;
            B.rel_move_to handle (-1.) 0.;
            B.rel_line_to handle 1. (-1.);
            B.stroke handle);
    "-", (fun handle ->
            B.rel_move_to handle (-0.5) 0.;
            B.rel_line_to handle 1. 0.;
            B.stroke handle);
    "|", (fun handle ->
            B.rel_move_to handle 0. (-0.5);
            B.rel_line_to handle 0. 1.;
            B.stroke handle);
    "o", (fun handle ->
            let x,y = 0., 0. (*B.get_point handle*) in
            B.arc handle x y 1. 0. (2. *. pi);
            B.stroke handle);
    "O", (fun handle ->
            let x,y =  0., 0. (*B.get_point handle*) in
            B.arc handle x y 1. 0. (2. *. pi);
            B.fill handle);
    "+", (fun handle ->
            B.rel_move_to handle (-0.5) 0.;
            B.rel_line_to handle 1. 0.;
            B.rel_move_to handle (-0.5) (-0.5);
            B.rel_line_to handle 0. 1.;
            B.stroke handle);
    "s", (fun handle ->
            B.rel_move_to handle (-0.5) (-0.5);
            B.rel_line_to handle 1. 0.;
            B.rel_line_to handle 0. 1.;
            B.rel_line_to handle (-1.) 0.;
            B.rel_line_to handle 0. (-1.);
            B.stroke handle);
    "S", (fun handle ->
            B.rel_move_to handle (-0.5) (-0.5);
            B.rel_line_to handle 1. 0.;
            B.rel_line_to handle 0. 1.;
            B.rel_line_to handle (-1.) 0.;
            B.rel_line_to handle 0. (-1.);
            B.fill handle);
    "d", (fun handle ->
            B.rel_move_to handle (-1.) 0.;
            B.rel_line_to handle 0.5 0.5;
            B.rel_line_to handle 0.5 (-0.5);
            B.rel_line_to handle (-0.5) (-0.5);
            B.rel_line_to handle (-0.5) 0.5;
            B.stroke handle);
    "D", (fun handle ->
            B.rel_move_to handle (-1.) 0.;
            B.rel_line_to handle 0.5 0.5;
            B.rel_line_to handle 0.5 (-0.5);
            B.rel_line_to handle (-0.5) (-0.5);
            B.rel_line_to handle (-0.5) 0.5;
            B.fill handle);
    "v", (fun handle ->
            B.rel_move_to handle (-0.5) (-0.5);
            B.rel_line_to handle 0.5 1.;
            B.rel_line_to handle 0.5 (-1.);
            B.stroke handle);
    "^", (fun handle ->
            B.rel_move_to handle (-0.5) 0.5;
            B.rel_line_to handle 0.5 (-1.);
            B.rel_line_to handle 0.5 1.;
            B.stroke handle);
    ">", (fun handle ->
            B.rel_move_to handle (-0.5) (-0.5);
            B.rel_line_to handle 1. 0.5;
            B.rel_line_to handle (-1.) 0.5;
            B.stroke handle);
    "<", (fun handle ->
            B.rel_move_to handle 0.5 (-0.5);
            B.rel_line_to handle (-1.) 0.5;
            B.rel_line_to handle 1. 0.5;
            B.stroke handle);
    "-v", (fun handle ->
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 0.5 1.;
             B.rel_line_to handle 0.5 (-1.);
             B.rel_line_to handle (-1.) 0.;
             B.stroke handle);
    "^-", (fun handle ->
             B.rel_move_to handle (-0.5) 0.5;
             B.rel_line_to handle 0.5 (-1.);
             B.rel_line_to handle 0.5 1.;
             B.rel_line_to handle (-1.) 0.;
             B.stroke handle);
    "|>", (fun handle ->
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 1. 0.5;
             B.rel_line_to handle (-1.) 0.5;
             B.rel_line_to handle 0. (-1.);
             B.stroke handle);
    "<|", (fun handle ->
             B.rel_move_to handle 0.5 (-0.5);
             B.rel_line_to handle (-1.) 0.5;
             B.rel_line_to handle 1. 0.5;
             B.rel_line_to handle 0. (-1.);
             B.stroke handle);

    "-V", (fun handle ->
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 0.5 1.;
             B.rel_line_to handle 0.5 (-1.);
             B.rel_line_to handle (-1.) 0.;
             B.fill handle);
    "--v", (fun handle ->
              B.rel_move_to handle (-0.5) (-0.5);
              B.rel_line_to handle 0.5 1.;
              B.rel_line_to handle 0.5 (-1.);
              B.rel_line_to handle (-1.) 0.;
              B.fill handle);

    "^--", (fun handle ->
              B.rel_move_to handle (-0.5) 0.5;
              B.rel_line_to handle 0.5 (-1.);
              B.rel_line_to handle 0.5 1.;
              B.rel_line_to handle (-1.) 0.;
              B.fill handle);
    "||>", (fun handle ->
              B.rel_move_to handle (-0.5) (-0.5);
              B.rel_line_to handle 1. 0.5;
              B.rel_line_to handle (-1.) 0.5;
              B.rel_line_to handle 0. (-1.);
              B.fill handle);
    "<||", (fun handle ->
              B.rel_move_to handle 0.5 (-0.5);
              B.rel_line_to handle (-1.) 0.5;
              B.rel_line_to handle 1. 0.5;
              B.rel_line_to handle 0. (-1.);
              B.fill handle);
    "*", (fun handle ->
            for i = 0 to 4 do
              let angle = (float (2*i)) *. pi /. 5. in
              let cosa = cos angle
              and sina = sin angle in
              (*We want (0,1) and other points uniformly. It is
                equivalent to get the axes rotated.*)
              B.rel_line_to handle (-.sina) cosa;
              B.rel_move_to handle sina (-.cosa)
            done;
            B.stroke handle);
    "p", (fun handle ->
            B.rel_move_to handle 0. 1.;
            for i = 1 to 5 do
              let angle = (float (2*i)) *. pi /. 5. in
              let cosa = cos angle
              and sina = sin angle in
              (*We want (0,1) and other points uniformly. It is
                equivalent to get the axes rotated.*)
              B.rel_line_to handle (-.sina) cosa;
            done;
            B.stroke handle);
    "P", (fun handle ->
            B.rel_move_to handle 0. 1.;
            for i = 1 to 5 do
              let angle = (float (2*i)) *. pi /. 5. in
              let cosa = cos angle
              and sina = sin angle in
              (*We want (0,1) and other points uniformly. It is
                equivalent to get the axes rotated.*)
              B.rel_line_to handle (-.sina) cosa;
            done;
            B.fill handle);

    "h", (fun handle ->
            B.rel_move_to handle 0. 1.;
            for i = 1 to 6 do
              let angle = (float (2*i)) *. pi /. 6. in
              let cosa = cos angle
              and sina = sin angle in
              (*We want (0,1) and other points uniformly. It is
                equivalent to get the axes rotated.*)
              B.rel_line_to handle (-.sina) cosa;
            done;
            B.stroke handle);
    "H", (fun handle ->
            B.rel_move_to handle 0. 1.;
            for i = 1 to 6 do
              let angle = (float (2*i)) *. pi /. 6. in
              let cosa = cos angle
              and sina = sin angle in
              (*We want (0,1) and other points uniformly. It is
                equivalent to get the axes rotated.*)
              B.rel_line_to handle (-.sina) cosa;
            done;
            B.fill handle);
    "tic_up", (fun handle ->
                 (*FIXME : 1 unit or 0.5 unit?*)
                 B.rel_move_to handle 0. (-1.);
                 B.rel_line_to handle 0. 1.;
                 B.stroke handle);
    "tic_down", (fun handle ->
                   (*FIXME : 1 unit or 0.5 unit?*)
                   B.rel_move_to handle 0. 1.;
                   B.rel_line_to handle 0. (-1.);
                   B.stroke handle);
    "tic_left", (fun handle ->
                   (*FIXME : 1 unit or 0.5 unit?*)
                   B.rel_move_to handle (-1.) 0.;
                   B.rel_line_to handle 1. 0.;
                   B.stroke handle);
    "tic_right", (fun handle ->
                    (*FIXME : 1 unit or 0.5 unit?*)
                    B.rel_move_to handle 1. 0.;
                    B.rel_line_to handle (-1.) 0.;
                    B.stroke handle)
  ]
  in
  ref (create M.empty pointstyles)

exception Error of string

let add ~name f =
  (*FIXME: what to do if name already used?*)
  registry := M.add name f !registry

let render name backend =
  try
    let f = M.find name !registry in
    f backend
  with Not_found -> raise (Error name)
