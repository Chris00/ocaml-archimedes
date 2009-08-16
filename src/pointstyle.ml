module B = Backend

let pi = 4.*. atan 1.

(*Registering point styles.*)
module M = Map.Make(String)

type name = string

let registry =
  let rec create registry = function
      [] -> registry
    | (name,f,g)::l -> create (M.add name (f,g) registry) l
  in
  let pointstyles = [
    "",(fun _ -> ()), {B.x=0.; y=0.; w=0.;h=0.};

    "X", (fun handle ->
            B.rel_move_to handle (-0.5) (-0.5);
            B.rel_line_to handle 1. 1.;
            B.rel_move_to handle (-1.) 0.;
            B.rel_line_to handle 1. (-1.);
            B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "-", (fun handle ->
            B.rel_move_to handle (-0.5) 0.;
            B.rel_line_to handle 1. 0.;
            B.stroke handle),
    {B.x=(-0.5); y=0.; w=1.;h=0.};

    "|", (fun handle ->
            B.rel_move_to handle 0. (-0.5);
            B.rel_line_to handle 0. 1.;
            B.stroke handle),
    {B.x=0.; y=(-0.5); w=0.;h=1.};

    "o", (fun handle ->
            let x,y = 0., 0. (*B.get_point handle*) in
            B.arc handle x y 1. 0. (2. *. pi);
            B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "O", (fun handle ->
            let x,y =  0., 0. (*B.get_point handle*) in
            B.arc handle x y 1. 0. (2. *. pi);
            B.fill handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "+", (fun handle ->
            B.rel_move_to handle (-0.5) 0.;
            B.rel_line_to handle 1. 0.;
            B.rel_move_to handle (-0.5) (-0.5);
            B.rel_line_to handle 0. 1.;
            B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "s", (fun handle ->
            B.rel_move_to handle (-0.5) (-0.5);
            B.rel_line_to handle 1. 0.;
            B.rel_line_to handle 0. 1.;
            B.rel_line_to handle (-1.) 0.;
            B.rel_line_to handle 0. (-1.);
            B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "S", (fun handle ->
            B.rel_move_to handle (-0.5) (-0.5);
            B.rel_line_to handle 1. 0.;
            B.rel_line_to handle 0. 1.;
            B.rel_line_to handle (-1.) 0.;
            B.rel_line_to handle 0. (-1.);
            B.fill handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "d", (fun handle ->
            B.rel_move_to handle (-1.) 0.;
            B.rel_line_to handle 0.5 0.5;
            B.rel_line_to handle 0.5 (-0.5);
            B.rel_line_to handle (-0.5) (-0.5);
            B.rel_line_to handle (-0.5) 0.5;
            B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "D", (fun handle ->
            B.rel_move_to handle (-1.) 0.;
            B.rel_line_to handle 0.5 0.5;
            B.rel_line_to handle 0.5 (-0.5);
            B.rel_line_to handle (-0.5) (-0.5);
            B.rel_line_to handle (-0.5) 0.5;
            B.fill handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "v", (fun handle ->
            B.rel_move_to handle (-0.5) (-0.5);
            B.rel_line_to handle 0.5 1.;
            B.rel_line_to handle 0.5 (-1.);
            B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "^", (fun handle ->
            B.rel_move_to handle (-0.5) 0.5;
            B.rel_line_to handle 0.5 (-1.);
            B.rel_line_to handle 0.5 1.;
            B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    ">", (fun handle ->
            B.rel_move_to handle (-0.5) (-0.5);
            B.rel_line_to handle 1. 0.5;
            B.rel_line_to handle (-1.) 0.5;
            B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "<", (fun handle ->
            B.rel_move_to handle 0.5 (-0.5);
            B.rel_line_to handle (-1.) 0.5;
            B.rel_line_to handle 1. 0.5;
            B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "-v", (fun handle ->
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 0.5 1.;
             B.rel_line_to handle 0.5 (-1.);
             B.rel_line_to handle (-1.) 0.;
             B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "^-", (fun handle ->
             B.rel_move_to handle (-0.5) 0.5;
             B.rel_line_to handle 0.5 (-1.);
             B.rel_line_to handle 0.5 1.;
             B.rel_line_to handle (-1.) 0.;
             B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "|>", (fun handle ->
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 1. 0.5;
             B.rel_line_to handle (-1.) 0.5;
             B.rel_line_to handle 0. (-1.);
             B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "<|", (fun handle ->
             B.rel_move_to handle 0.5 (-0.5);
             B.rel_line_to handle (-1.) 0.5;
             B.rel_line_to handle 1. 0.5;
             B.rel_line_to handle 0. (-1.);
             B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "-V", (fun handle ->
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 0.5 1.;
             B.rel_line_to handle 0.5 (-1.);
             B.rel_line_to handle (-1.) 0.;
             B.fill handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "--v", (fun handle ->
              B.rel_move_to handle (-0.5) (-0.5);
              B.rel_line_to handle 0.5 1.;
              B.rel_line_to handle 0.5 (-1.);
              B.rel_line_to handle (-1.) 0.;
              B.fill handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "^--", (fun handle ->
              B.rel_move_to handle (-0.5) 0.5;
              B.rel_line_to handle 0.5 (-1.);
              B.rel_line_to handle 0.5 1.;
              B.rel_line_to handle (-1.) 0.;
              B.fill handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "||>", (fun handle ->
              B.rel_move_to handle (-0.5) (-0.5);
              B.rel_line_to handle 1. 0.5;
              B.rel_line_to handle (-1.) 0.5;
              B.rel_line_to handle 0. (-1.);
              B.fill handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "<||", (fun handle ->
              B.rel_move_to handle 0.5 (-0.5);
              B.rel_line_to handle (-1.) 0.5;
              B.rel_line_to handle 1. 0.5;
              B.rel_line_to handle 0. (-1.);
              B.fill handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

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
            B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

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
            B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

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
            B.fill handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

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
            B.stroke handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

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
            B.fill handle),
    {B.x=(-0.5); y=(-0.5); w=1.;h=1.};

    "tic_up", (fun handle ->
                 B.rel_move_to handle 0. (-0.5);
                 B.rel_line_to handle 0. 0.5;
                 B.stroke handle),
    {B.x=0.; y=(-0.5); w=0.;h=0.5};

    "tic_down", (fun handle ->
                   B.rel_move_to handle 0. 0.5;
                   B.rel_line_to handle 0. (-0.5);
                   B.stroke handle),
    {B.x=0.; y=0.; w=0.;h=0.5};

    "tic_left", (fun handle ->
                   B.rel_move_to handle (-0.5) 0.;
                   B.rel_line_to handle 0.5 0.;
                   B.stroke handle),
    {B.x=(-0.5); y=0.; w=0.5;h=0.};

    "tic_right", (fun handle ->
                    B.rel_move_to handle 0.5 0.;
                    B.rel_line_to handle (-0.5) 0.;
                    B.stroke handle),
    {B.x=0.; y=0.; w=0.5;h=0.}
  ]
  in
  ref (create M.empty pointstyles)

exception Error of string

let add ~name f g =
  (*FIXME: what to do if name already used?*)
  registry := M.add name (f,g) !registry

let render name =
  try
    let f, rect = M.find name !registry in
    fun b -> (f b; rect)
  with Not_found -> raise (Error name)
