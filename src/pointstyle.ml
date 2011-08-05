module B = Backend

(*let pi = 4. *. atan 1.*)
let two_pi = 8. *. atan 1.

(*Registering point styles. TODO: shouldn't we be better with hash
  tables ? Since we have to often access the elements. *)
module M = Map.Make(String)

type name = string

exception Error of string

let registry = ref M.empty

let add ~name f g =
  (*Note: if name already used, then replaces the data (see Map.S.add)*)
  registry := M.add name (f,g) !registry


(* TODO Improve (remove B.move_to handle 0.5 0.5 and fix orders) *)
let () =
  add "" (fun _ -> ()) {Matrix.x=0.; y=0.; w=0.;h=0.};
  add "x" (fun handle ->
             B.move_to handle 0.5 0.5;
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 1. 1.;
             B.rel_move_to handle (-1.) 0.;
             B.rel_line_to handle 1. (-1.);
             B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "-" (fun handle ->
             B.move_to handle 0.5 0.5;
             B.rel_move_to handle (-0.5) 0.;
             B.rel_line_to handle 1. 0.;
             B.stroke handle)
    {Matrix.x=(-0.5); y=0.; w=1.;h=0.};
  add "|" (fun handle ->
             B.move_to handle 0.5 0.5;
             B.rel_move_to handle 0. (-0.5);
             B.rel_line_to handle 0. 1.;
             B.stroke handle)
    {Matrix.x=0.; y=(-0.5); w=0.;h=1.};
  add "o" (fun handle ->
             B.move_to handle 0.5 0.5;
             B.rel_move_to handle 0.5 0.;
             B.arc handle 0.5 0. two_pi;
             B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "O" (fun handle ->
             B.move_to handle 0.5 0.5;
             B.rel_move_to handle 0.5 0.;
             B.arc handle 0.5 0. two_pi;
             B.fill handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "+" (fun handle ->
             B.move_to handle 0.5 0.5;
             B.rel_move_to handle (-0.5) 0.;
             B.rel_line_to handle 1. 0.;
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 0. 1.;
             B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "s" (fun handle ->
             B.move_to handle 0.5 0.5;
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 1. 0.;
             B.rel_line_to handle 0. 1.;
             B.rel_line_to handle (-1.) 0.;
             B.rel_line_to handle 0. (-1.);
             B.close_path handle;
             B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "S" (fun handle ->
             B.move_to handle 0.5 0.5;
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 1. 0.;
             B.rel_line_to handle 0. 1.;
             B.rel_line_to handle (-1.) 0.;
             B.rel_line_to handle 0. (-1.);
             B.close_path handle;
             B.fill handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "d" (fun handle ->
             B.move_to handle 0.5 0.5;
             B.rel_move_to handle (-0.5) 0.;
             B.rel_line_to handle 0.5 0.5;
             B.rel_line_to handle 0.5 (-0.5);
             B.rel_line_to handle (-0.5) (-0.5);
             B.rel_line_to handle (-0.5) 0.5;
             B.close_path handle;
             B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "D" (fun handle ->
             B.move_to handle 0.5 0.5;
             B.rel_move_to handle (-0.5) 0.;
             B.rel_line_to handle 0.5 0.5;
             B.rel_line_to handle 0.5 (-0.5);
             B.rel_line_to handle (-0.5) (-0.5);
             B.rel_line_to handle (-0.5) 0.5;
             B.close_path handle;
             B.fill handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "^" (fun handle ->
             B.move_to handle 0.5 0.5;
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 0.5 1.;
             B.rel_line_to handle 0.5 (-1.);
             B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "v" (fun handle ->
             B.move_to handle 0.5 0.5;
             B.rel_move_to handle (-0.5) 0.5;
             B.rel_line_to handle 0.5 (-1.);
             B.rel_line_to handle 0.5 1.;
             B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add ">" (fun handle ->
             B.move_to handle 0.5 0.5;
             B.rel_move_to handle (-0.5) (-0.5);
             B.rel_line_to handle 1. 0.5;
             B.rel_line_to handle (-1.) 0.5;
             B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "<" (fun handle ->
             B.move_to handle 0.5 0.5;
             B.rel_move_to handle 0.5 (-0.5);
             B.rel_line_to handle (-1.) 0.5;
             B.rel_line_to handle 1. 0.5;
             B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "^-" (fun handle ->
              B.move_to handle 0.5 0.5;
              B.rel_move_to handle (-0.5) (-0.5);
              B.rel_line_to handle 0.5 1.;
              B.rel_line_to handle 0.5 (-1.);
              B.close_path handle;
              B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "v-" (fun handle ->
              B.move_to handle 0.5 0.5;
              B.rel_move_to handle (-0.5) 0.5;
              B.rel_line_to handle 0.5 (-1.);
              B.rel_line_to handle 0.5 1.;
              B.close_path handle;
              B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "|>" (fun handle ->
              B.move_to handle 0.5 0.5;
              B.rel_move_to handle (-0.5) (-0.5);
              B.rel_line_to handle 1. 0.5;
              B.rel_line_to handle (-1.) 0.5;
              B.close_path handle;
              B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "<|" (fun handle ->
              B.move_to handle 0.5 0.5;
              B.rel_move_to handle 0.5 (-0.5);
              B.rel_line_to handle (-1.) 0.5;
              B.rel_line_to handle 1. 0.5;
              B.close_path handle;
              B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "^--" (fun handle ->
              B.move_to handle 0.5 0.5;
              B.rel_move_to handle (-0.5) (-0.5);
              B.rel_line_to handle 0.5 1.;
              B.rel_line_to handle 0.5 (-1.);
              B.close_path handle;
              B.fill handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "v--" (fun handle ->
               B.move_to handle 0.5 0.5;
               B.rel_move_to handle (-0.5) 0.5;
               B.rel_line_to handle 0.5 (-1.);
               B.rel_line_to handle 0.5 1.;
               B.close_path handle;
               B.fill handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "||>" (fun handle ->
               B.move_to handle 0.5 0.5;
               B.rel_move_to handle (-0.5) (-0.5);
               B.rel_line_to handle 1. 0.5;
               B.rel_line_to handle (-1.) 0.5;
               B.close_path handle;
               B.fill handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "<||" (fun handle ->
               B.move_to handle 0.5 0.5;
               B.rel_move_to handle 0.5 (-0.5);
               B.rel_line_to handle (-1.) 0.5;
               B.rel_line_to handle 1. 0.5;
               B.close_path handle;
               B.fill handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "*" (fun handle ->
             B.move_to handle 0.5 0.5;
             for i = 0 to 4 do
               let angle = (float i) *. two_pi /. 5. in
               let cosa = cos angle /.2.
               and sina = sin angle  /.2.in
                 (*We want (0,1) and other points uniformly. It is
                   equivalent to get the axes rotated.*)
                 B.rel_line_to handle (-.sina) cosa;
                 B.rel_move_to handle sina (-.cosa)
             done;
             B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "p" (fun handle ->
             B.move_to handle 0.5 0.5;
             let xstart = 0. and ystart = 0.5 in
             B.rel_move_to handle xstart ystart;
             let rec next_vertex i x y=
               if i < 5 then
                 (let angle = (float i) *. two_pi /. 5. in
                  let cosa = cos angle /.2.
                  and sina = sin angle /.2.in
                    (*We want (0,1) as vertex, then all the others
                      uniformly. We have to make a rotation of pi/2; or
                      equivalently, switching sin and cos, and take the
                      opposite sign for sin.*)
                    B.rel_line_to handle ( -.sina -.x) (cosa -.y);
                    next_vertex (i+1) ( -.sina) cosa)
             in next_vertex 1 xstart ystart;
             B.close_path handle;
             B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "P" (fun handle ->
             B.move_to handle 0.5 0.5;
             let xstart = 0. and ystart = 0.5 in
             B.rel_move_to handle xstart ystart;
             let rec next_vertex i x y =
               if i < 5 then
                 (let angle = (float i) *. two_pi /. 5. in
                  let cosa = cos angle/.2.
                  and sina = sin angle/.2. in
                  (*We want (0,1) as vertex, then all the others
                    uniformly. We have to make a rotation of pi/2; or
                    equivalently, switching sin and cos, and take the
                    opposite sign for sin.*)
                  B.rel_line_to handle ( -.sina -.x) (cosa -.y);
                  next_vertex (i+1) ( -.sina) cosa)
             in next_vertex 1 xstart ystart;
             B.close_path handle;
             B.fill handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "h" (fun handle ->
             B.move_to handle 0.5 0.5;
             let xstart = 0. and ystart = 0.5 in
             B.rel_move_to handle xstart ystart;
             let rec next_vertex i x y =
               if i < 6 then
                 (let angle = (float i) *. two_pi /. 6. in
                  let cosa = cos angle/.2.
                  and sina = sin angle/.2. in
                  (*We want (0,1) as vertex, then all the others
                    uniformly. We have to make a rotation of pi/2; or
                    equivalently, switching sin and cos, and take the
                    opposite sign for sin.*)
                  B.rel_line_to handle ( -.sina -.x) (cosa -.y);
                  next_vertex (i+1) ( -.sina) cosa)
             in next_vertex 1 xstart ystart;
             B.close_path handle;
             B.stroke handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "H" (fun handle ->
             B.move_to handle 0.5 0.5;
             let xstart = 0. and ystart = 0.5 in
             B.rel_move_to handle xstart ystart;
             let rec next_vertex i x y =
               if i < 6 then
                 (let angle = (float i) *. two_pi /. 6. in
                  let cosa = cos angle /.2.
                  and sina = sin angle /.2. in
                  (*We want (0,1) as vertex, then all the others
                    uniformly. We have to make a rotation of pi/2; or
                    equivalently, switching sin and cos, and take the
                    opposite sign for sin.*)
                  B.rel_line_to handle ( -.sina -.x) (cosa -.y);
                  next_vertex (i+1) ( -.sina) cosa)
             in next_vertex 1 xstart ystart;
             B.close_path handle;
             B.fill handle)
    {Matrix.x=(-0.5); y=(-0.5); w=1.;h=1.};
  add "tic_up" (fun handle ->
                  B.move_to handle 0.5 1.;
                  B.line_to handle 0.5 0.5;
                  B.stroke handle)
    {Matrix.x=0.; y=(-0.5); w=0.;h=0.5};
  add "tic_down" (fun handle ->
                    B.move_to handle 0.5 0.;
                    B.line_to handle 0.5 0.5;
                    B.stroke handle)
    {Matrix.x=0.; y=0.; w=0.;h=0.5};
  add "tic_left" (fun handle ->
                    B.move_to handle 0.5 0.5;
                    B.rel_move_to handle (-0.5) 0.;
                    B.rel_line_to handle 0.5 0.;
                    B.stroke handle)
    {Matrix.x=(-0.5); y=0.; w=0.5;h=0.};
  add "tic_right" (fun handle ->
                     B.move_to handle 0.5 0.5;
                     B.rel_move_to handle 0.5 0.;
                     B.rel_line_to handle (-0.5) 0.;
                     B.stroke handle)
    {Matrix.x=0.; y=0.; w=0.5;h=0.}


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

let names () =
  M.fold (fun k _ acc -> k :: acc) !registry []
