(* Generic code implementing the path optimization for the various
   array types. *)

DEFINE SET_CURR_SUB(p, x, y, i0) =
  if not p.curr_pt then (
    (* No current point, so [line_to x.(i0) y.(i0)] behaves like [move_to] *)
    p.sub_x <- GET(x,i0);
    p.sub_y <- GET(y,i0);
    p.sub <- true;
  )

DEFINE SET_CURR_PT(p, x, y, i1) =
  p.x <- GET(x,i1);
  p.y <- GET(y,i1);
  p.curr_pt <- true

let unsafe_line_to p (x:t) (y:t) i0 i1 =
  SET_CURR_SUB(p, x, y, i0);
  Queue.add (CONSTRUCTOR(x, y, i0, i1)) p.path;
  SET_CURR_PT(p, x, y, i1)

(* returns the index [i] of the first finite element in
   [x.(i0);...;x.(i1)] (and the same for [y]) or [i > i1] of it does
   not exist.  Assume [i0 <= i1] and both are valid indices. *)
let rec index_finite_incr (x:t) (y:t) i0 i i1 =
  if i > i1 then i
  else if is_finite(GET(x, i)) && is_finite(GET(y,i)) then i
  else index_finite_incr x y i0 (i + 1) i1

let rec index_finite_decr (x:t) (y:t) i0 i i1 =
  if i < i1 then i
  else if is_finite(GET(x, i)) && is_finite(GET(y,i)) then i
  else index_finite_decr x y i0 (i - 1) i1

(* Assume the point is finite at index [i0] and that the range is valid. *)
let rec index_all_finite_incr (x:t) (y:t) i0 i i1 =
  if i = i1 then i
  else if is_finite(GET(x,i)) && is_finite(GET(y,i)) then
    index_all_finite_incr x y i0 (i + 1) i1
  else i - 1

let rec index_all_finite_decr (x:t) (y:t) i0 i i1 =
  if i = i1 then i
  else if is_finite(GET(x,i)) && is_finite(GET(y,i)) then
    index_all_finite_incr x y i0 (i - 1) i1
  else i + 1

(* Assume the point is finite at index [i0] and that the range is valid. *)
let rec subdivide_incr p x y i0 i1 =
  let i = index_all_finite_incr x y i0 i0 i1 in
  Queue.add (CONSTRUCTOR(x, y, i0, i)) p.path;
  if i = i1 then i
  else (
    (* point at index [i + 1] no finite but maybe more finite points *)
    let i0 = i + 1 in
    let j = index_finite_incr x y i0 i0 i1 in
    if j > i1 then i (* = last finite index *)
    else subdivide_incr p x y j i1
  )

let rec subdivide_decr p x y i0 i1 = (* i0 >= i1 *)
  let i = index_all_finite_decr x y i0 i0 i1 in
  Queue.add (CONSTRUCTOR(x, y, i0, i)) p.path;
  if i = i1 then i
  else (
    (* point at index [i -1 1] no finite but maybe more finite points *)
    let i0 = i - 1 in
    let j = index_finite_decr x y i0 i0 i1 in
    if j < i1 then i (* = last finite index *)
    else subdivide_decr p x y j i1
  )


let line_to p ?(i0=FIRST) ?i1 ?(const_x=false) (x:t) ?(const_y=false) (y:t) =
  let i1 = get_i1 FNAME FIRST (LAST(DIM(x))) (LAST(DIM(y))) i0 i1 in
  let x = if const_x then x else COPY(x) in
  let y = if const_y then y else COPY(y) in
  if i0 <= i1 then (
    let i0 = index_finite_incr x y i0 i0 i1 in
    if i0 <= i1 then (
      (* At least 1 finite point *)
      SET_CURR_SUB(p, x, y, i0);
      let i1 = subdivide_incr p x y i0 i1 in
      SET_CURR_PT(p, x, y, i1)
    ))
  else
    let i0 = index_finite_decr x y i0 i0 i1 in
    if i0 >= i1 then (
      SET_CURR_SUB(p, x, y, i0);
      let i1 = subdivide_decr p x y i0 i1 in
      SET_CURR_PT(p, x, y, i1)
    )
