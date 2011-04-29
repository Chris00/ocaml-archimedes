open Bigarray

type t = {
  data:(float,float64_elt,c_layout) Array2.t;
  mutable extents: Matrix.rectangle;
  mutable pos: int;
  len: int
}

let dummy = {
  data = Array2.create float64 c_layout 0 0;
  extents = {Matrix.x = 0.; y = 0.; w = 0.; h = 0.};
  pos = 0;
  len = 0
}

let base_extents x y = {Matrix.x = x; y = y; w = 0.; h = 0.}

let of_list = function
  | [] -> dummy
  | (x, y) :: tl ->
      let n = List.length tl in
      let array = Array2.create float64 c_layout n 2 in
      (*Initialization:*)
      array.{0, 0} <- x;
      array.{0, 1} <- y;
      (*Recursion*)
      let rec fill_array i extents = function
        | [] -> {
            data = array;
            extents = extents;
            pos = 0;
            len = n
          }
        | (x, y) :: l ->
            array.{i, 0} <- x;
            array.{i, 1} <- y;
            fill_array (i + 1) (Functions.update_extents extents x y) l
      in
      fill_array 1 (base_extents x y) tl

let of_array array =
  let n = Array.length array in
  if n = 0 then dummy
  else (
    let bigarray = Array2.create float64 c_layout n 2 in
    (*Initialisation*)
    let x, y = array.(0) in
    bigarray.{0, 0} <- x;
    bigarray.{0, 1} <- y;
    (*Recursion*)
    let rec fill_array i extents =
      if i >= n then
        { data = bigarray;
          extents = extents;
          pos = 0;
          len = n }
      else
        let x,y = array.(i) in
        bigarray.{i, 0} <- x;
        bigarray.{i, 1} <- y;
        fill_array (i + 1) (Functions.update_extents extents x y)
    in
    fill_array 1 (base_extents x y))


let of_bigarray2 ?(clayout=true) array =
  if Array2.dim2 array < 2 then invalid_arg "Data.of_bigarray2"
  else
    let dim = Array2.dim1 array in
    if dim = 0 then dummy
    else
      let bigarray = Array2.create float64 c_layout dim 2 in
      let ofs = if clayout then 0 else 1 in
      let x = array.{ofs, ofs}
      and y = array.{ofs, ofs+1} in
      bigarray.{0, 0} <- x;
      bigarray.{0, 1} <- y;
      let rec fill_array i extents =
        if i >= dim then
          {data = bigarray;
           extents = extents;
           pos = 0; len = dim}
        else
          let x = array.{i+ofs, ofs}
          and y = array.{i+ofs, ofs+1} in
          bigarray.{i,0} <- x;
          bigarray.{i,1} <- y;
          fill_array (i + 1) (Functions.update_extents extents x y)
      in
      fill_array 1 (base_extents x y)


let of_lists listx listy =
  let n = List.length listx in
  if n <> (List.length listy) then invalid_arg "Iterator.of_lists"
  else
    match listx, listy with
    | [], [] -> dummy
    | (x :: tlx), (y :: tly) ->
        let array = Array2.create float64 c_layout n 2 in
        array.{0,0} <- x;
        array.{0,1} <- y;
        let rec fill_array i extents = function
          | (x :: lx), (y :: ly) ->
              array.{i, 0} <- x;
              array.{i, 1} <- y;
              let extents = Functions.update_extents extents x y in
              fill_array (i + 1) extents (lx, ly)
          | _, _ ->
              (*Condition on lengths ensures that this matches only two
                empty lists.*)
              {data= array;
               extents = extents;
               pos = 0; len = n}
        in fill_array 1 (base_extents x y) (tlx, tly)
    | _,_ -> failwith "Iterator.of_lists : internal error"
        (*this cannot happen due to lengths checking.*)


let of_arrays arrayx arrayy =
  let n = Array.length arrayx in
  if n <> (Array.length arrayy) then invalid_arg "Data.of_arrays"
  else if n = 0 then dummy
  else
    let bigarray = Array2.create float64 c_layout n 2 in
    let x = arrayx.(0)
    and y = arrayy.(0) in
    bigarray.{0,0} <- x;
    bigarray.{0,1} <- y;
    let rec fill_array i extents =
      if i >= n then
        {data = bigarray;
         extents = extents;
         pos = 0; len = n}
      else
        let x = arrayx.(i)
        and y = arrayy.(i) in
        bigarray.{i, 0} <- x;
        bigarray.{i, 1} <- y;
        fill_array (i + 1) (Functions.update_extents extents x y)
    in fill_array 1 (base_extents x y)

let of_bigarrays ?(xclayout=true) arrayx ?(yclayout=true) arrayy =
  let n = Array1.dim arrayx in
  if n <> (Array1.dim arrayy) then invalid_arg "Data.of_bigarrays";
  let bigarray = Array2.create float64 c_layout n 2 in
  let ofsx = if xclayout then 0 else 1
  and ofsy = if yclayout then 0 else 1 in
  let x = arrayx.{ofsx}
  and y = arrayy.{ofsy} in
  bigarray.{0, 0} <- x;
  bigarray.{0, 1} <- y;
  let rec fill_array i extents =
    if i >= n then
      {data = bigarray;
       extents = extents;
       pos = 0; len = n}
    else
      let x = arrayx.{i + ofsx}
      and y = arrayy.{i + ofsy} in
      bigarray.{i, 0} <- x;
      bigarray.{i, 1} <- y;
      fill_array (i + 1) (Functions.update_extents extents x y)
  in fill_array 1 (base_extents x y)

let from_sampling f ?min_step ?nsamples a b =
  let len, extents, fct =
    Functions.samplefxy f ?min_step ?nsamples a b
  in
  let array = Array2.create float64 c_layout len 2 in
  let n = ref 0 in
  let fill_array array (x,y) =
    array.{!n,0} <- x;
    array.{!n,1} <- y;
    n := !n + 1;
    array
  in
  {data = fct fill_array array;
   extents = extents;
   pos = 0; len = len}

let next iter =
  let n = iter.pos in
  iter.pos <- n+1;
  try
    Some (iter.data.{n,0}, iter.data.{n,1})
  with Invalid_argument _ ->
    iter.pos <- n;
    None

let reset iter = iter.pos <- 0

let nb_data iter = iter.len

let extents iter = (*Make a copy*)
  {iter.extents with Matrix.x = iter.extents.Matrix.x}
