open Bigarray

type t =
    {
      data:(float,float64_elt,c_layout) Array2.t;
      xmin: float; xmax: float; ymin: float; ymax: float;
      mutable pos: int;len: int
    }

let of_list list =
  let n = List.length list in
  let array = Array2.create float64 c_layout n 2 in
  let rec fill_array i xmin xmax ymin ymax = function
      [] -> (*Make the iterator*)
        {data= array;
         xmin = xmin; xmax = xmax; ymin = ymin; ymax = ymax;
         pos = 0; len = n}
    | (x,y)::l ->
        array.{i,0} <- x;
        array.{i,1} <- y;
        fill_array (i+1) (min x xmin) (max x xmax) (min y ymin) (max y ymax) l
  in fill_array 0 max_float (-.max_float) max_float (-.max_float) list

let of_array array =
  let n = Array.length array in
  let bigarray = Array2.create float64 c_layout n 2 in
  let rec fill_array i xmin xmax ymin ymax =
    if i >= n then
      {data= bigarray;
       xmin = xmin; xmax = xmax; ymin = ymin; ymax = ymax;
       pos = 0; len = n}
    else
      let x,y = array.(i) in
      bigarray.{i,0} <- x;
      bigarray.{i,1} <- y;
      fill_array (i+1) (min x xmin) (max x xmax) (min y ymin) (max y ymax)
  in fill_array 0 max_float (-.max_float) max_float (-.max_float)



let of_bigarray2 ?(clayout=true) array =
  if Array2.dim2 array < 2 then invalid_arg "Data.of_bigarray2"
  else
    let dim = Array2.dim1 array in
    let bigarray = Array2.create float64 c_layout dim 2 in
    let ofs = if clayout then 0 else 1 in
    let rec fill_array i xmin xmax ymin ymax =
      if i >= dim then
        {data= bigarray;
         xmin = xmin; xmax = xmax; ymin = ymin; ymax = ymax;
         pos = 0; len = dim}
      else
        let x = array.{i+ofs, ofs}
        and y = array.{i+ofs, ofs+1} in
        bigarray.{i,0} <- x;
        bigarray.{i,1} <- y;
        fill_array (i+1) (min x xmin) (max x xmax) (min y ymin) (max y ymax)
  in fill_array 0 max_float (-.max_float) max_float (-.max_float)


let of_lists listx listy =
  let n,m = List.length listx, List.length listy in
  if n <> m then invalid_arg "Data.of_lists";
  let array = Array2.create float64 c_layout n 2 in
  let rec fill_array i xmin xmax ymin ymax = function
    | (x::l),(y::l') ->
        array.{i,0} <- x;
        array.{i,1} <- y;
        fill_array (i+1)
          (min x xmin) (max x xmax) (min y ymin) (max y ymax) (l,l')
    | _,_ ->
        (*Condition on lengths ensures that this matches only two
          empty lists.*)
        {data= array;
         xmin = xmin; xmax = xmax; ymin = ymin; ymax = ymax;
         pos = 0; len = n}
  in fill_array 0 max_float (-.max_float) max_float (-.max_float) (listx,listy)

let of_arrays arrayx arrayy =
  let n = Array.length arrayx
  and m = Array.length arrayy in
  if n <> m then invalid_arg "Data.of_arrays";
  let bigarray = Array2.create float64 c_layout n 2 in
  let rec fill_array i xmin xmax ymin ymax =
    if i >= n then
      {data= bigarray;
       xmin = xmin; xmax = xmax; ymin = ymin; ymax = ymax;
       pos = 0; len = n}
    else
      let x = arrayx.(i)
      and y = arrayy.(i) in
      bigarray.{i,0} <- x;
      bigarray.{i,1} <- y;
      fill_array (i+1) (min x xmin) (max x xmax) (min y ymin) (max y ymax)
  in fill_array 0 max_float (-.max_float) max_float (-.max_float)

let of_bigarrays ?(xclayout=true) arrayx ?(yclayout=true) arrayy =
  let n = Array1.dim arrayx
  and m = Array1.dim arrayy in
  if n <> m then invalid_arg "Data.of_bigarrays";
    let bigarray = Array2.create float64 c_layout n 2 in
    let ofsx = if xclayout then 0 else 1
    and ofsy = if yclayout then 0 else 1 in
  let rec fill_array i xmin xmax ymin ymax =
    if i >= n then
      {data= bigarray;
       xmin = xmin; xmax = xmax; ymin = ymin; ymax = ymax;
       pos = 0; len = n}
    else
      let x = arrayx.{i+ofsx}
      and y = arrayy.{i+ofsy} in
      bigarray.{i,0} <- x;
      bigarray.{i,1} <- y;
      fill_array (i+1) (min x xmin) (max x xmax) (min y ymin) (max y ymax)
  in fill_array 0 max_float (-.max_float) max_float (-.max_float)

let from_sampling f ?min_step ?nsamples a b =
  let len, (xmin,xmax,ymin,ymax), fct =
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
   xmin = xmin; xmax = xmax; ymin = ymin; ymax = ymax;
   pos = 0; len = len}

let next iter =
  let n = iter.pos in
  iter.pos <- n+1;
  try
    Some (iter.data.{n,0}, iter.data.{n,1})
  with Invalid_argument _ ->
    iter.pos <- n;
    None

let reinit iter = iter.pos <- 0

let nb_data iter = iter.len

let extents iter = iter.xmin, iter.xmax, iter.ymin, iter.ymax
