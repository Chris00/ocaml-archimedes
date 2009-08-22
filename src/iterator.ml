open Bigarray

type t =
    {
      data:(float,float64_elt,c_layout) Array2.t;
      mutable pos: int
    }

let of_list list =
  let n = List.length list in
  let array = Array2.create float64 c_layout n 2 in
  let rec fill_array i = function
      [] -> ()
    | (x,y)::l ->
        array.{i,0} <- x;
        array.{i,1} <- y;
        fill_array (i+1) l
  in fill_array 0 list;
  {data= array; pos = 0}

let of_array array =
  let n = Array.length array in
  let bigarray = Array2.create float64 c_layout n 2 in
  for i=0 to n-1 do
    let x,y = array.(i) in
    bigarray.{i,0} <- x;
    bigarray.{i,1} <- y
  done;
  {data= bigarray; pos = 0}


let of_bigarray2 ?(clayout=true) array =
  if Array2.dim2 array < 2 then invalid_arg "Data.of_bigarray2"
  else
    let dim = Array2.dim1 array in
    let bigarray = Array2.create float64 c_layout dim 2 in
    let ofs = if clayout then 0 else 1 in
    for i = 0 to dim - 1 do
      bigarray.{i,0} <- array.{i+ofs, ofs};
      bigarray.{i,1} <- array.{i+ofs, ofs+1}
    done;
    {data = bigarray; pos = 0}

let of_lists listx listy =
  let n,m = List.length listx, List.length listy in
  if n <> m then invalid_arg "Data.of_lists";
  let array = Array2.create float64 c_layout n 2 in
  let rec fill_array i = function
    | (x::l),(y::l') ->
        array.{i,0} <- x;
        array.{i,1} <- y;
        fill_array (i+1) (l,l')
    | _,_ -> ()
        (*Condition on lengths ensures that this matches only two
        empty lists.*)
  in fill_array 0 (listx,listy);
  {data= array; pos = 0}

let of_arrays arrayx arrayy =
  let n = Array.length arrayx
  and m = Array.length arrayy in
  if n <> m then invalid_arg "Data.of_arrays";
  let bigarray = Array2.create float64 c_layout n 2 in
  for i=0 to n-1 do
    bigarray.{i,0} <- arrayx.(i);
    bigarray.{i,1} <- arrayy.(i)
  done;
  {data= bigarray; pos = 0}

let of_bigarrays ?(xclayout=true) arrayx ?(yclayout=true) arrayy =
  let n = Array1.dim arrayx
  and m = Array1.dim arrayy in
  if n <> m then invalid_arg "Data.of_bigarrays";
    let bigarray = Array2.create float64 c_layout n 2 in
    let ofsx = if xclayout then 0 else 1
    and ofsy = if yclayout then 0 else 1 in
  for i=0 to n-1 do
    bigarray.{i,0} <- arrayx.{i+ofsx};
    bigarray.{i,1} <- arrayy.{i+ofsy}
  done;
  {data= bigarray; pos = 0}

let next iter =
  let n = iter.pos in
  iter.pos <- n+1;
  try
    Some (iter.data.{n,0}, iter.data.{n,1})
  with Invalid_argument _ ->
    iter.pos <- n;
    None
