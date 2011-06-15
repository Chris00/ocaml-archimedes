let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

(* Return the index of the first space in s.[i0 .. i1-1] or [i1] if
   none was found.  s.[i0 .. i1-1] is assumed to be a valid substring
   of s.  *)
let rec index_of_space s i0 i1 =
  if i0 >= i1 then i1
  else if is_space s.[i0] then i0
  else index_of_space s (i0 + 1) i1

let rec index_of_non_space s i0 i1 =
  if i0 >= i1 then i1
  else if is_space s.[i0] then index_of_non_space s (i0 + 1) i1
  else i0

(* Return a list of substrings of s.[i0 .. i1-1] which are separated
   by one or several spaces. *)
let rec split_on_spaces s i0 i1 =
  let i0 = index_of_non_space s i0 i1 in (* skip spaces *)
  if i0 >= i1 then []
  else (
    let i01 = index_of_space s i0 i1 in
    String.sub s i0 (i01 - i0) :: split_on_spaces s (i01 + 1) i1
  )


(* [s.[i]] and [p.[i]] are identical for all [i] s.t. [i0 <= i < i1]. *)
let rec identical s p i0 i1 =
  i0 >= i1 || (s.[i0] = p.[i0] && identical s p (i0 + 1) i1)

let start_with s p =
  let len_p = String.length p in
  String.length s >= len_p && identical s p 0 len_p

let first_and_list b =
  let len = String.length b in
  let i = index_of_space b 0 len in
  if i = len then (b, []) (* no options *)
  else (String.sub b 0 i, split_on_spaces b (i+1) len)

let index_string matcher string =
  let nm = String.length matcher
  and ns = String.length string in
  if nm > ns then ns + 1
  else
    let rec find_first from =
    try
      let i = String.index_from string from matcher.[0] in
      if start_with (String.sub string i (ns - i)) matcher then i
      else find_first (i+1)
    with Not_found -> ns + 1
    in find_first 0
