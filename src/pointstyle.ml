module B = Backend

module type Style =
  sig
    val name: string
    type t
    val make: string list -> t
    val point: t -> float -> float -> Backend.t -> unit
  end

module Default =
struct
  open String_utils
  let name = "default"
  type t =
      NONE
    | X of float
    | HORIZ of float
    | VERT of float
    | TIC_UP of float (*Useful for axes tics*)
    | TIC_DOWN of float
    | TIC_LEFT of float
    | TIC_RIGHT of float

  type style = t

  let string_list_of_t t =
    (*let make_string t params =
      let rec convert_params str list =
        match list with
          [] -> str
        | x::l -> convert_params (str^" "^(string_of_float x)) l
      in
      convert_params t params
    in*)
    match t with
      NONE -> []
    | X(len) -> ["X"; string_of_float len]
    | HORIZ(len) -> ["-"; string_of_float len]
    | VERT(len) ->["|"; string_of_float len]
    | TIC_UP(len) -> ["TU"; string_of_float len]
    | TIC_DOWN(len) -> ["TD"; string_of_float len]
    | TIC_LEFT(len) -> ["TL"; string_of_float len]
    | TIC_RIGHT(len) -> ["TR"; string_of_float len]

  let make opt =
    let error reason = invalid_arg ("Pointstyle.Default.make -- "^reason) in
    let args t n m = Printf.sprintf "%s has only %i args (found %i)" t n m
    and unparseable s = Printf.sprintf "Unparseable arg: %s" s
    in
    if opt = [] then NONE
    else let t::opts = opt in
    match t with
      "X" ->
        if List.length opts <> 1 then error (args t 1 (List.length opts));
        (try
           X(float_of_string (List.hd opts))
         with Failure "float_of_string" -> error (unparseable (List.hd opts)))
    | "-" ->
        if List.length opts <> 1 then error (args t 1 (List.length opts));
        (try
           HORIZ(float_of_string (List.hd opts))
         with Failure "float_of_string" -> error (unparseable (List.hd opts)))
    | "|"  ->
        if List.length opts <> 1 then error (args t 1 (List.length opts));
        (try
           VERT(float_of_string (List.hd opts))
         with Failure "float_of_string" -> error (unparseable (List.hd opts)))
    | "TU" ->
        if List.length opts <> 1 then error (args t 1 (List.length opts));
        (try
           TIC_UP(float_of_string (List.hd opts))
         with Failure "float_of_string" -> error (unparseable (List.hd opts)))
    | "TD" ->
        if List.length opts <> 1 then error (args t 1 (List.length opts));
        (try
           TIC_DOWN(float_of_string (List.hd opts))
         with Failure "float_of_string" -> error (unparseable (List.hd opts)))
    | "TL" ->
        if List.length opts <> 1 then error (args t 1 (List.length opts));
        (try
           TIC_LEFT(float_of_string (List.hd opts))
         with Failure "float_of_string" -> error (unparseable (List.hd opts)))
    | "TR" ->
        if List.length opts <> 1 then error (args t 1 (List.length opts));
        (try
           TIC_RIGHT(float_of_string (List.hd opts))
         with Failure "float_of_string" -> error (unparseable (List.hd opts)))
    | _ -> error (t^" is not recognized as a default point style")

  let point t x y handle =
    B.move_to handle x y;
    B.save handle;
    B.set_matrix handle (B.Matrix.identity ());
    (match t with
      NONE -> ()
    | X(len) ->
        let x = len /. 2. in
        B.rel_move_to handle (-.x) (-.x);
        B.rel_line_to handle len len;
        B.rel_move_to handle (-.len) 0.;
        B.rel_line_to handle len (-.len)
    | HORIZ(len) ->
        B.rel_move_to handle (-.len /. 2.) 0.;
        B.rel_line_to handle len 0.
    | VERT(len) ->
        B.rel_move_to handle 0. (-.len /. 2.);
        B.rel_line_to handle 0. len
    | TIC_UP(len) ->
        B.rel_move_to handle 0. (-.len);
        B.rel_line_to handle 0. len
    | TIC_DOWN(len) ->
        B.rel_move_to handle 0. len;
        B.rel_line_to handle 0. (-.len)
    | TIC_LEFT(len) ->
        B.rel_move_to handle (-.len) 0.;
        B.rel_line_to handle len 0.
    | TIC_RIGHT(len) ->
        B.rel_move_to handle len 0.;
        B.rel_line_to handle (-.len) 0.

    );
    B.restore handle
end

type t = float -> float -> B.t -> unit


(*Registering point styles.*)
module M = Map.Make(String)

let registry = ref M.empty

module Register(S:Style) =
struct
  if not(M.mem S.name !registry) then
    let maker options =
      let t = S.make options in
      S.point t
    in
    registry := M.add S.name maker !registry;
end

(*Default is always available*)
let () =
  let module D = Register(Default) in ()

(*Working with axes*)
exception PSError of string

let make options =
  let name, opts = String_utils.first_and_list options in
  try
    let maker = M.find name !registry in
    maker opts
  with Not_found ->
    raise (PSError name)

let make_default def = Default.point def

let point t = t

let make_point options = make options

let points t list handle =
  let rec points list =
    match list with
      [] -> ()
    | (x,y) :: l -> point t x y handle; points l
  in points list
