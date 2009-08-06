module B = Backend

module type Style =
  sig
    val name: string
    type t
    val make: string -> t
    val point: t -> float -> float -> Backend.t -> unit
  end

module Default =
struct
  open String_utils
  let name = "default"
  type style =
      NONE
    | X of float
    | HORIZ of float
    | VERT of float

  type t = style

  let make opt =
    let error () = invalid_arg "Pointstyle.Default.make" in
    let t,opts = first_and_list opt in
    if t = "" then
      if opts = [] then NONE
      else error()
    else
      match t with
        "X" ->
          if List.length opts <> 1 then error ();
          (try
            X(float_of_string (List.hd opts))
          with Failure "float_of_string" -> error ())
      | _ -> error()

  let point t x y handle =
    B.move_to handle x y;
    B.save handle;
    B.set_matrix handle (B.Matrix.identity ());
    (match t with
      NONE -> ()
    | X(len) ->
        B.rel_move_to handle (-.len) (-.len);
        B.rel_line_to handle (2.*.len) (2.*.len);
        B.rel_move_to handle (-.2. *. len) 0.;
        B.rel_line_to handle (2.*.len) (-2.*.len)
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
    maker options
  with Not_found ->
    raise (PSError name)

let point t = t

let make_point options = make options

let points t list handle =
  let rec points list =
    match list with
      [] -> ()
    | (x,y) :: l -> point t x y handle; points l
  in points list
