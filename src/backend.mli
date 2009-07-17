

type slant = Upright | Italic

type weight = Normal | Bold

type text_position =
  | CC  (** centrer horizontally and vertically *)
  | LC  (** align left horizontally and center vertically *)
  | RC  (** align right horizontally and center vertically *)
  | CT  (** center horizontally and align top vertically *)
  | CB
  | LT
  | LB
  | RT
  | RB

val register : string ->
  create:(float -> float -> 'a) ->
  finish:('a -> unit) ->
  set_color:('a -> r:float -> g:float -> b:float -> a:float -> unit) ->
  set_line_width:('a -> float -> unit) ->
  fill:('a -> unit) ->
  stroke:('a -> unit) ->
  line_to:('a -> x:float -> y:float -> unit) ->
  move_to:('a -> x:float -> y:float -> unit) ->
  rectangle:('a -> x:float -> y:float -> width:float -> height:float -> unit) ->
  arc:('a -> x:float -> y:float -> r:float -> a1:float -> a2:float -> unit) ->
  select_font_face:('a -> slant -> weight -> string) ->
  text:('a -> text_position -> string -> float -> float -> unit) ->
  unit

(* Are [save] and [restore] needed?  Is [clip] needed ?  How to
   implement it for graphics ?  Do we want to rotate text ?
*)
