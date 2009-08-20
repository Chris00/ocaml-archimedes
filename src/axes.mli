exception Not_available
type axes =
    [ `None of bool * bool
    | `Rectangle of bool * bool
    | `Two_lines of float * float ]
val print_axes :
  [> #axes ] ->
  xmin:float -> xmax:float -> ymin:float -> ymax:float -> C.t -> unit
val axes_meeting :
  [> #axes ] ->
  xmin:'a -> xmax:'a -> ymin:'b -> ymax:'b -> 'a * 'b
type tic = [ `P of Pointstyle.name ]
val print_tic : C.t -> [> `P of Pointstyle.name ] -> unit
type label = {
  action : C.t -> unit;
  box : Backend.rectangle;
  rotation : float;
}
val tic_extents : Backend.rectangle -> label option -> Backend.rectangle
type 'a axis1 = {
  major : 'a;
  minor : 'a;
  positions : (float * float * label option) list;
}
type 'a axis = float -> float -> float -> 'a axis1
type ('a, 'b) t = { axes : 'a; x : 'b axis; y : 'b axis; }
val make_axis :
  'a ->
  'a ->
  ('b -> 'c -> 'd -> (float * float * label option) list) ->
  'b -> 'c -> 'd -> 'a axis1
val make : 'a -> 'b axis -> 'b axis -> ('a, 'b) t
val print_tics : 'a axis1 -> (C.t -> 'a -> unit) -> C.t -> unit
val print :
  ([> `None of bool * bool
    | `Rectangle of bool * bool
    | `Two_lines of float * float ]
   as 'a, [> `P of Pointstyle.name ] as 'b)
  t ->
  xmin:float ->
  xmax:float ->
  ymin:float ->
  ymax:float ->
  ?print_axes:('a ->
               xmin:float ->
               xmax:float -> ymin:float -> ymax:float -> C.t -> unit) ->
  ?axes_meeting:('a ->
                 xmin:float ->
                 xmax:float -> ymin:float -> ymax:float -> float * float) ->
  ?print_tic:(C.t -> 'b -> unit) -> C.t -> unit
