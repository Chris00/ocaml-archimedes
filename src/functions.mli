val samplefxy :
  (float -> float * float) ->
  ?min_step:float ->
  ?nsamples:int -> float -> float -> ('a -> float * float -> 'a) -> 'a -> 'a

val samplefx :
  (float -> float) ->
  ?min_step:float ->
  ?nsamples:int -> float -> float -> ('a -> float -> 'a) -> 'a -> 'a

val fxy_list :
  (float -> float * float) ->
  ?min_step:float -> ?nsamples:int -> float -> float -> (float * float) list

val fx_list :
  (float -> float) ->
  ?min_step:float -> ?nsamples:int -> float -> float -> float list

val plotfxy :
  Backend.t ->
  (float -> float * float) -> ?nsamples:int -> float -> float -> unit

val plotfx :
  Backend.t -> (float -> float) -> ?nsamples:int -> float -> float -> unit

val stroke_plot :
  ?init:bool ->
  Backend.t -> (float -> float) -> ?nsamples:int -> float -> float -> unit

val stroke_plot_param :
  ?init:bool ->
  Backend.t ->
  (float -> float * float) -> ?nsamples:int -> float -> float -> unit

type extend = NONE | PAD | REPEAT | REFLECT

val color_level :
  (float -> float -> float) ->
  ?extend:extend ->
  xmin:float ->
  xmax:float ->
  ymin:float ->
  ymax:float ->
  float -> Color.t -> float -> Color.t -> float -> float -> Color.t
