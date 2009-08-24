type t

val of_list: (float * float) list -> t

val of_array: (float * float) array -> t

val of_bigarray2: ?clayout:bool -> (float, 'b, 'c)  Bigarray.Array2.t -> t

val of_lists: float list -> float list -> t

val of_arrays: float array -> float array -> t

val of_bigarrays:
  ?xclayout:bool -> (float, 'b, 'c)Bigarray.Array1.t ->
  ?yclayout:bool -> (float, 'b, 'c)Bigarray.Array1.t -> t

val from_sampling :
  (float -> float * float) ->
  ?min_step:float -> ?nsamples:int ->
  float -> float -> t

val next: t -> (float * float) option

val reinit : t -> unit

val nb_data : t -> int

val extents : t -> float * float * float * float
