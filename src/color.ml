type t = {r:float; g:float; b:float; a:float}

let color ?(a=1.) r g b =
  let in_interval ?(min=0.) ?(max=1.) x = x >= min && x <= max in
  if in_interval r && in_interval g && in_interval b && in_interval a then
    if in_interval ~max:a r
      && in_interval ~max:a g
      && in_interval ~max:a b then
        {r = r; g = g; b = b; a = a}
    else
      {r = r/.a; g = g/.a; b = b/.a; a = a}
  else
    let msg_intro = "color: some data is not in the range. (Values r,g,b,a :" in
    let data ?(s=", ") x = (string_of_float x)^s in
    let msg = msg_intro^(data r)^(data g)^(data b)^(data ~s:")" a) in
    invalid_arg msg

let red t = t.r

let green t = t.g

let blue t = t.b

let alpha t = t.a

type operator =
    OVER
  | SOURCE
  | CLEAR
  | IN
  | OUT
  | ATOP
  | DEST
  | DEST_OVER
  | DEST_IN
  | DEST_OUT
  | DEST_ATOP
  | XOR
  | ADD
  | SATURATE

let add ?(op=OVER) init newc =
  let alpha, merge =
    match op with
      OVER ->
        let conv ?(x=1.) cn ?(y=1.) ci =
          y *. cn  +.  x  *. ci *.(1. -. cn)
        in
        let alphares = conv newc.a init.a in
        let colorres x y = conv ~x newc.a ~y init.a /. alphares in
        alphares, colorres
    | SOURCE ->
        let f _ y = y in
        newc.a, f
    | CLEAR ->
        let f _ _ = 0. in
        0., f
    | IN ->
        let f _ y = y in
        init.a *. newc.a, f
    | OUT ->
        let f _ y = y in
        newc.a *. (1. -. init.a), f
    | ATOP ->
        let f x y =
          y *. newc.a  +. x *. (1. -. init.a)
        in init.a, f
    | DEST ->
        let f x _ = x
        in init.a, f
    | DEST_OVER ->
        let conv ?(x=1.) cn ?(y=1.) ci =
          x *. ci  +.  y  *. cn *.(1. -. ci)
        in
        let alphares = conv newc.a init.a in
        let colorres x y = conv ~x newc.a ~y init.a /. alphares in
        alphares, colorres
    | DEST_IN ->
        let f x _ = x in
        init.a *. newc.a, f
    | DEST_OUT ->
        let f x _ = x in
        init.a *. (1. -. newc.a), f
    | DEST_ATOP ->
        let f x y =
          y *.(1. -. newc.a)  +. x *. init.a
        in newc.a, f
    | XOR ->
        let f ?(x=1.) a ?(y=1.) b =
          let ca,cb = 1. -. a, 1. -. b in
          y *. b *. ca +. x *. a *. cb
        in
        let alphares = f newc.a init.a in
        let colorres x y = f ~x newc.a ~y init.a /. alphares in
        alphares, colorres
    | ADD ->
        let alphares = min 1. (init.a +. newc.a) in
        let colorres x y = (x *. init.a  +. y *. newc.a) /. alphares
        in alphares, colorres
    | SATURATE ->
        let alphares = min 1. (init.a +. newc.a) in
        let colorres x y =
          let transp_sat = min newc.a (1. -. init.a) in
          (x *. init.a  +. y *. transp_sat) /. alphares
        in alphares, colorres
  in
  {r = merge init.r newc.r;
   g = merge init.g newc.g;
   b = merge init.b newc.b;
   a = alpha}

(*Local variables:*)
(*compile-command: "ocamlc -c color.ml"*)
(*End:*)
