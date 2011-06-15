type t = {r:float; g:float; b:float; a:float}

let rgba r g b a =
  let in_interval ?(min=0.) ?(max=1.) x = x >= min && x <= max in
  if in_interval r && in_interval g && in_interval b && in_interval a then
    if in_interval ~max:a r
      && in_interval ~max:a g
      && in_interval ~max:a b then
        {r = r; g = g; b = b; a = a}
    else
      {r = r*.a; g = g*.a; b = b*.a; a = a}
  else
    let msg_intro = "color: some data is not in the range. (Values r,g,b,a :" in
    let data ?(s=", ") x = (string_of_float x)^s in
    let msg = msg_intro^(data r)^(data g)^(data b)^(data ~s:")" a) in
    invalid_arg msg

let rgb r g b = rgba r g b 1.

let r t = t.r

let g t = t.g

let b t = t.b

let a t = t.a

let get_rgb t = t.r, t.g, t.b

let get_rgba t = t.r, t.g, t.b, t.a

let black = {r = 0.; g = 0.; b = 0.; a = 1.}
let red = {r = 1.; g = 0.; b = 0.; a = 1.}
let green = {r = 0.; g = 1.; b = 0.; a = 1.}
let blue = {r = 0.; g = 0.; b = 1.; a = 1.}
let yellow = {r = 1.; g = 1.; b = 0.; a = 1.}
let magenta = {r = 1.; g = 0.; b = 1.; a = 1.}
let cyan = {r = 0.; g = 1.; b = 1.; a = 1.}
let white = {r = 1.; g = 1.; b = 1.; a = 1.}


type operator =
    Over
  | Source
  | Clear
  | In
  | Out
  | Atop
  | Dest
  | Dest_Over
  | Dest_In
  | Dest_Out
  | Dest_Atop
  | Xor
  | Add
  | Saturate

let add ?(op=Over) init newc =
  let alpha, merge =
    match op with
      Over ->
        let conv ?(x=1.) cn ?(y=1.) ci =
          y *. cn  +.  x  *. ci *.(1. -. cn)
        in
        let alphares = conv newc.a init.a in
        let colorres x y = conv ~x newc.a ~y init.a /. alphares in
        alphares, colorres
    | Source ->
        let f _ y = y in
        newc.a, f
    | Clear ->
        let f _ _ = 0. in
        0., f
    | In ->
        let f _ y = y in
        init.a *. newc.a, f
    | Out ->
        let f _ y = y in
        newc.a *. (1. -. init.a), f
    | Atop ->
        let f x y =
          y *. newc.a  +. x *. (1. -. init.a)
        in init.a, f
    | Dest ->
        let f x _ = x
        in init.a, f
    | Dest_Over ->
        let conv ?(x=1.) cn ?(y=1.) ci =
          x *. ci  +.  y  *. cn *.(1. -. ci)
        in
        let alphares = conv newc.a init.a in
        let colorres x y = conv ~x newc.a ~y init.a /. alphares in
        alphares, colorres
    | Dest_In ->
        let f x _ = x in
        init.a *. newc.a, f
    | Dest_Out ->
        let f x _ = x in
        init.a *. (1. -. newc.a), f
    | Dest_Atop ->
        let f x y =
          y *.(1. -. newc.a)  +. x *. init.a
        in newc.a, f
    | Xor ->
        let f ?(x=1.) a ?(y=1.) b =
          let ca,cb = 1. -. a, 1. -. b in
          y *. b *. ca +. x *. a *. cb
        in
        let alphares = f newc.a init.a in
        let colorres x y = f ~x newc.a ~y init.a /. alphares in
        alphares, colorres
    | Add ->
        let alphares = min 1. (init.a +. newc.a) in
        let colorres x y = (x *. init.a  +. y *. newc.a) /. alphares
        in alphares, colorres
    | Saturate ->
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
