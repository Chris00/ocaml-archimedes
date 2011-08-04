type t = {r:float; g:float; b:float; a:float}

let in_interval ~max x = x >= 0. && x <= max

let rgba r g b a =
  if in_interval 1. r && in_interval 1. g && in_interval 1. b
    && in_interval 1. a then
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

let hex c =
  let r = (c lsr 16) land 0xFF
  and g = (c lsr 8) land 0xFF
  and b = c land 0xFF in
  rgb (float r /. 255.) (float g /. 255.) (float b /. 255.)

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
let dark_slate_grey = {r = 0.18431372549019609; g = 0.309803921568627461;
                       b = 0.309803921568627461; a = 1.} (* 2F4F4F *)

let aquamarine = {r = 0.439215686274509798; g = 0.858823529411764652;
                  b = 0.576470588235294068; a = 1.} (* 70DB93 *)
let deep_sky_blue = {r = 0.; g = 0.749019607843137258; b = 1.; a = 1.}
let dodger_blue = {r = 0.11764705882352941; g = 0.564705882352941169;
                   b = 1.; a = 1.} (* 1E90FF *)
let light_blue = {r = 0.67843137254901964; g = 0.847058823529411753;
                  b = 0.901960784313725505; a = 1.}
let medium_blue = {r = 0.; g = 0.; b = 0.803921568627451; a = 1.}
let navy_blue = {r = 0.; g = 0.; b = 0.501960784313725483; a = 1.}
let royal_blue = {r = 0.254901960784313708; g = 0.411764705882352922;
                  b = 0.882352941176470562; a = 1.}

let burlywood = {r = 0.870588235294117663; g = 0.721568627450980382;
                 b = 0.529411764705882359; a = 1.}
let chocolate = {r = 0.823529411764705843; g = 0.411764705882352922;
                 b = 0.11764705882352941; a = 1.}
let tan = {r = 0.823529411764705843; g = 0.705882352941176516;
           b = 0.549019607843137303; a = 1.}

let dark_green = {r = 0.18431372549019609; g = 0.309803921568627461;
                  b = 0.18431372549019609; a = 1.}
let dark_olive_green = {r = 0.333333333333333315; g = 0.41960784313725491;
                        b = 0.18431372549019609; a = 1.}
let forest_green = {r = 0.133333333333333331; g = 0.545098039215686225;
                    b = 0.133333333333333331; a = 1.}
let green_yellow = {r = 0.67843137254901964; g = 1.;
                    b = 0.18431372549019609; a = 1.}
let sea_green = {r = 0.180392156862745096; g = 0.545098039215686225;
                 b = 0.341176470588235303; a = 1.}

let dark_orange = {r = 1.; g = 0.549019607843137303; b = 0.; a = 1.}
let peach_puff = {r = 1.; g = 0.854901960784313686;
                  b = 0.725490196078431349; a = 1.}
let coral = {r = 1.; g = 0.498039215686274517; b = 0.; a = 1.}
let orange = {r = 1.; g = 0.647058823529411797; b = 0.; a = 1.}

let hot_pink = {r = 1.; g = 0.411764705882352922;
                b = 0.705882352941176516; a = 1.}
let indian_red = {r = 0.803921568627451; g = 0.360784313725490191;
                  b = 0.360784313725490191; a = 1.}
let light_pink = {r = 1.; g = 0.713725490196078449;
                  b = 0.756862745098039191; a = 1.}
let misty_rose = {r = 1.; g = 0.894117647058823573;
                  b = 0.882352941176470562; a = 1.}
let orange_red = {r = 1.; g = 0.270588235294117629; b = 0.; a = 1.}
let firebrick = {r = 0.698039215686274472; g = 0.133333333333333331;
                 b = 0.133333333333333331; a = 1.}

let dark_orchid = {r = 0.6; g = 0.196078431372549017; b = 0.8; a = 1.}
let lavender_blush = {r = 1.; g = 0.941176470588235281;
                      b = 0.960784313725490224; a = 1.}
let plum = {r = 0.866666666666666696; g = 0.627450980392156854;
            b = 0.866666666666666696; a = 1.}
let orchid = {r = 0.854901960784313686; g = 0.439215686274509798;
              b = 0.83921568627450982; a = 1.}
let purple = {r = 0.627450980392156854; g = 0.125490196078431371;
              b = 0.941176470588235281; a = 1.}
let thistle = {r = 0.847058823529411753; g = 0.749019607843137258;
               b = 0.847058823529411753; a = 1.}

let antique_white = {r = 0.980392156862745057; g = 0.921568627450980338;
                     b = 0.843137254901960786; a = 1.}
let old_lace = {r = 0.992156862745098067; g = 0.960784313725490224;
                b = 0.901960784313725505; a = 1.}
let ivory = {r = 1.; g = 1.; b = 0.941176470588235281; a = 1.}
let linen = {r = 0.980392156862745057; g = 0.941176470588235281;
             b = 0.901960784313725505; a = 1.}
let wheat = {r = 0.960784313725490224; g = 0.870588235294117663;
             b = 0.701960784313725439; a = 1.}
let white_smoke = {r = 0.960784313725490224; g = 0.960784313725490224;
                   b = 0.960784313725490224; a = 1. }

let lemon_chiffon = {r = 1.; g = 0.980392156862745057;
                     b = 0.803921568627451; a = 1.}
let light_goldenrod = {r = 0.933333333333333348; g = 0.866666666666666696;
                       b = 0.509803921568627416; a = 1.}
let cornsilk = {r = 1.; g = 0.972549019607843124;
                b = 0.86274509803921573; a = 1.}
let gold = {r = 1.; g = 0.843137254901960786; b = 0.; a = 1.}

let light_gray = {r = 0.82745098039215681; g = 0.82745098039215681;
                  b = 0.82745098039215681; a = 1. }
let gainsboro = {r = 0.86274509803921573; g = 0.86274509803921573;
                 b = 0.86274509803921573; a = 1. }
let silver = {r = 0.752941176470588225; g = 0.752941176470588225;
              b = 0.752941176470588225; a = 1. }
let trolley_grey = {r = 0.501960784313725483; g = 0.501960784313725483;
                    b = 0.501960784313725483; a = 1. }

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
