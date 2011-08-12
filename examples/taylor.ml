module A = Archimedes

let tsin d =
  (* Compute the Taylor expansion of sin of order 2d+1 *)
  if d < 0 then invalid_arg "tsin";
  let fac = ref 1.
  and s = ref 1. in
  let p = Array.init (d+1) begin fun i ->
    let c = !s /. !fac in
    let i2 = 2. *. float(i+1) in
    fac := !fac *. i2 *. (i2 +. 1.);
    s := -. !s;
    c
  end in
  fun x ->
    (* Horner eval of the poly *)
    let x2 = x *. x
    and y = ref 0. in
    for i = d downto 0 do y := p.(i) +. !y *. x2 done;
    !y *. x

let () =
    let xmax = 9. in
    let f = Array.init 10 tsin in

    let vp = A.init (try A.backend_of_filename Sys.argv.(1) with _ -> [])
      ~w:600. ~h:600. in
    A.Axes.box vp;
    A.Viewport.yrange vp (-3.) 3.;
    A.set_color vp A.Color.blue;
    A.set_line_width vp 1.;
    (* P.yrange p (-3.) 3.; *)
    let len = Array.length f - 1 in
    for i = 0 to len do
      let c = float i /. float len in
      A.set_color vp (A.Color.rgb c 0. (1. -. c));
      A.fx vp f.(i) 0. xmax;
    done;
    A.set_color vp A.Color.green;
    A.set_line_width vp 2.;
    A.fx vp sin 0. xmax;

    A.close vp
