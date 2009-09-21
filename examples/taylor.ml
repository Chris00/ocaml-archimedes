open Archimedes
module P = Plot.Array

let tsin d =
  (* Compute the Taylor expansion of sin of order 2d+1 *)
  let d = max d 0 in
  let fac = ref 1.
  and s = ref 1. in
  let p = Array.init (d+1)
            (fun i ->
               let c = !s /. !fac in
               let i2 = 2. *. float(i+1) in
               let () =
                 fac := !fac *. i2 *. (i2 +. 1.);
                 s := -. !s in
               c) in
  fun x ->
    (* Horner eval of the poly *)
    let x2 = x *. x
    and y = ref 0. in
    for i = d downto 0 do
      y := p.(i) +. !y *. x2
    done;
    !y *. x

let () =
  let p = P.make "graphics hold" 600. 600. in

  let b = 9. in
  let f = Array.init 10 tsin in
  P.f p sin 0. b;
  P.set_color p Color.blue;
  for i = 0 to Array.length f - 1 do
    P.f p f.(i) 0. b;
  done;

  P.close p
