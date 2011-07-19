include Testing
module P = Plot.Generic

let draw backend =
  (* The directories are only added so it is not necessary to install
     the software to execute this test.  This should be seldom needed. *)
  let p = P.make backend w h ~dirs in
  P.f p (fun x -> x *. sin(1. /. x)) (-3.) 3. ~nsamples:500;
  P.close p

