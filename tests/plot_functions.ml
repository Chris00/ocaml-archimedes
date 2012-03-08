open Tests_common
module A = Archimedes

let description = "Various functions y = f(x) and (x,y) = f(t)"

let draw backend =
  let p = A.init ~w ~h ~dirs backend in
  let vp = A.Viewport.grid p 2 2 in

  A.Viewport.set_mark_size vp.(0).(0) 3.;
  A.Axes.box vp.(0).(0);
  A.fx vp.(0).(0) (fun x -> x +. 2.) (-3.) 3. ~n:2;
  A.set_color vp.(0).(0) A.Color.blue;
  A.fx vp.(0).(0) (fun x -> x *. x) (-3.) 3. ~style:(`Linesmarkers "+");

  A.Axes.box vp.(1).(0);
  A.set_color vp.(1).(0) A.Color.red;
  A.fx vp.(1).(0) (fun x -> x *. (x *. x -. 3.)) (-3.) 3. ~fill:true;

  A.set_color vp.(0).(1) (A.Color.rgb 0. 0.5 0.);
  A.xyf vp.(0).(1) (fun t -> (sin t, sin(2. *. t))) 0. 6.5 ~fill:true;
  A.set_color vp.(0).(1) A.Color.red;
  A.xyf vp.(0).(1) (fun t -> (sin t, sin(3. *. t))) 0. 10.
  ~style:(`Linesmarkers "o");

  A.Axes.box vp.(1).(1);
  A.xyf vp.(1).(1) (fun x -> (cos x, sin x)) 0. 6.3 ~n:3;
  A.set_color vp.(1).(1) A.Color.blue;
  A.xyf vp.(1).(1) (fun x -> (cos x, sin x)) 0. 6.3 ~n:10;
  A.set_color vp.(1).(1) A.Color.red;
  A.xyf vp.(1).(1) (fun x -> (cos x, sin x)) 0. 6.3;
  (* Some border cases : *)
  A.fx vp.(1).(1) (fun x -> if -0.5 < x && x < 0.5  then 0. else nan) 0. 1.;
  A.close p
