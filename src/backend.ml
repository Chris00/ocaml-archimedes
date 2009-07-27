
type 'a t =
    { finish: 'a -> unit;

      set_pointstyle: 'a -> pointstyle -> unit;
      set_pattern: 'a -> pattern -> unit;
      set_color: 'a -> Color.t -> unit;
      set_thickness: 'a -> thickness -> unit;
      set_endstyle: 'a -> endstyle -> unit;
      set_linestyle: 'a -> linestyle -> unit;
      set_anglestyle: 'a -> anglestyle -> unit;

      get_pointstyle: 'a -> pointstyle;
      get_pattern: 'a -> pattern;
      get_thickness: 'a -> thickness;
      get_endstyle: 'a -> endstyle;
      get_linestyle: 'a -> linestyle;
      get_anglestyle: 'a -> anglestyle;

      string_of_error: backend_error -> string;

      move: 'a -> float -> float -> unit;
      get_point: 'a -> float*float;
      line: 'a -> ?x:float -> ?y:float -> float -> float -> unit;
      rel_move: 'a -> float -> float -> unit;
      rel_line: 'a -> float -> float -> unit;
      bezier: 'a -> ?x0:float -> ?y0:float -> cx1:float  -> cy1:float
                 -> ?cx2:float -> ?cy2:float -> x3:float -> y3:float -> unit;
      rel_bezier: 'a -> cx1:float  -> cy1:float
                     -> ?cx2:float -> ?cy2:float -> x3:float -> y3:float -> unit;

      ellipse_arc: 'a -> ?x:float -> ?y:float -> a:float -> b:float
                                              -> float -> float -> unit;
      ellipse: 'a -> ?x:float -> ?y:float -> a:float -> b:float -> unit;
      circle_arc: 'a -> ?x:float -> ?y:float -> r:float
                                             -> float -> float -> unit;
      circle: 'a -> ?x:float -> ?y:float -> r:float -> unit;
      rectangle: 'a -> ?x:float -> ?y:float -> float -> float -> unit;
      rectangle_box: 'a -> rectangle -> unit;

      save: 'a -> unit;
      restore: 'a -> unit;

      translate : 'a -> float -> float -> unit;
      scale : 'a -> float -> float -> unit;
      rotate : 'a -> float -> unit;
      transform : 'a -> float -> float -> float * float;
      transform_dist : 'a -> float -> float -> float * float;
      invert : 'a -> coord;
      inv_transform : 'a -> float -> float -> float * float;
      inv_transform_dist : 'a -> float -> float -> float * float;
      apply : next:coord -> 'a -> unit;
      get_coord :'a -> coord;
      reset_to_id : 'a -> unit;

      close_path: 'a -> unit;
      path_box : 'a -> rectangle;
      clip: ?keeppath:bool -> 'a -> unit;
      fill: ?keeppath:bool -> 'a -> unit;
      draw: ?keeppath:bool -> 'a -> unit;
      select_font_face:('a -> fontslant -> fontweight -> fontname -> unit);

      text: 'a -> ?size:float -> x:float -> y:float -> string -> unit;
      put_image: 'a -> x:float -> y:float -> ?scale:float -> string -> unit;
    }


module M = Map.Make(String)

let registry = ref M.empty

let register ?(force=false) ~create ~finish
    ~set_pointstyle ~set_pattern ~set_color ~set_thickness ~set_endstyle
    ~set_linestyle  ~set_anglestyle
    ~get_pointstyle ~get_pattern ~get_thickness ~get_endstyle
    ~get_linestyle  ~get_anglestyle
    ~string_of_error
    ~move ~get_point ~line ~rel_move ~rel_line
    ~bezier ~rel_bezier ~ellipse_arc ?ellipse ?circle_arc ?circle
    ~rectangle ~rectangle_box
    ~save ~restore
    ~translate ~scale ~rotate ~transform ~transform_dist
    ~invert ~inv_transform ~inv_transform_dist ~apply ~get_coord ~reset_to_id
    ~close_path ~path_box
    ~clip ~fill ~draw
    ~select_font_face ~text
    ~put_image
    name =
  if M.mem name !registry then
    if force then
      Printf.printf
        "A binding for %s was present; il will be overwritten.\n" name
    else
      failwith "Backend.register: Binding is already made"
  else
    (*Default functions*)
    let rec def_ellipse_arc handle z ?x ?y ~a ~b t1 t2 =
      let tolerance =
        match z with
          None -> pi /. 2.
        | Some d ->
            if d <= 0. then invalid_arg "ellipse_arc"
            else d
      in
      let diff = t2 -. t1 in
      let intervals = diff /. tolerance in
      if intervals <= 1. then
        let u,v =
          match x,y with
            Some a, Some b -> a, b
          | _, _ -> get_point handle
        in
        let ct1 = cos t1
        and ct2 = cos t2
        and st1 = sin t1
        and st2 = sin t2 in
        let x0,y0 = u +. a *. ct1, v +. b *. st1
        and x3,y3 = u +. a *. ct2, v +. b *. st2 in
        let cx1,cy1 = x0 -. b *. st1, y0 +. a *. ct1
        and cx2,cy2 = x3 +. b *. st2, y3 -. a *. ct2 in
        bezier handle ?x0:(Some x0) ?y0:(Some y0) ~cx1 ~cy1
          ?cx2:(Some cx2) ?cy2:(Some cy2) ~x3 ~y3
      else
        let num_intervals = (truncate intervals) + 1 in
        let diff_angle = diff /. (float num_intervals) in
        for i = 1 to num_intervals do
          let nt1 = t1 +. (float (i-1)) *. diff_angle
          and nt2 = t1 +. (float i) *. diff_angle in
          def_ellipse_arc handle z ?x ?y ~a ~b nt1 nt2
            (*Note: nt2, for i=num_intervals, is t2*)
        done
    in
    (*Find options*)
    let ellipse_arc = match ellipse_arc with
        Given f -> f
      | Default z ->
          (fun handle ?x ?y ~a ~b t1 t2 ->
             def_ellipse_arc handle z ?x ?y ~a ~b t1 t2)
    in
    let ellipse = match ellipse with
        Some f -> f
      | None -> (fun handle ?x ?y ~a ~b ->
                   ellipse_arc handle ?x ?y ~a ~b  0. (2.*.pi))
    in
    let circle_arc = match circle_arc with
        Some f -> f
      | None -> (fun handle ?x ?y ~r t1 t2 ->
                   ellipse_arc handle ?x ?y ~a:r ~b:r  t1 t2)
    in
    let circle = match circle with
        Some f -> f
      | None -> (fun handle ?x ?y ~r ->
            circle_arc handle ?x ?y ~r 0. (2.*.pi))
    in
    (*Create backend*)
    let creator w h =
    let handle = create w h in
    { finish = (fun () -> finish handle);

      set_pointstyle = (fun () -> set_pointstyle handle);
      set_pattern = (fun () -> set_pattern handle);
      set_color = (fun () -> set_color handle);
      set_thickness = (fun () -> set_thickness handle);
      set_endstyle = (fun () -> set_endstyle handle);
      set_linestyle = (fun () -> set_linestyle handle);
      set_anglestyle = (fun () -> set_anglestyle handle);

      get_pointstyle = (fun () -> get_pointstyle handle);
      get_pattern = (fun () -> get_pattern handle);
      get_thickness = (fun () -> get_thickness handle);
      get_endstyle = (fun () -> get_endstyle handle);
      get_linestyle = (fun () -> get_linestyle handle);
      get_anglestyle = (fun () -> get_anglestyle handle);

      string_of_error = string_of_error;

      move = (fun () -> move handle);
      get_point=(fun () -> get_point handle);
      line = (fun () -> line handle);
      rel_move = (fun () -> rel_move handle);
      rel_line = (fun () -> rel_line handle);
      bezier = (fun () -> bezier handle);
      rel_bezier = (fun () -> rel_bezier handle);
      ellipse_arc = (fun () -> ellipse_arc handle);
      ellipse = (fun () -> ellipse handle);
      circle_arc = (fun () -> circle_arc handle);
      circle = (fun () -> circle handle);
      rectangle = (fun () -> rectangle handle);
      rectangle_box = (fun () -> rectangle_box handle);

      save = (fun () -> save handle);
      restore = (fun () -> restore handle);

      translate = (fun () -> translate handle);
      scale = (fun () -> scale handle);
      rotate = (fun () -> rotate handle);
      transform = (fun () -> transform handle);
      transform_dist = (fun () -> transform_dist handle);
      invert = (fun () -> invert handle);
      inv_transform = (fun () -> inv_transform handle);
      inv_transform_dist = (fun () -> inv_transform_dist handle);
      apply = (fun ~next () -> apply ~next handle);
      get_coord = (fun () -> get_coord handle);
      reset_to_id = (fun () -> reset_to_id handle);


      close_path = (fun () -> close_path handle);
      path_box = (fun () -> path_box handle);
      clip = (fun ?keeppath () -> clip ?keeppath handle);
      fill = (fun ?keeppath () -> fill ?keeppath handle);
      draw = (fun ?keeppath () -> draw ?keeppath handle);

      select_font_face = (fun () -> select_font_face handle);
      text = (fun () -> text handle);
      put_image = (fun () -> put_image handle);
    }
  in
  registry := M.add name creator !registry


exception Non_existent

