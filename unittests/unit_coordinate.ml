open OUnit
open Archimedes

let pi = 4. *. atan 1.

let points_almost_equal (x1, y1) (x2, y2) =
  (*Printf.printf "(%f, %f) ?= (%f, %f)\n" x1 y1 x2 y2;*)
  ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.) ** 0.5 < 1E-8

let test_identity _ =
	let id = Matrix.make_identity () in
	let root = Coordinate.make_root id in
	let coords = [(-1.); 0.; 1.] in
	  List.iter (fun x -> List.iter (fun y -> begin
	    assert_equal (x, y) (Coordinate.to_device root ~x ~y);
	    assert_equal (x, y) (Coordinate.to_coord root ~x ~y)
	  end ) coords) coords

let test_operations _ =
	let id = Matrix.make_identity () in
	let root = Coordinate.make_root id in
	let translated = Coordinate.make_translate root ~x:42. ~y:42. in
	let scaled = Coordinate.make_scale root ~x:2. ~y:3. in
	let rotated = Coordinate.make_rotate root ~angle:(pi /. 4.) in
  let all =
    Coordinate.make_rotate
        (Coordinate.make_scale
            (Coordinate.make_translate
                (Coordinate.make_rotate root ~angle:(-. pi /. 2.))
              ~x:14. ~y:8.)
          ~x:4. ~y:4.)
      ~angle:(pi /. 2.) in
	(* coordinates in root (=device), translated, scaled and rotated *)
	let verifs = [
	  ((0., 0.), (42., 42.), (0., 0.), (0., 0.));
	  ((1., 1.), (43., 43.), (2., 3.), (0., 2. ** 0.5));
	  ((2., 0.), (44., 42.), (4., 0.), (2. ** 0.5, 2. ** 0.5))
	] in
	  List.iter (fun ((x, y), tr, sc, rot) -> begin
	    assert_equal ~cmp:points_almost_equal tr
        (Coordinate.to_device translated ~x ~y);
	    assert_equal ~cmp:points_almost_equal sc
        (Coordinate.to_device scaled ~x ~y);
	    assert_equal ~cmp:points_almost_equal rot
        (Coordinate.to_device rotated ~x ~y)
	  end) verifs;
    assert_equal ~cmp:points_almost_equal (24., -6.)
      (Coordinate.to_device all 4. 2.)

let suite = "Testing coordinates" >::: [
  "test_identity" >:: test_identity;
  "test_operations" >:: test_operations
]

let _ =
  run_test_tt_main suite