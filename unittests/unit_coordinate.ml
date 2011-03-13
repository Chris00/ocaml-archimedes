open OUnit
open Archimedes

let points_almost_equal (x1, y1) (x2, y2) =
  (* TODO add debug here *)
  Printf.printf "(%f, %f) =? (%f, %f)\n" x1 y1 x2 y2;
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
	let rotated = Coordinate.make_rotate root ~angle:30. in
	(* coordinates in root (=device), translated, scaled and rotated *)
	let verifs = [
	  ((0., 0.), (42., 42.), (0., 0.), (0., 0.));
	  ((1., 1.), (43., 43.), (2., 3.), (3. ** 0.5 /. 2., 0.5));
	  ((2., 2.), (44., 44.), (4., 6.), (3. ** 0.5, 1.))
	] in
	  List.iter (fun ((x, y), tr, sc, rot) -> begin
	    assert_equal ~cmp:points_almost_equal tr (Coordinate.to_device translated ~x ~y);
	    assert_equal ~cmp:points_almost_equal sc (Coordinate.to_device scaled ~x ~y);
	    assert_equal ~cmp:points_almost_equal rot (Coordinate.to_device rotated ~x ~y)
	  end) verifs

let suite = "Testing coordinates" >::: [
  "test_identity" >:: test_identity;
  "test_operations" >:: test_operations
]

let _ =
  run_test_tt_main suite